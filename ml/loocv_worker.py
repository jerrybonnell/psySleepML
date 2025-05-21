import pandas as pd
import numpy as np
import sys
import json
import shap
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestRegressor
from sklearn.impute import SimpleImputer
from sklearn.metrics import r2_score
from sklearn.metrics import root_mean_squared_error
from sklearn.model_selection import LeaveOneOut
from sklearn.linear_model import Ridge
from sklearn.linear_model import Lasso
from sklearn.svm import SVR
from sklearn.decomposition import PCA
from sklearn.preprocessing import RobustScaler, PowerTransformer, QuantileTransformer
from tqdm import tqdm
from reg_resampler import resampler
from imblearn.over_sampling import RandomOverSampler
from imblearn.over_sampling import SMOTE
from factor_analyzer import FactorAnalyzer
from sklearn.ensemble import StackingRegressor
from sklearn.linear_model import ElasticNet
from sklearn.linear_model import LinearRegression
from sklearn.linear_model import BayesianRidge
from sklearn.linear_model import HuberRegressor
from sklearn.ensemble import ExtraTreesRegressor
from sklearn.ensemble import BaggingRegressor
from sklearn.tree import DecisionTreeRegressor
from sklearn.metrics import mean_squared_error
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.model_selection import cross_val_score  
from sklearn.model_selection import GridSearchCV

# outcome_tests = {1: ["IS_fm1_actrhythm"]}
# outcome_tests = {1: ["IV_fm1_actrhythm"]}
outcome_tests = {1: ["se36_mean_pt1_sleep"]}
# outcome_tests = {1: ["avgWASO_min_fm_actcomp"]}
# outcome_tests = {1: ["tb110_mean_pt1_sleep"]}
# outcome_tests = {1: ["se36_mean_pt1_sleep"]}
# outcome_tests = {1: ["avgSOL_min_pt_actcomp"]}

"""
todo list
TODO [] need to re-incorporate the correlated features analysis
TODO [] need to play with different settings of PCA and training data augmentation
        (different # components, # bins, different augmentation algorithms)
NOTE two different PCA approaches possible:
     (1) apply to the entire feature set
     (2) apply only to questionnaire features (seems to work better)
TODO [] (!!) for RIDGE model need to play with different penalty thresholds
        (compress until 50% of features remain, 40%, etc.)
TODO [] why doesn't LASSO work well for this data? perhaps it will work better
        with the PCA'd features
TODO [x] need to redo the variable feature importance visualization for python
TODO [] (!!) may need to re-orient the variable feature importance experiment as
        an ablation study to better distill the influence of features derived
        from the different databases

other todo's:
TODO cardio and co databases -- need to understand missingness better
    * how many rows have missing data?
    * how many of the phases from some subject are missing?
    * many not be an across-the-board MNAR or MAR, some rows may be MAR and
      others might be MNAR; the device could have failed for one subject,
      for another subject the reason is they couldn't take it anymore and
      quit the study
"""

def run_loocv_fold(loocv_id, outcome_test_id, preproc_method):
    test_binned = pd.read_csv("../integrated/BiPs_PTFM_T1_integrated_sleep_binned_012325.csv")

    cols = [c for c in list(test_binned.columns) if c in list(test_binned.filter(regex='(BiPs_DID)|(_screen)|(_q)|(_actcomp)|(_actrhythm)|(_sleep)|(_co$)|(_cardio)|(gamma_sp1)'))]

    test_binned = test_binned[cols]

    test_binned[test_binned.filter(regex='_q').columns] = test_binned.filter(regex='_q')\
        .apply(
            lambda col: col.astype('Int64')).apply(lambda col: col.astype('category'))
    test_binned[test_binned.filter(regex='_screen').columns] = test_binned.filter(regex='_screen')\
        .apply(
            lambda col: col.astype('Int64')).apply(lambda col: col.astype('category'))
    outcome_names = test_binned.filter(regex='(_actrhythm|_actcomp|_sleep)').columns.tolist()

    # NOTE drop screener columns
    test_binned = test_binned.drop(test_binned.filter(regex='_screen').columns, axis=1)

    try:
        use_pca = bool(int(preproc_method[0]))
        use_corr = bool(int(preproc_method[1]))
        use_aug = bool(int(preproc_method[2]))
    except ValueError:
        raise ValueError("invalid preproc_method string")
    assert use_pca is not None
    assert use_corr is not None
    assert use_aug is not None
    # print(f"pca {use_pca} corr {use_corr} aug {use_aug}")

    class PreprocessingPipeline:
        """
        A preprocessing pipeline that follows the fit/transform pattern for train/test consistency
        """
        def __init__(self, given_out, outcome_names):
            self.out = given_out
            self.other_outs = [out for out in outcome_names if out != given_out]

            self.numeric_imputer = SimpleImputer(strategy='mean')
            # self.pca_transformer = PCA(n_components=30)
            self.pca_transformer = None
            self.scaler = StandardScaler()
            self.power_transformer = None
            self.clo_pca = None

            self.numeric_columns = None
            self.factor_levels = {}
            self.zero_var_cols = None
            self.correlated_features = None

        def _get_column_types(self, data):
            self.numeric_columns = data.select_dtypes(include=[np.number]).columns.tolist()
            self.factor_columns = data.select_dtypes(include=['category']).columns.tolist()

        def _create_dummies(self, data, fit=True):
            # Create dummy variables
            dummies = pd.get_dummies(data[self.factor_columns],
                                     prefix_sep='_X', dummy_na=True, drop_first=True)

            # Combine with original data
            data = data.drop(columns=self.factor_columns)
            return pd.concat([data, dummies], axis=1)

        def _remove_zero_variance(self, data, fit=True):
            """Remove zero variance predictors"""
            if fit:
                variance = data[[col for col in data.columns if col != "BiPs_DID"]].var()
                self.zero_var_cols = variance[variance == 0].index.tolist()

            if self.zero_var_cols:
                data = data.drop(columns=self.zero_var_cols)
            return data
        
        def _select_correlated_features(self, data, outcome, fit=True, correlation_threshold=0.1):
            """Select features correlated with any outcome above the threshold"""
            if fit:
                
                correlation_with_outcome = data.corr()[outcome]
                correlated_features = correlation_with_outcome[
                    correlation_with_outcome.abs() >= correlation_threshold].index.tolist()

                correlated_features = [col for col in correlated_features if col != outcome]

                self.correlated_features = correlated_features

            assert self.correlated_features is not None, "No correlated features selected"

            # combine selected features data with the outcome variable
            return data[self.correlated_features + [outcome]]

        # def fit_transform(self, data, clo_threshold=0.0, other_threshold=0.1):
        def fit_transform(self, data, correlation_threshold=0.1):
            """Fit the pipeline to training data and transform it"""
            data = data.copy()
            data = data.drop(columns=self.other_outs)

            self._get_column_types(data)

            numeric_cols = data[self.numeric_columns]
            self.numeric_imputer.fit(numeric_cols)
            data[self.numeric_columns] = self.numeric_imputer.transform(numeric_cols)

            log_cols = [col for col in data.columns if col.endswith('cardio') or col.endswith('clo')] #or col == self.out]
            for col in log_cols:
                data[col] = np.log1p(data[col])

            # step_normalize
            data[self.numeric_columns] = self.scaler.fit_transform(data[self.numeric_columns])
            # step_dummy
            data = self._create_dummies(data, fit=True)
            # there should be no missing values beyond this point
            assert not data.isnull().values.any()

            # data.to_csv("after_dummy.csv")

            # step_zv
            data = self._remove_zero_variance(data, fit=True)
            data.pop('BiPs_DID')

            # data.to_csv("after_zv.csv")

            ## step_pca
            if use_pca:
                data_X = data.filter(regex='_q') #data.drop(columns=self.out)
                data_y = data.drop(data.filter(regex='_q').columns, axis=1) #data[self.out]

                pca = PCA(n_components=40, svd_solver='full')
                pca.fit(data_X)
                cum_variance = np.cumsum(pca.explained_variance_ratio_)
                final_n = (cum_variance >= 0.4).argmax() + 1
                self.pca_transformer = PCA(n_components=final_n, svd_solver='full')

                X_pca = self.pca_transformer.fit_transform(data_X)
                X_pca_df = pd.DataFrame(X_pca, columns=[f'PC{i+1}_q' for i in range(X_pca.shape[1])])
                # data = pd.concat([X_pca_df, data_y.reset_index(drop=True)], axis=1)
                data = pd.concat([X_pca_df, data_y.reset_index(drop=True)], axis=1)
              
                data.to_csv("after_pca.csv")

            ## step_correlated
            # data.to_csv("after_corr.csv")
            if use_corr:
                data = self._select_correlated_features(data, self.out,
                                                        fit=True, correlation_threshold=correlation_threshold)
                # data = self._select_correlated_features(data, self.out,
                #                                          fit=True,clo_threshold=clo_threshold, other_threshold=other_threshold)
                # if returning without data augmentation:
                # y = data.pop(self.out)
                # return data, y
                data.to_csv("after_preproc.csv")

            ## LINES 187-195 data augmentation
            # the use of data augmentation here also functions as an alternative to the log
            # transform that was originally applied to the outcome variable to overcome skewness
            # Initialize the resampler object and generate pseudo-classes
            if use_aug:
                np.random.seed(2025)
                rs = resampler()
                y_classes = rs.fit(data, target=self.out, bins = 5,verbose=0)
                unq_classes = np.unique(y_classes)
                X_res, y_res = rs.resample(
                    RandomOverSampler(sampling_strategy={clss_lbl: 300 for clss_lbl in unq_classes},
                                      random_state=2025
                    ),
                    trainX=data,
                    trainY=y_classes
                )
                return X_res, y_res
            else:
                # if returning without data augmentation:
                y = data.pop(self.out)
                return data, y

        def transform(self, data, correlation_threshold=0.1):
        # def transform(self, data, clo_threshold=0.0, other_threshold=0.1):
            if self.numeric_columns is None:
                raise ValueError("Pipeline must be fitted before transform")
            data = data.copy()

            if self.other_outs:
                data = data.drop(columns=self.other_outs)
            data[self.numeric_columns] = self.numeric_imputer.transform(data[self.numeric_columns])

            # step log -- all predictor variables, *not* outcomes
            # log_cols = [col for col in data.columns if col.endswith('cardio') or col == self.out]
            log_cols = [col for col in data.columns if col.endswith('cardio') or col.endswith('clo')]
            for col in log_cols:
                data[col] = np.log1p(data[col])

            data[self.numeric_columns] = self.scaler.transform(data[self.numeric_columns])
            data = self._create_dummies(data, fit=False)
            assert not data.isnull().values.any()
            data = self._remove_zero_variance(data, fit=False)
            data.pop('BiPs_DID')

            # step_pca
            if use_pca:
                data_X = data.filter(regex='_q') #data.drop(columns=self.out)
                data_y = data.drop(data.filter(regex='_q').columns, axis=1) #data[self.out]
                X_pca = self.pca_transformer.transform(data_X)
                X_pca_df = pd.DataFrame(X_pca, columns=[f'PC{i+1}_q' for i in range(X_pca.shape[1])])
                data = pd.concat([X_pca_df, data_y.reset_index(drop=True)], axis=1)

            if use_corr:
                # data = self._select_correlated_features(data, self.out,
                #                                 fit=False,
                #                                 clo_threshold=clo_threshold,
                #                                 other_threshold=other_threshold)
                
                data = self._select_correlated_features(data, self.out,
                        fit=False, correlation_threshold=correlation_threshold)

            y = data.pop(self.out)
            return data, y

    ## REGULAR
    loo = LeaveOneOut()
    train_index, test_index = list(loo.split(test_binned))[loocv_id]

    test_outcomes = None
    if outcome_test_id == 0:
        test_outcomes = outcome_names
    elif outcome_config_id == 1:
        test_outcomes = outcome_tests[outcome_test_id]
    else:
        raise ValueError("unknown id")

    results = []
    for one_outcome in test_outcomes:
        np.random.seed(2025)
        models = {
            'RIDGE': Ridge(alpha=1.0, random_state=2025),
            'SVR': SVR(C=0.4, kernel='linear'),
            'LR': LinearRegression(),
            'RF': RandomForestRegressor(random_state=2025)  
        }

        pipeline = PreprocessingPipeline(
            given_out=one_outcome,
            outcome_names=outcome_names
        )
        
        # Fit and transform training data
        X_train, y_train = pipeline.fit_transform(test_binned.iloc[train_index, :])
        X_test, y_test = pipeline.transform(test_binned.iloc[test_index, :])

        feature_names = X_train.columns.tolist()

        for name, model in models.items():
            model.fit(X_train, y_train)

            # Compute SHAP values
            if name == 'RF':  
                explainer = shap.TreeExplainer(model)
            else:
                explainer = shap.Explainer(model, X_train)

            shap_values = explainer(X_test)
            shap_values_flat = shap_values.values.flatten().tolist()

            # Extract model coefficients
            if name in ['RIDGE', 'BayesRidge', 'LR', 'ENET']:
                weights = model.coef_
            elif name == 'SVR':
                weights = model.coef_[0]
            elif name == 'RF':
                weights = model.feature_importances_

            results.append({
                'loocv_id': loocv_id,
                'out': one_outcome,
                'model': name,
                'pred': model.predict(X_test).tolist()[0],
                'y': y_test.tolist()[0],
                'feat_names': feature_names,
                'abs_weights': np.array(weights).tolist(),
                'shap_values': shap_values_flat,   # Store SHAP values
                'n_components': int(pipeline.pca_transformer.n_components_)
                    if pipeline.pca_transformer is not None else -1,
                'preproc': preproc_method
            })

    print(json.dumps(results))


if __name__ == '__main__':
    loocv_id = int(sys.argv[1])
    outcome_config_id = int(sys.argv[2])
    run_loocv_fold(loocv_id, outcome_config_id, sys.argv[3])
