from collections import defaultdict
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import sys
import shap
import json
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestRegressor
from sklearn.impute import SimpleImputer
from sklearn.metrics import r2_score
from sklearn.metrics import root_mean_squared_error
from sklearn.model_selection import LeaveOneOut
from sklearn.linear_model import Ridge
from sklearn.linear_model import Lasso
from sklearn.linear_model import LinearRegression
from sklearn.svm import SVR
from sklearn.decomposition import PCA
from sklearn.preprocessing import RobustScaler, PowerTransformer, QuantileTransformer
from tqdm import tqdm
from reg_resampler import resampler
from imblearn.over_sampling import RandomOverSampler
from imblearn.over_sampling import SMOTE
from sklearn.impute import KNNImputer

# outcome_tests = {1: ["IS_fm1_actrhythm"]}
# outcome_tests = {1: ["se36_mean_pt1_sleep"]}

def run_loocv_fold(loocv_id, one_outcome, preproc_method, loocv_model):
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
        def __init__(self, given_out, outcome_names, use_pca, use_corr, use_aug):
            self.out = given_out
            self.other_outs = [out for out in outcome_names if out != given_out]

            self.numeric_imputer = SimpleImputer(strategy='mean')
            # self.pca_transformer = PCA(n_components=30)
            self.pca_transformer = None
            self.loadings = None
            self.scaler = StandardScaler()
            self.power_transformer = None
            self.clo_pca = None
            self.pca_in_cols = None
            self.use_pca = use_pca
            self.use_corr = use_corr
            self.use_aug = use_aug

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
            
        def _select_correlated_features(self, data, outcome, fit=True, q_threshold=0.1, other_threshold=0.1):
            if fit:
                # Calculate correlation with the outcome
                correlation_with_outcome = data.corr()[outcome]

                # Separate features into q features and other features
                q_features = [col for col in data.columns if 'PC' in col or 'Factor' in col]
                # q_features = [col for col in data.columns if 'Factor' in col]
                other_features = [col for col in data.columns if col not in q_features and col != outcome]

                # Apply thresholds
                q_correlated = correlation_with_outcome[q_features][
                    correlation_with_outcome[q_features].abs() >= q_threshold
                ].index.tolist()

                if not q_correlated:
                    q_correlated = correlation_with_outcome[q_features][
                        correlation_with_outcome[q_features].abs() >= 0.1
                    ].index.tolist()

                other_correlated = correlation_with_outcome[other_features][
                    correlation_with_outcome[other_features].abs() >= other_threshold
                ].index.tolist()

                # Combine correlated features, excluding the outcome column itself
                correlated_features = q_correlated + other_correlated

                # Save the correlated features during fit
                self.correlated_features = correlated_features

            assert self.correlated_features is not None, "No correlated features selected"

            # Return data with correlated features and the outcome column
            return data[self.correlated_features + [outcome]]

        def fit_transform(self, data, q_threshold=0.1, other_threshold=0.1):
            # def fit_transform(self, data, correlation_threshold=0.2):
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
            # step_zv
            data = self._remove_zero_variance(data, fit=True)
            data.pop('BiPs_DID')

            ## step_pca
            if self.use_pca:
                data_X = data.filter(regex='_q') #data.drop(columns=self.out)
                data_y = data.drop(data.filter(regex='_q').columns, axis=1) #data[self.out]

                pca = PCA(n_components=40, svd_solver='full')
                pca.fit(data_X)
                cum_variance = np.cumsum(pca.explained_variance_ratio_)
                final_n = (cum_variance >= 0.4).argmax() + 1
                self.pca_transformer = PCA(n_components=final_n, svd_solver='full')

                self.pca_in_cols = data_X.columns.tolist()
                X_pca = self.pca_transformer.fit_transform(data_X)
                self.loadings = pd.DataFrame(self.pca_transformer.components_,
                                             columns=self.pca_transformer.feature_names_in_)
                X_pca_df = pd.DataFrame(X_pca, columns=[f'PC{i+1}' for i in range(X_pca.shape[1])])
                # data = pd.concat([X_pca_df, data_y.reset_index(drop=True)], axis=1)
                data = pd.concat([X_pca_df, data_y.reset_index(drop=True)], axis=1)
                # data.to_csv("after_pca.csv")

            ## step_correlated
            if self.use_corr:
                data = self._select_correlated_features(data, self.out,
                                                fit=True,
                                                q_threshold=q_threshold,
                                                other_threshold=other_threshold)
                # if returning without data augmentation:
                # y = data.pop(self.out)
                # return data, y
            # data_X = data.filter(regex='_q').copy() #data.drop(columns=self.out)
            # data_X[self.out] = data[self.out]
            # data_y = data.drop(data_X.columns, axis=1) #data[self.out]
            # X_corr_df = self._select_correlated_features(data_X, self.out,
            #     fit=True, correlation_threshold=correlation_threshold)
            # data = pd.concat([X_corr_df, data_y], axis=1)
            # data.to_csv("after_corr.csv")

            ## LINES 187-195 data augmentation
            # the use of data augmentation here also functions as an alternative to the log
            # transform that was originally applied to the outcome variable to overcome skewness
            # Initialize the resampler object and generate pseudo-classes
            if self.use_aug:
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

        # def transform(self, data, correlation_threshold=0.2):
        def transform(self, data, q_threshold=0.1, other_threshold=0.1):
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

            # clo_cols = [col for col in data.columns if col.endswith('clo')]
            # data[clo_cols] = self.power_transformer.transform(data[clo_cols])

            data[self.numeric_columns] = self.scaler.transform(data[self.numeric_columns])
            data = self._create_dummies(data, fit=False)
            assert not data.isnull().values.any()
            data = self._remove_zero_variance(data, fit=False)
            data.pop('BiPs_DID')

            # step_pca
            if self.use_pca:
                data_X = data.filter(regex='_q') #data.drop(columns=self.out)
                data_y = data.drop(data.filter(regex='_q').columns, axis=1) #data[self.out]
                X_pca = self.pca_transformer.transform(data_X)
                X_pca_df = pd.DataFrame(X_pca, columns=[f'PC{i+1}' for i in range(X_pca.shape[1])])
                data = pd.concat([X_pca_df, data_y.reset_index(drop=True)], axis=1)

            if self.use_corr:
                data = self._select_correlated_features(data, self.out,
                                                fit=False,
                                                q_threshold=q_threshold,
                                                other_threshold=other_threshold)


            y = data.pop(self.out)
            return data, y

    loo = LeaveOneOut()

    list_shap_values = list()
    list_test_sets = list()
    list_base_values = []
    y_preds = []
    ys = []
    pca_loadings_list = []
    for train_index, test_index in tqdm(list(loo.split(test_binned)), total=len(test_binned)):

        np.random.seed(2025)
        models = {
            'RIDGE': Ridge(alpha=1, random_state=2025),
            'SVR': SVR(C=1, kernel='linear'),
            'RF': RandomForestRegressor(random_state=2025),
            'LR': LinearRegression()
        }

        pipeline = PreprocessingPipeline(
            given_out=one_outcome,
            outcome_names=outcome_names,
            use_pca=use_pca,
            use_corr=use_corr,
            use_aug=use_aug
        )
        # Fit and transform training data
        X_train, y_train = pipeline.fit_transform(test_binned.iloc[train_index, :])
        X_test, y_test = pipeline.transform(test_binned.iloc[test_index, :])
        pca_loadings_list.append(pipeline.loadings)


        for name, model in models.items():
            if name != loocv_model:
                continue
            model.fit(X_train, y_train)
            # correlation = np.corrcoef(y_test.to_numpy(),
            #                         model.predict(X_test))[0, 1]
            # rsq = correlation ** 2 # should follow yardstick's rsq computation
            # rmse = root_mean_squared_error(y_test.to_numpy(), model.predict(X_test))
            if name == 'RIDGE' or name == 'LR':
                explainer = shap.LinearExplainer(model, shap.sample(X_train, 100))#shap.sample(X_train, 100))
            elif name == 'SVR':
                explainer = shap.KernelExplainer(model.predict, shap.sample(X_train, 100))#shap.sample(X_train, 100))
            elif name == 'RF':
                explainer = shap.TreeExplainer(model)
            # explainer = shap.Explainer(model)
            shap_values = explainer.shap_values(X_test).flatten()
            shap_dict = dict(zip(X_test.columns.tolist(), shap_values))

            list_base_values.append(explainer.expected_value)
            list_shap_values.append(shap_dict)
            list_test_sets.append(X_test)
            if name != "RF":
                ys.append(y_test.to_numpy()[0])
                y_preds.append(model.predict(X_test)[0])

    if use_pca:
        padded_loadings_list = []
        all_columns = set()
        for df in pca_loadings_list:
            all_columns.update(df.columns)

        aligned_dfs = []
        for df in pca_loadings_list:
            missing_columns = list(all_columns - set(df.columns))
            missing_data = pd.DataFrame(np.nan, index=df.index, columns=missing_columns)
            aligned_df = pd.concat([df, missing_data], axis=1)
            aligned_df = aligned_df[list(all_columns)]
            aligned_dfs.append(aligned_df)


        max_rows = max(df.shape[0] for df in aligned_dfs)
        for i in range(len(aligned_dfs)):
            if aligned_dfs[i].shape[0] < max_rows:
                pad_rows = max_rows - aligned_dfs[i].shape[0]
                padding = pd.DataFrame(np.nan, index=range(pad_rows), columns=aligned_dfs[i].columns)
                aligned_dfs[i] = pd.concat([aligned_dfs[i], padding], axis=0,ignore_index=True)

        stacked_dfs = np.stack([df.values for df in aligned_dfs])
        mean_df = np.nanmean(stacked_dfs, axis=0)
        final_df = pd.DataFrame(mean_df, columns=list(all_columns))
        final_df = final_df[sorted(list(all_columns))]
        final_df.to_csv("pca_avg_components.csv", index=False)

    if name != "RF":
        correlation = np.corrcoef(np.array(ys),
                                np.array(y_preds))[0, 1]
        rsq = correlation ** 2 # should follow yardstick's rsq computation
        print(f"rsq {rsq}")

    unique_keys = sorted({key for d in list_shap_values for key in d.keys()})
    num_rows = len(list_shap_values)
    num_cols = len(unique_keys)
    # shap_array = np.full((num_rows, num_cols), np.nan)
    shap_array = np.zeros((num_rows, num_cols)) #np.full((num_rows, num_cols), np.nan)

    for i, (shap_dict, base_value) in enumerate(zip(list_shap_values, list_base_values)):
        shap_array[i, :] = base_value
        for j, key in enumerate(unique_keys):
            if key in shap_dict:
                shap_array[i, j] = shap_dict[key]

    test_set_df = pd.concat(list_test_sets).reset_index(drop=True)
    test_set_df = test_set_df[unique_keys]
    test_set_df = test_set_df.fillna(0) # ???


    def analyze_shap_dist(shap_array, test_set_df):
        from scipy import stats
        summaries = []

        for i, feature in enumerate(test_set_df.columns.tolist()):
            feature_shap = shap_array[:, i]
            feature_val = test_set_df.iloc[:, i]
            mask = ~pd.isna(feature_shap) & ~pd.isna(feature_val)
            if len(feature_val[mask]) < 2:
                correlation, p_value = np.nan, 1
                spearman_corr, spearman_pval = np.nan, 1
            else:
                correlation, p_value = stats.pearsonr(feature_val[mask], feature_shap[mask])
                spearman_corr, spearman_pval = stats.spearmanr(feature_val[mask], feature_shap[mask])
            summary = {
                'feature': feature,
                'model': loocv_model,
                'out': one_outcome,
                'mean_abs_impact': np.mean(np.abs(feature_shap)),
                'feat_shap_corr': correlation if p_value < 0.1 else np.nan,
                'feat_shap_spear': spearman_corr if spearman_pval < 0.1 else np.nan,
                'feat_shap_corr_org': correlation
            }
            summaries.append(summary)

        df = pd.DataFrame(summaries)
        df = df.sort_values('mean_abs_impact', ascending=False)
        df['rank'] = range(1, len(df) + 1)
        return df

    # analyze_shap_dist(shap_array, test_set_df)\
    #     .to_csv(f"shap_dist_{one_outcome}_{preproc_method}_{loocv_model}.csv")
    return analyze_shap_dist(shap_array, test_set_df)
    explanation = shap.Explanation(
        values=shap_array,
        base_values=np.array(list_base_values),
        data=test_set_df.values,
        feature_names=test_set_df.columns.tolist(),
    )
    order = np.argsort(y_preds)
    shap.plots.heatmap(explanation,instance_order=order,show=False,max_display=20)
    plt.savefig(f"shap_heatmap_{one_outcome}_{preproc_method}_{loocv_model}.png",
                bbox_inches="tight", dpi=300)
    plt.close()


# IS_fm1: LR/111,RIDGE/111,SVR/111
# se36_mean_pt1: LR/101, RIDGE/101, SVR/101
# tb110_mean_pt1: RF/001

# avg_SOL_min_pt: SVR/010
# avgWASO_min_fm: RF/110
# IS_fm1: LR/111,RIDGE/111,SVR/111
# IV_fm1: LR/100, SVR/111
# SRI_pt1: RF/000, SVR/110
# se36_mean_pt1: LR/101, RIDGE/101, SVR/101
# tb110_mean_pt1: RF/001

if __name__ == '__main__':

    configs = [
        {"out": "SRI_pt1_actrhythm", "model": "RF", "preproc": "000"},
        {"out": "IS_fm1_actrhythm", "model": "LR", "preproc": "111"},
        {"out": "IS_fm1_actrhythm", "model": "RIDGE", "preproc": "111"},
        {"out": "IS_fm1_actrhythm", "model": "SVR", "preproc": "111"},
        {"out": "IV_fm1_actrhythm", "model": "RF", "preproc": "100"},
        {"out": "IV_fm1_actrhythm", "model": "LR", "preproc": "100"},
        {"out": "se36_mean_pt1_sleep", "model": "RIDGE", "preproc": "101"},
        {"out": "se36_mean_pt1_sleep", "model": "LR", "preproc": "101"},
        {"out": "se36_mean_pt1_sleep", "model": "SVR", "preproc": "101"},
        {"out": "tb110_mean_pt1_sleep", "model": "RF", "preproc": "001"},
        {"out": "avgSD_hr_fm_actcomp", "model": "RF", "preproc": "111"},
        {"out": "avgWASO_min_fm_actcomp", "model": "RF", "preproc": "011"}
    ]

    # loocv_id = int(sys.argv[1])
    # outcome_config_id = int(sys.argv[2])

    df_list = []
    for my_config in configs:
        # run_loocv_fold(None, outcome_config_id, sys.argv[3])
        df_list.append(run_loocv_fold(None, my_config['out'], my_config['preproc'], my_config['model']))

    pd.concat(df_list)\
        .to_csv("shap_ranked_dist_all4_na.csv", index=0)
