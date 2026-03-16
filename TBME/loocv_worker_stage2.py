import pandas as pd
import numpy as np
import sys
import json
import shap
import re
from sklearn.preprocessing import StandardScaler, MinMaxScaler
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
# outcome_tests = {1: ["se36_mean_pt1_sleep"]}
# outcome_tests = {1: ["avgWASO_min_fm_actcomp"]}
# outcome_tests = {1: ["tb110_mean_pt1_sleep"]}
# outcome_tests = {1: ["se36_mean_pt1_sleep"]}
# outcome_tests = {1: ["avgSOL_min_pt_actcomp"]}
# outcome_tests = {1: ["aaslp7avg_pt1_dailysaliva", "cortslp7avg_pt1_dailysaliva", "dheasslp7avg_pt1_dailysaliva",
#                      "aaslp7avg_fm1_dailysaliva", "cortslp7avg_fm1_dailysaliva", "dheasslp7avg_fm1_dailysaliva"]}

# outcome_tests = {1: ["aaslp7sd_pt1_dailysaliva", "cortslp7sd_pt1_dailysaliva", "dheasslp7sd_pt1_dailysaliva",
#                      "aaslp7sd_fm1_dailysaliva", "cortslp7sd_fm1_dailysaliva", "dheasslp7sd_fm1_dailysaliva"]}


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

def run_loocv_fold(loocv_id, outcome_test_id, preproc_method, model_name, outcome_name, view):
    test_binned = pd.read_csv("../integrated/BiPs_PTFM_T1_integrated_saliva_binned_012026.csv")

    cols = [c for c in list(test_binned.columns) if c in list(test_binned.filter(regex='(BiPs_DID)|(_screen)|(_q)|(_co$)|(_cardio)|(gamma_sp1)|(_acutesaliva)|(_dailysaliva)'))]

    test_binned = test_binned[cols]

    test_binned[test_binned.filter(regex='_q').columns] = test_binned.filter(regex='_q')\
        .apply(
            lambda col: col.astype('Int64')).apply(lambda col: col.astype('category'))
    test_binned[test_binned.filter(regex='_screen').columns] = test_binned.filter(regex='_screen')\
        .apply(
            lambda col: col.astype('Int64')).apply(lambda col: col.astype('category'))
    outcome_names = test_binned.filter(regex='(_dailysaliva)').columns.tolist()

    # NOTE drop screener columns
    test_binned = test_binned.drop(test_binned.filter(regex='_screen').columns, axis=1)

    try:
        # use_pca = bool(int(preproc_method[0]))
        # use_corr = bool(int(preproc_method[1]))
        # use_aug = bool(int(preproc_method[2]))
        pm = str(preproc_method).strip()
        if len(pm) < 3: raise ValueError("invalid preproc_method string")
        use_pca  = bool(int(pm[0]))
        use_corr = bool(int(pm[1]))
        use_aug  = bool(int(pm[2]))
    except ValueError:
        raise ValueError("invalid preproc_method string")
    assert use_pca is not None
    assert use_corr is not None
    assert use_aug is not None
    # print(f"pca {use_pca} corr {use_corr} aug {use_aug}")

    def _y_policy_for_outcome(out_name):
        o = out_name.lower()
        if o.startswith("cortslp7avg") and "_fm1_" in o:
            return "asinh_unscaled"
        if o.startswith("dheasslp7avg") and "_pt1_" in o:
            return "asinh_unscaled"
        # if o.startswith("aaslp7") or o.startswith("cortslp7"):
        #     return "asinh_scaled"
        return "asinh_scaled"

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
            self.pca_scaler = StandardScaler()
            self.power_transformer = None
            self.clo_pca = None

            self.numeric_columns = None
            self.factor_levels = {}
            self.zero_var_cols = None
            self.correlated_features = None
            self.y_policy = _y_policy_for_outcome(self.out)
            self.y_asinh_scale = None
            self.y_scaler = StandardScaler()

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

        def _freeze_partner_q(self, data_X, view):
            if view == "both" or data_X.shape[1] == 0:
                return data_X
            if not hasattr(self, "q_means_") or self.q_means_ is None:
                raise ValueError('ok')

            if view == "fm":
                partner_pat = "_pt1"
            elif view == "pt":
                partner_pat = "_fm1"
            else:
                raise ValueError(f"unknown view: {view}")

            partner_cols = [c for c in data_X.columns if partner_pat in c]
            assert len(partner_cols) > 0
            if partner_cols:
                data_X = data_X.copy()
                # data_X.loc[:, partner_cols] = self.q_means_.loc[partner_cols].to_numpy()
                data_X.loc[:, partner_cols] = self.q_means_.loc[partner_cols].to_numpy(dtype=float)
            return data_X



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


        def _iqr_over_2_scale(self, y):
            yv = pd.to_numeric(y, errors="raise").to_numpy(dtype=float)
            q25, q75 = np.nanpercentile(yv, [25, 75])
            s = (q75 - q25) / 2.0
            if not np.isfinite(s) or s <= 0:
                s = 1.0
            return float(s)

        def _transform_y_fit(self, y):
            y = pd.to_numeric(y, errors="raise").astype(float)
            if self.y_policy == "scale_only":
                self.y_asinh_scale = None
                return y
            if self.y_policy == "asinh_unscaled":
                self.y_asinh_scale = 1.0
                return np.arcsinh(y)
            # default: asinh_scaled
            self.y_asinh_scale = self._iqr_over_2_scale(y)
            return np.arcsinh(y / self.y_asinh_scale)

        def _transform_y_apply(self, y):
            y = pd.to_numeric(y, errors="raise").astype(float)
            if self.y_policy == "scale_only":
                return y
            if self.y_policy == "asinh_unscaled":
                return np.arcsinh(y)
            # asinh_scaled
            s = getattr(self, "y_asinh_scale")
            return np.arcsinh(y / s)

        # def fit_transform(self, data, clo_threshold=0.0, other_threshold=0.1):
        def fit_transform(self, data, correlation_threshold=0.1, view="both"):
            """Fit the pipeline to training data and transform it"""
            data = data.copy()
            data = data.drop(columns=self.other_outs)

            self._get_column_types(data)

            numeric_cols = data[self.numeric_columns]
            self.numeric_imputer.fit(numeric_cols)
            data[self.numeric_columns] = self.numeric_imputer.transform(numeric_cols)

            log_cols = [col for col in data.columns if col.endswith('cardio')
                        or col.endswith('clo') or col.endswith('_co') or col.endswith("acutesaliva")] #or col == self.out]
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
                # data_X = data.filter(regex='_q') #data.drop(columns=self.out)
                # data_y = data.drop(data.filter(regex='_q').columns, axis=1) #data[self.out]
                q_cols = data.filter(regex='_q').columns.tolist()
                data_X_full = data[q_cols].astype(float)
                data_y = data.drop(columns=q_cols)
                # store training means of questionnaire dummy columns (post-scaling, post-dummy, pre-PCA)
                self.q_means_ = data_X_full.mean(axis=0)

                pca = PCA(n_components=40, svd_solver='full')
                # pca.fit(data_X)
                pca.fit(data_X_full)
                cum_variance = np.cumsum(pca.explained_variance_ratio_)
                final_n = (cum_variance >= 0.4).argmax() + 1
                self.pca_transformer = PCA(n_components=final_n, svd_solver='full')

                # X_pca = self.pca_transformer.fit_transform(data_X)
                # X_pca = self.pca_scaler.fit_transform(X_pca)
                # X_pca_df = pd.DataFrame(X_pca, columns=[f'PC{i+1}_q' for i in range(X_pca.shape[1])])
                # data = pd.concat([X_pca_df, data_y.reset_index(drop=True)], axis=1)
                # data = pd.concat([X_pca_df, data_y.reset_index(drop=True)], axis=1)
                # Fit PCA on BOTH, always
                X_pca_both = self.pca_transformer.fit_transform(data_X_full)
                X_pca_both = self.pca_scaler.fit_transform(X_pca_both)
                pc_cols = [f'PC{i+1}_q' for i in range(X_pca_both.shape[1])]
                X_pca_both_df = pd.DataFrame(X_pca_both, columns=pc_cols)
                data_both = pd.concat([X_pca_both_df, data_y.reset_index(drop=True)], axis=1)
                # If actor-only derived from BOTH, recompute PC scores with partner-Q frozen
                if view != "both":
                    data_X_actor = self._freeze_partner_q(data_X_full, view=view)
                    X_pca_actor = self.pca_transformer.transform(data_X_actor)
                    X_pca_actor = self.pca_scaler.transform(X_pca_actor)
                    X_pca_actor_df = pd.DataFrame(X_pca_actor, columns=pc_cols)
                    data = pd.concat([X_pca_actor_df, data_y.reset_index(drop=True)], axis=1)
                else:
                    data = data_both
                # IMPORTANT: fit correlated-features list on BOTH space (Stage 1 recipe is BOTH)
                data_for_corr_fit = data_both
            else:
                data_for_corr_fit = data
                # data.to_csv("after_pca.csv")

            ## step_correlated
            # data.to_csv("after_corr.csv")
            if use_corr:
                # data = self._select_correlated_features(data, self.out,
                #                                         fit=True, correlation_threshold=correlation_threshold)
                # data = self._select_correlated_features(data, self.out,
                #                                          fit=True,clo_threshold=clo_threshold, other_threshold=other_threshold)
                # Fit selection set on BOTH representation (even if returning actor-only view)
                _ = self._select_correlated_features(
                    data_for_corr_fit, self.out, fit=True,
                    correlation_threshold=correlation_threshold
                )
                data = self._select_correlated_features(
                    data, self.out, fit=False,
                    correlation_threshold=correlation_threshold
                )
                # if returning without data augmentation:
                # y = data.pop(self.out)
                # return data, y
                # data.to_csv("after_preproc.csv")

            ## LINES 187-195 data augmentation
            # the use of data augmentation here also functions as an alternative to the log
            # transform that was originally applied to the outcome variable to overcome skewness
            # Initialize the resampler object and generate pseudo-classes
            if use_aug:
                np.random.seed(2025)
                rs = resampler()
                # y_classes = rs.fit(data, target=self.out, bins = 5,verbose=0)
                # unq_classes = np.unique(y_classes)
                # X_res, y_res = rs.resample(
                #     RandomOverSampler(sampling_strategy={clss_lbl: 300 for clss_lbl in unq_classes},
                #                       random_state=2025
                #     ),
                #     trainX=data,
                #     trainY=y_classes
                # )
                # return X_res, y_res
                # Keep continuous y, but oversample rows using pseudo-classes
                y_cont = data[self.out].copy()
                y_cont = self._transform_y_fit(y_cont)
                y_cont = pd.Series(
                    self.y_scaler.fit_transform(y_cont.to_frame()).ravel(),
                    index=y_cont.index,
                    name=self.out
                )
                X = data.drop(columns=[self.out])

                df_for_rs = pd.concat([X, y_cont], axis=1)
                y_classes = None
                for b in (5, 4, 3, 2):
                    y_tmp = rs.fit(df_for_rs, target=self.out, bins=b, verbose=0)
                    y_tmp = np.asarray(y_tmp)
                    unq, counts = np.unique(y_tmp, return_counts=True)
                    if unq.size >= 2:
                        y_classes = y_tmp
                        break
                # 2) Hard guarantee: if resampler STILL returns 1 class, fall back to median split
                if y_classes is None:
                    y_nm = y_cont.dropna()
                    if y_nm.nunique() < 2:
                        raise ValueError("ok")
                        return X, y_cont
                    med = y_nm.median()
                    y_classes = (y_cont > med).astype(int).to_numpy()
                    unq = np.unique(y_classes)
                    if unq.size < 2:
                        raise ValueError('ok')
                        return X, y_cont
                # 3) Oversample using the pseudo-classes
                unq = np.unique(y_classes)
                sampler = RandomOverSampler(
                    sampling_strategy={int(clss_lbl): 300 for clss_lbl in unq},
                    random_state=2025
                )
                X_res, _ = sampler.fit_resample(X, y_classes)
                idx = sampler.sample_indices_
                y_res = y_cont.iloc[idx].to_numpy()
                return X_res, y_res
            else:
                # if returning without data augmentation:
                # y = data.pop(self.out)
                # return data, y
                y = data.pop(self.out)
                y = self._transform_y_fit(y)
                y = self.y_scaler.fit_transform(y.to_frame()).ravel()
                return data, y

        def transform(self, data, correlation_threshold=0.1,view="both"):
        # def transform(self, data, clo_threshold=0.0, other_threshold=0.1):
            if self.numeric_columns is None:
                raise ValueError("Pipeline must be fitted before transform")
            data = data.copy()

            if self.other_outs:
                data = data.drop(columns=self.other_outs)
            data[self.numeric_columns] = self.numeric_imputer.transform(data[self.numeric_columns])
            # step log -- all predictor variables, *not* outcomes
            # log_cols = [col for col in data.columns if col.endswith('cardio') or col == self.out]
            log_cols = [col for col in data.columns if col.endswith('cardio')
                        or col.endswith('clo') or col.endswith('_co') or col.endswith("acutesaliva")]
            for col in log_cols:
                data[col] = np.log1p(data[col])

            data[self.numeric_columns] = self.scaler.transform(data[self.numeric_columns])
            data = self._create_dummies(data, fit=False)
            assert not data.isnull().values.any()
            data = self._remove_zero_variance(data, fit=False)
            data.pop('BiPs_DID')

            # step_pca
            if use_pca:
                # data_X = data.filter(regex='_q') #data.drop(columns=self.out)
                # data_y = data.drop(data.filter(regex='_q').columns, axis=1) #data[self.out]
                # X_pca = self.pca_transformer.transform(data_X)
                # X_pca = self.pca_scaler.transform(X_pca)
                # X_pca_df = pd.DataFrame(X_pca, columns=[f'PC{i+1}_q' for i in range(X_pca.shape[1])])
                # data = pd.concat([X_pca_df, data_y.reset_index(drop=True)], axis=1)
                q_cols = data.filter(regex='_q').columns.tolist()
                data_X_full = data[q_cols].astype(float)
                data_y = data.drop(columns=q_cols)

                # For actor-only derived-from-BOTH, freeze partner-Q before projecting into PCs
                data_X_use = self._freeze_partner_q(data_X_full, view=view)

                X_pca = self.pca_transformer.transform(data_X_use)
                X_pca = self.pca_scaler.transform(X_pca)
                pc_cols = [f'PC{i+1}_q' for i in range(X_pca.shape[1])]
                X_pca_df = pd.DataFrame(X_pca, columns=pc_cols)
                data = pd.concat([X_pca_df, data_y.reset_index(drop=True)], axis=1)



            if use_corr:
                # data = self._select_correlated_features(data, self.out,
                #                                 fit=False,
                #                                 clo_threshold=clo_threshold,
                #                                 other_threshold=other_threshold)

                data = self._select_correlated_features(data, self.out,
                        fit=False, correlation_threshold=correlation_threshold)

            y = data.pop(self.out)
            # return data, y
            y = self._transform_y_apply(y)
            y = self.y_scaler.transform(y.to_frame()).ravel()
            return data, y

    ## REGULAR
    loo = LeaveOneOut()
    train_index, test_index = list(loo.split(test_binned))[loocv_id]

    # test_outcomes = None
    # if outcome_test_id == 0:
    #     test_outcomes = outcome_names
    # elif outcome_config_id == 1:
    #     test_outcomes = outcome_tests[outcome_test_id]
    # else:
    #     raise ValueError("unknown id")
    # Stage 2: single specified outcome
    test_outcomes = [outcome_name]

    results = []
    for one_outcome in test_outcomes:
        np.random.seed(2025)
        models = {
            'RIDGE': Ridge(alpha=1.0, random_state=2025),
            # 'SVR': SVR(C=0.4, kernel='linear'),
            'GBR': GradientBoostingRegressor(loss="quantile", alpha=0.9, random_state=2025),
            'LR': LinearRegression(),
            'RF': RandomForestRegressor(random_state=2025),
            'ET': ExtraTreesRegressor(
                random_state=2025,
                n_estimators=100,
                max_depth=10,
                min_samples_split=5,
                n_jobs=1
            )
        }
        if model_name not in models:
            raise ValueError(f"unknown model_name: {model_name}")
        models = {model_name: models[model_name]}

        pipeline = PreprocessingPipeline(
            given_out=one_outcome,
            outcome_names=outcome_names
        )

        # Fit and transform training data
        # X_train, y_train = pipeline.fit_transform(test_binned.iloc[train_index, :])
        # X_test, y_test = pipeline.transform(test_binned.iloc[test_index, :])
        def apply_view(df_):
            if view == "both":
                return df_
            df_ = df_.copy()
            keep_always = set(["BiPs_DID"]) | set(outcome_names)
            if view == "fm":
                drop_pat = "_pt1"
            elif view == "pt":
                drop_pat = "_fm1"
            else:
                raise ValueError(f"unknown view: {view}")
            drop_cols = [
                c for c in df_.columns
                if (c not in keep_always) and (drop_pat in c)
            ]
            if drop_cols:
                df_ = df_.drop(columns=drop_cols)
            return df_

        def restrict_view(X, view):
            if view == "both":
                return X

            X = X.copy()

            if view == "fm":
                drop_pat = "_pt1"
            elif view == "pt":
                drop_pat = "_fm1"
            else:
                raise ValueError(f"unknown view: {view}")

            drop_cols = [c for c in X.columns if drop_pat in c]
            return X.drop(columns=drop_cols)

        # train_df = apply_view(test_binned.iloc[train_index, :])
        # test_df  = apply_view(test_binned.iloc[test_index, :])
        train_df = test_binned.iloc[train_index, :]
        test_df  = test_binned.iloc[test_index, :]


        # Fit and transform training data
        X_train, y_train = pipeline.fit_transform(train_df, view=view)
        X_test, y_test = pipeline.transform(test_df, view=view)

        feature_names = X_train.columns.tolist()

        for name, model in models.items():
            Xtr = restrict_view(X_train, view)
            Xte = restrict_view(X_test, view)
            model.fit(Xtr, y_train)

            # Compute SHAP values
            if name == 'RF' or name == "GBR" or name == "ET":
                explainer = shap.TreeExplainer(model)
            else:
                explainer = shap.Explainer(model, Xtr)

            shap_values = explainer(Xte)
            shap_values_flat = shap_values.values.flatten().tolist()

            # Extract model coefficients
            # if name in ['RIDGE', 'BayesRidge', 'LR', 'ENET']:
            #     weights = model.coef_
            # elif name == 'SVR':
            #     weights = model.coef_[0]
            # elif name == 'RF':
            #     weights = model.feature_importances_
            if hasattr(model, "coef_"):
                weights = model.coef_
                if getattr(weights, "ndim", 1) > 1:
                    weights = weights[0]
            elif hasattr(model, "feature_importances_"):
                weights = model.feature_importances_
            else:
                weights = np.zeros(len(feature_names))

            results.append({
                'loocv_id': loocv_id,
                'out': one_outcome,
                'model': name,
                'view': view,
                'pred': model.predict(Xte).tolist()[0],
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
    # run_loocv_fold(loocv_id, outcome_config_id, sys.argv[3])
    preproc_method = sys.argv[3]
    model_name = sys.argv[4]
    outcome_name = sys.argv[5]
    view = sys.argv[6] if len(sys.argv) > 6 else "both"
    run_loocv_fold(loocv_id, outcome_config_id, preproc_method, model_name, outcome_name, view)