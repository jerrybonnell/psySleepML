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
from sklearn.ensemble import ExtraTreesRegressor
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.svm import SVR
from sklearn.decomposition import PCA
from sklearn.preprocessing import RobustScaler, PowerTransformer, QuantileTransformer
from tqdm import tqdm
from reg_resampler import resampler
from imblearn.over_sampling import RandomOverSampler
from imblearn.over_sampling import SMOTE
from sklearn.impute import KNNImputer
from scipy.stats import spearmanr
from statsmodels.stats.multitest import multipletests

outcome_tests = {1: ["aaslp7avg_pt1_dailysaliva", "cortslp7avg_pt1_dailysaliva", "dheasslp7avg_pt1_dailysaliva",
                     "aaslp7avg_fm1_dailysaliva", "cortslp7avg_fm1_dailysaliva", "dheasslp7avg_fm1_dailysaliva",
					 "aaslp7sd_pt1_dailysaliva", "cortslp7sd_pt1_dailysaliva", "dheasslp7sd_pt1_dailysaliva",
                     "aaslp7sd_fm1_dailysaliva", "cortslp7sd_fm1_dailysaliva", "dheasslp7sd_fm1_dailysaliva"]}


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

def run_loocv_fold(loocv_id, one_outcome, preproc_method, loocv_model, view="both", alpha=0.10):
    test_binned = pd.read_csv("../integrated/BiPs_PTFM_T1_integrated_saliva_binned_012026.csv")

    cols = [
        c for c in test_binned.columns
        if c in test_binned.filter(
            regex=r'(BiPs_DID)|(_screen)|(_q)|(_co$)|(_cardio)|(gamma_sp1)|(_acutesaliva)|(_dailysaliva)'
        )
    ]

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
        use_pca = bool(int(preproc_method[0]))
        use_corr = bool(int(preproc_method[1]))
        use_aug = bool(int(preproc_method[2]))
    except ValueError:
        raise ValueError("invalid preproc_method string")
    assert use_pca is not None
    assert use_corr is not None
    assert use_aug is not None

    def _y_policy_for_outcome(out_name):
        o = out_name.lower()
        if o.startswith("cortslp7avg") and "_fm1_" in o:
            return "asinh_unscaled"
        if o.startswith("dheasslp7avg") and "_pt1_" in o:
            return "asinh_unscaled"
        return "asinh_scaled"

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
            self.pca_scaler = StandardScaler()
            self.power_transformer = None
            self.clo_pca = None
            self.pca_in_cols = None
            self.q_means_ = None
            self.use_pca = use_pca
            self.use_corr = use_corr
            self.use_aug = use_aug

            self.numeric_columns = None
            self.factor_levels = {}
            self.zero_var_cols = None
            self.correlated_features = None
            self.y_policy = _y_policy_for_outcome(self.out)
            self.y_asinh_scale = None
            self.y_scaler = StandardScaler()

        def _freeze_partner_q(self, data_X_full, view):
            """
            Stage-2 semantics: when producing actor-only view, keep PCA basis from BOTH but
            freeze partner questionnaire dummy columns to their TRAINING means before projection.
            """
            if view == "both" or data_X_full.shape[1] == 0:
                return data_X_full
            if self.q_means_ is None:
                raise ValueError("q_means_ missing; fit_transform must run first")

            if view == "fm":
                partner_pat = "_pt1"
            elif view == "pt":
                partner_pat = "_fm1"
            else:
                raise ValueError(f"unknown view: {view}")

            partner_cols = [c for c in data_X_full.columns if partner_pat in c]
            assert len(partner_cols) > 0
            # partner_cols may be empty if q vars were filtered earlier; just no-op then
            if not partner_cols:
                return data_X_full
            data_X_full = data_X_full.copy()
            data_X_full.loc[:, partner_cols] = self.q_means_.loc[partner_cols].to_numpy(dtype=float)
            return data_X_full


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

        def fit_transform(self, data, correlation_threshold=0.1, view="both"):
            """Fit the pipeline to training data and transform it"""
            data = data.copy()
            data = data.drop(columns=self.other_outs)

            self._get_column_types(data)

            numeric_cols = data[self.numeric_columns]
            self.numeric_imputer.fit(numeric_cols)
            data[self.numeric_columns] = self.numeric_imputer.transform(numeric_cols)

            log_cols = [col for col in data.columns if col.endswith('cardio') or col.endswith('clo') or col.endswith('_co') or col.endswith('acutesaliva')]
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
                q_cols = data.filter(regex='_q').columns.tolist()
                data_X_full = data[q_cols].astype(float)
                data_y = data.drop(columns=q_cols)

                # store training means of questionnaire dummy columns (post-scaling, post-dummy, pre-PCA)
                self.q_means_ = data_X_full.mean(axis=0)

                # Fit PCA basis on BOTH Q space (always)
                pca0 = PCA(n_components=40, svd_solver='full')
                pca0.fit(data_X_full)
                cum_variance = np.cumsum(pca0.explained_variance_ratio_)
                final_n = (cum_variance >= 0.4).argmax() + 1
                self.pca_transformer = PCA(n_components=final_n, svd_solver='full')

                # Fit transformer on BOTH Q space
                X_pca_both = self.pca_transformer.fit_transform(data_X_full)
                X_pca_both = self.pca_scaler.fit_transform(X_pca_both)
                pc_cols = [f'PC{i+1}' for i in range(X_pca_both.shape[1])]
                self.loadings = pd.DataFrame(
                    self.pca_transformer.components_,
                    columns=self.pca_transformer.feature_names_in_
                )

                # Actor-only view: freeze partner-Q then project into the SAME PCA basis
                if view != "both":
                    data_X_actor = self._freeze_partner_q(data_X_full, view=view)
                    X_pca = self.pca_transformer.transform(data_X_actor)
                    X_pca = self.pca_scaler.transform(X_pca)
                else:
                    X_pca = X_pca_both

                X_pca_df = pd.DataFrame(X_pca, columns=pc_cols)
                data = pd.concat([X_pca_df, data_y.reset_index(drop=True)], axis=1)

                # For corr-fit we need BOTH representation regardless of view
                data_both = pd.concat(
                    [pd.DataFrame(X_pca_both, columns=pc_cols), data_y.reset_index(drop=True)],
                    axis=1
                )
                data_for_corr_fit = data_both
            else:
                data_for_corr_fit = data

            ## step_correlated
            if self.use_corr:
                _ = self._select_correlated_features(
                    data_for_corr_fit, self.out, fit=True,
                    correlation_threshold=correlation_threshold
                )
                data = self._select_correlated_features(
                    data, self.out, fit=False,
                    correlation_threshold=correlation_threshold
                )

            ## LINES 187-195 data augmentation
            # the use of data augmentation here also functions as an alternative to the log
            # transform that was originally applied to the outcome variable to overcome skewness
            # Initialize the resampler object and generate pseudo-classes
            if self.use_aug:
                np.random.seed(2025)
                rs = resampler()
                # Keep continuous target for regression
                y_cont = data[self.out].copy()
                # outcome transform policy (fit on train fold)
                y_cont = self._transform_y_fit(y_cont)
                y_cont = pd.Series(
                    self.y_scaler.fit_transform(y_cont.to_frame()).ravel(),
                    index=y_cont.index,
                    name=self.out
                )
                X = data.drop(columns=[self.out])

                # 1) Try resampler with decreasing bins until we get >=2 classes
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
                y = data.pop(self.out)
                # outcome transform policy (fit on train fold)
                y = self._transform_y_fit(y)
                y = self.y_scaler.fit_transform(y.to_frame()).ravel()
                return data, y

        # def transform(self, data, correlation_threshold=0.2):
        def transform(self, data, correlation_threshold=0.1, view="both"):
        # def transform(self, data, clo_threshold=0.0, other_threshold=0.1):
            if self.numeric_columns is None:
                raise ValueError("Pipeline must be fitted before transform")
            data = data.copy()

            if self.other_outs:
                data = data.drop(columns=self.other_outs)
            data[self.numeric_columns] = self.numeric_imputer.transform(data[self.numeric_columns])

            # step log -- all predictor variables, *not* outcomes
            log_cols = [col for col in data.columns if col.endswith('cardio') or col.endswith('clo') or col.endswith('_co') or col.endswith('acutesaliva')]
            for col in log_cols:
                data[col] = np.log1p(data[col])

            data[self.numeric_columns] = self.scaler.transform(data[self.numeric_columns])
            data = self._create_dummies(data, fit=False)
            assert not data.isnull().values.any()
            data = self._remove_zero_variance(data, fit=False)
            data.pop('BiPs_DID')

            # step_pca
            if self.use_pca:
                q_cols = data.filter(regex='_q').columns.tolist()
                data_X_full = data[q_cols].astype(float)
                data_y = data.drop(columns=q_cols)
                # Freeze partner-Q in actor-only view before projecting
                data_X_use = self._freeze_partner_q(data_X_full, view=view)

                X_pca = self.pca_transformer.transform(data_X_use)
                X_pca = self.pca_scaler.transform(X_pca)
                pc_cols = [f'PC{i+1}' for i in range(X_pca.shape[1])]
                X_pca_df = pd.DataFrame(X_pca, columns=pc_cols)
                data = pd.concat([X_pca_df, data_y.reset_index(drop=True)], axis=1)

            if self.use_corr:
                data = self._select_correlated_features(data, self.out,
                                                fit=False,
                                                correlation_threshold=correlation_threshold)

            y = data.pop(self.out)
            # outcome transform policy (apply train-fold params, then scaler)
            y = self._transform_y_apply(y)
            y = self.y_scaler.transform(y.to_frame()).ravel()
            return data, y

    loo = LeaveOneOut()
    # https://lucasramos-34338.medium.com/visualizing-variable-importance-using-shap-and-cross-validation-bd5075e9063a
    list_shap_values = list()
    list_test_sets = list()
    list_base_values = []
    y_preds = []
    ys = []
    pca_loadings_list = []
    for train_index, test_index in tqdm(list(loo.split(test_binned)), total=len(test_binned)):
        np.random.seed(2025)

        models = {
            'RIDGE': Ridge(alpha=1.0, random_state=2025),
            'LR': LinearRegression(),
            'GBR': GradientBoostingRegressor(loss="quantile", alpha=0.9, random_state=2025),
            'RF': RandomForestRegressor(random_state=2025,n_jobs=1),
            'ET': ExtraTreesRegressor(
                random_state=2025,
                n_estimators=100,
                max_depth=10,
                min_samples_split=5,
                n_jobs=1
            )
        }

        pipeline = PreprocessingPipeline(
            given_out=one_outcome,
            outcome_names=outcome_names,
            use_pca=use_pca,
            use_corr=use_corr,
            use_aug=use_aug
        )
        # Fit and transform training data
        # X_train, y_train = pipeline.fit_transform(test_binned.iloc[train_index, :])
        # X_test, y_test = pipeline.transform(test_binned.iloc[test_index, :])
        X_train, y_train = pipeline.fit_transform(test_binned.iloc[train_index, :], view=view)
        X_test, y_test = pipeline.transform(test_binned.iloc[test_index, :], view=view)
        pca_loadings_list.append(pipeline.loadings)
        def restrict_view(X, view):
            if view == "both":
                return X
            X = X.copy()
            drop_pat = "_pt1" if view == "fm" else "_fm1"
            drop_cols = [c for c in X.columns if drop_pat in c]
            return X.drop(columns=drop_cols)

        for name, model in models.items():
            if name != loocv_model:
                continue

            Xtr = restrict_view(X_train, view)
            Xte = restrict_view(X_test, view)
            assert list(Xtr.columns) == list(Xte.columns)

            model.fit(Xtr, y_train)
            if name == 'RIDGE' or name == 'LR':
                explainer = shap.LinearExplainer(model, shap.sample(Xtr, 100))#shap.sample(X_train, 100))
            elif name == 'SVR':
                explainer = shap.KernelExplainer(model.predict, shap.sample(Xtr, 100))#shap.sample(X_train, 100))
            elif name == 'RF' or name == 'GBR' or name == 'ET':
                explainer = shap.TreeExplainer(model)
            shap_values = explainer.shap_values(Xte).flatten()
            shap_dict = dict(zip(Xte.columns.tolist(), shap_values))
 
            # Normalize expected_value to a scalar float (avoids object arrays / broadcasting issues)
            list_base_values.append(float(np.asarray(explainer.expected_value).reshape(-1)[0]))
            list_shap_values.append(shap_dict)
            list_test_sets.append(Xte)
			
            if name != "GBR":
                ys.append(y_test[0])
                y_preds.append(model.predict(Xte)[0])

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

    if loocv_model != "GBR":
        correlation = np.corrcoef(np.array(ys),
                                np.array(y_preds))[0, 1]
        rsq = correlation ** 2 # should follow yardstick's rsq computation
        spearman_corr, _ = spearmanr(np.array(ys), np.array(y_preds))
        print(f"rsq {rsq}")
        print(f"spearman {spearman_corr}")

    unique_keys = sorted({key for d in list_shap_values for key in d.keys()})
    num_rows = len(list_shap_values)
    num_cols = len(unique_keys)

    shap_array = np.zeros((num_rows, num_cols), dtype=float)

    for i, shap_dict in enumerate(list_shap_values):
        for j, key in enumerate(unique_keys):
            if key in shap_dict:
                shap_array[i, j] = float(np.asarray(shap_dict[key]).reshape(-1)[0])

    test_set_df = pd.concat(list_test_sets).reset_index(drop=True)
    # Align to union feature space; missing => absent in that fold
    test_set_df = test_set_df.reindex(columns=unique_keys).fillna(0)


    def analyze_shap_dist_spearman_fdr(shap_array, test_set_df, *,
                                  loocv_model, one_outcome, preproc_method,
                                  alpha=0.10, fdr_method="fdr_bh"):
        """
        Per-feature association gate:
        - Spearman(feature_value, SHAP_value) across LOOCV rows
        - Multiple testing control across features via BH-FDR (default)
        Also computes mean_abs_impact and rank.
        """
        feats = test_set_df.columns.tolist()
        n_feat = len(feats)

        spears = np.full(n_feat, np.nan, dtype=float)
        pvals  = np.full(n_feat, 1.0, dtype=float)
        impacts = np.full(n_feat, np.nan, dtype=float)

        for j, feat in enumerate(feats):
            s = pd.to_numeric(pd.Series(shap_array[:, j]), errors="raise").to_numpy(dtype=float)
            x = pd.to_numeric(test_set_df.iloc[:, j], errors="raise").to_numpy(dtype=float)

            m = np.isfinite(s) & np.isfinite(x)
            impacts[j] = float(np.nanmean(np.abs(s)))

            if m.sum() < 3:
                continue

            rho, p = spearmanr(x[m], s[m])
            spears[j] = float(rho)
            pvals[j]  = float(p)

        # BH-FDR across all features in this (model,out,preproc,view) unit
        # Keep NaN spears as non-sig by setting p=1
        pvals_safe = np.where(np.isfinite(spears), pvals, 1.0)
        rej, qvals, _, _ = multipletests(pvals_safe, alpha=alpha, method=fdr_method)

        df = pd.DataFrame({
            "feature": feats,
            "model": loocv_model,
            "out": one_outcome,
            "preproc": preproc_method,
            "mean_abs_impact": impacts,
            "feat_shap_spear": spears,
            "p_spear": pvals,
            "q_spear": qvals,
            "gate_spear_fdr": rej.astype(bool),
        })

        # rank by mean_abs_impact (descending); rank 1 = largest impact
        df = df.sort_values("mean_abs_impact", ascending=False).reset_index(drop=True)
        df["rank"] = np.arange(1, len(df) + 1)

        # top-half gate (your existing semantics)
        df["top_half"] = df["rank"] <= (len(df) / 2.0)

        # final “colored” gate (Stage-3 ON)
        df["stage3_on"] = df["gate_spear_fdr"] & df["top_half"]
        return df

    def analyze_shap_dist(shap_array, test_set_df):
        from scipy import stats
        summaries = []

        for i, feature in enumerate(test_set_df.columns.tolist()):
            feature_shap = pd.to_numeric(pd.Series(shap_array[:, i]), errors="raise").to_numpy(dtype=float)
            feature_val  = pd.to_numeric(test_set_df.iloc[:, i], errors="raise").to_numpy(dtype=float)
            # mask = ~pd.isna(feature_shap) & ~pd.isna(feature_val)
            mask = np.isfinite(feature_shap) & np.isfinite(feature_val)

            if mask.sum() < 2:
                correlation, p_value = np.nan, 1.0
                spearman_corr, spearman_pval = np.nan, 1.0
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
    df = analyze_shap_dist_spearman_fdr(
        shap_array, test_set_df,
        loocv_model=loocv_model,
        one_outcome=one_outcome,
        preproc_method=preproc_method,
        alpha=alpha
    )
    df["view"] = view
    return df

if __name__ == '__main__':

    configs = [

        {"out": "aaslp7avg_fm1_dailysaliva",    "model": "ET",    "preproc": "011"},
        {"out": "aaslp7avg_fm1_dailysaliva",    "model": "GBR",   "preproc": "110"},
        {"out": "aaslp7avg_fm1_dailysaliva",    "model": "RF",    "preproc": "100"},
        {"out": "aaslp7avg_fm1_dailysaliva",    "model": "LR",    "preproc": "110"},
        {"out": "aaslp7avg_fm1_dailysaliva",    "model": "RIDGE", "preproc": "110"},

        {"out": "aaslp7avg_pt1_dailysaliva",    "model": "ET",    "preproc": "111"},
        {"out": "aaslp7avg_pt1_dailysaliva",    "model": "GBR",   "preproc": "010"},
        {"out": "aaslp7avg_pt1_dailysaliva",    "model": "RF",    "preproc": "110"},
        {"out": "aaslp7avg_pt1_dailysaliva",    "model": "LR",   "preproc": "011"},
        {"out": "aaslp7avg_pt1_dailysaliva",    "model": "RIDGE",   "preproc": "011"},

        {"out": "cortslp7avg_fm1_dailysaliva",  "model": "GBR",    "preproc": "110"},
        {"out": "cortslp7avg_fm1_dailysaliva",  "model": "LR",    "preproc": "010"},
        {"out": "cortslp7avg_fm1_dailysaliva",  "model": "RIDGE",    "preproc": "011"},

        {"out": "cortslp7avg_pt1_dailysaliva",  "model": "ET",    "preproc": "101"},
        {"out": "cortslp7avg_pt1_dailysaliva",  "model": "GBR",   "preproc": "101"},
        {"out": "cortslp7avg_pt1_dailysaliva",  "model": "LR",   "preproc": "010"},
        
        {"out": "dheasslp7avg_fm1_dailysaliva", "model": "GBR",    "preproc": "110"},
        
        {"out": "dheasslp7avg_pt1_dailysaliva", "model": "ET",    "preproc": "110"},
        {"out": "dheasslp7avg_pt1_dailysaliva", "model": "RF",    "preproc": "001"},
        {"out": "dheasslp7avg_pt1_dailysaliva", "model": "LR",    "preproc": "111"},
        {"out": "dheasslp7avg_pt1_dailysaliva", "model": "RIDGE",    "preproc": "110"},

        {"out": "aaslp7sd_fm1_dailysaliva",    "model": "ET",    "preproc": "110"},
        {"out": "aaslp7sd_fm1_dailysaliva",    "model": "GBR",   "preproc": "000"},
        {"out": "aaslp7sd_fm1_dailysaliva",    "model": "LR",    "preproc": "110"},
        {"out": "aaslp7sd_fm1_dailysaliva",    "model": "RF",    "preproc": "110"},
        {"out": "aaslp7sd_fm1_dailysaliva",    "model": "RIDGE", "preproc": "110"},

        {"out": "aaslp7sd_pt1_dailysaliva",    "model": "ET",    "preproc": "010"},
        {"out": "aaslp7sd_pt1_dailysaliva",    "model": "LR",    "preproc": "110"},
        {"out": "aaslp7sd_pt1_dailysaliva",    "model": "RF",    "preproc": "010"},
        {"out": "aaslp7sd_pt1_dailysaliva",    "model": "RIDGE",    "preproc": "110"},

        {"out": "cortslp7sd_fm1_dailysaliva",  "model": "ET",    "preproc": "000"},
        {"out": "cortslp7sd_fm1_dailysaliva",  "model": "GBR",    "preproc": "000"},
        {"out": "cortslp7sd_fm1_dailysaliva",  "model": "LR",    "preproc": "100"},
        {"out": "cortslp7sd_fm1_dailysaliva",  "model": "RF",    "preproc": "010"},
        {"out": "cortslp7sd_fm1_dailysaliva",  "model": "RIDGE",    "preproc": "001"},

        {"out": "cortslp7sd_pt1_dailysaliva",  "model": "ET",    "preproc": "110"},
        {"out": "cortslp7sd_pt1_dailysaliva",  "model": "GBR",    "preproc": "011"},
        {"out": "cortslp7sd_pt1_dailysaliva",  "model": "LR",    "preproc": "100"},
        {"out": "cortslp7sd_pt1_dailysaliva",  "model": "RF",   "preproc": "100"},
        
	    {"out": "dheasslp7sd_fm1_dailysaliva", "model": "ET",    "preproc": "111"},
        {"out": "dheasslp7sd_fm1_dailysaliva", "model": "LR",    "preproc": "000"},
        {"out": "dheasslp7sd_fm1_dailysaliva", "model": "RF",    "preproc": "111"},
        
        {"out": "dheasslp7sd_pt1_dailysaliva", "model": "ET",    "preproc": "000"}
    ]

    def actor_view_for_outcome(out_name):
        o = out_name.lower()
        if "_fm1_" in o:
            return "fm"
        if "_pt1_" in o:
            return "pt"
        raise ValueError(f"cannot infer actor view from outcome: {out_name}")

    df_list = []
    for my_config in configs:
        print(my_config)
        out = my_config["out"]
        model = my_config["model"]
        preproc = my_config["preproc"]

        v_actor = actor_view_for_outcome(out)

        df_both  = run_loocv_fold(None, out, preproc, model, view="both",  alpha=0.10)
        df_actor = run_loocv_fold(None, out, preproc, model, view=v_actor, alpha=0.10)

        key_cols = ["feature", "model", "out", "preproc"]
        keep_cols = key_cols + ["mean_abs_impact", "feat_shap_spear", "p_spear", "q_spear",
                                "gate_spear_fdr", "rank", "top_half", "stage3_on", "view"]

        df_both  = df_both[keep_cols].rename(columns={
            "mean_abs_impact": "mean_abs_impact_both",
            "feat_shap_spear": "spear_both",
            "p_spear": "p_both",
            "q_spear": "q_both",
            "gate_spear_fdr": "gate_both",
            "rank": "rank_both",
            "top_half": "top_half_both",
            "stage3_on": "on_both",
            "view": "view_both"
        })

        df_actor = df_actor[keep_cols].rename(columns={
            "mean_abs_impact": "mean_abs_impact_actor",
            "feat_shap_spear": "spear_actor",
            "p_spear": "p_actor",
            "q_spear": "q_actor",
            "gate_spear_fdr": "gate_actor",
            "rank": "rank_actor",
            "top_half": "top_half_actor",
            "stage3_on": "on_actor",
            "view": "view_actor"
        })

        df_m = df_both.merge(df_actor, on=key_cols, how="outer")

        # DELTA: “ON in BOTH, OFF in ACTOR”
        # NOTE: partner cells won’t exist in ACTOR by construction -> treated as off (False) via fillna(False)
        df_m["on_actor"] = df_m["on_actor"].fillna(False)
        df_m["on_both"]  = df_m["on_both"].fillna(False)
        df_m["on_delta"] = df_m["on_both"] & (~df_m["on_actor"])
        # Direction flip: significant in BOTH views but opposite signs
        df_m["sign_both"] = np.sign(df_m["spear_both"])
        df_m["sign_actor"] = np.sign(df_m["spear_actor"])

        df_m["direction_flip"] = (
            df_m["gate_both"] &
            df_m["gate_actor"] &
            (df_m["sign_both"] != df_m["sign_actor"]) &
            (df_m["sign_both"] != 0) &
            (df_m["sign_actor"] != 0)
        )
        # positive = was negative in actor, became positive in both
        df_m["flip_to_positive"] = df_m["direction_flip"] & (df_m["sign_both"] > 0)
        df_m["flip_to_negative"] = df_m["direction_flip"] & (df_m["sign_both"] < 0)

        # For plotting direction in delta: use BOTH Spearman sign (only meaningful if delta is on)
        df_m["spear_delta"] = np.where(df_m["on_delta"], df_m["spear_both"], np.nan)

        # For ranking in the heatmap: keep BOTH ranking (consistent story)
        df_m["rank"] = df_m["rank_both"]
        df_list.append(df_m)

    pd.concat(df_list).to_csv("shap_stage3_both_actor_delta3.csv", index=False)
