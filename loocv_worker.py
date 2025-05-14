# import pandas as pd
# import numpy as np
# import sys
# import json
# from sklearn.preprocessing import StandardScaler
# from sklearn.ensemble import RandomForestRegressor
# from sklearn.impute import SimpleImputer
# from sklearn.metrics import r2_score
# from sklearn.metrics import root_mean_squared_error
# from sklearn.model_selection import LeaveOneOut
# from sklearn.linear_model import Ridge
# from sklearn.linear_model import Lasso
# from sklearn.svm import SVR
# from sklearn.neural_network import MLPRegressor
# from sklearn.decomposition import PCA
# from tqdm import tqdm
# from reg_resampler import resampler
# from imblearn.over_sampling import RandomOverSampler
# from imblearn.over_sampling import SMOTE
# from factor_analyzer import FactorAnalyzer
# from sklearn.ensemble import StackingRegressor
# from sklearn.linear_model import ElasticNet

# from scipy.stats import pearsonr

# outcome_tests = {1: ["IV_pt1_actrhythm", "se36_mean_pt1_sleep"]}
# # outcome_tests = {1: ["avgSOL_min_pt_actcomp"]}

# """
# todo list
# TODO [] need to re-incorporate the correlated features analysis
# TODO [] need to play with different settings of PCA and training data augmentation
#         (different # components, # bins, different augmentation algorithms)
# NOTE two different PCA approaches possible:
#      (1) apply to the entire feature set
#      (2) apply only to questionnaire features (seems to work better)
# TODO [] (!!) for RIDGE model need to play with different penalty thresholds
#         (compress until 50% of features remain, 40%, etc.)
# TODO [] why doesn't LASSO work well for this data? perhaps it will work better
#         with the PCA'd features
# TODO [x] need to redo the variable feature importance visualization for python
# TODO [] (!!) may need to re-orient the variable feature importance experiment as
#         an ablation study to better distill the influence of features derived
#         from the different databases

# other todo's:
# TODO cardio and co databases -- need to understand missingness better
#     * how many rows have missing data?
#     * how many of the phases from some subject are missing?
#     * many not be an across-the-board MNAR or MAR, some rows may be MAR and
#       others might be MNAR; the device could have failed for one subject,
#       for another subject the reason is they couldn't take it anymore and
#       quit the study
# """

# def run_loocv_fold(loocv_id, outcome_test_id, preproc_method):
#     # test_binned = pd.read_csv("../integrated/BiPs_PTFM_T1_integrated_sleep_binned_012325.csv")
#     test_binned = pd.read_csv("../integrated/BiPs_PTFM_T1_integrated_sleep_binned_101424.csv")

#     test_binned[test_binned.filter(regex='_q').columns] = test_binned.filter(regex='_q')\
#         .apply(
#             lambda col: col.astype('Int64')).apply(lambda col: col.astype('category'))
#     test_binned[test_binned.filter(regex='_screen').columns] = test_binned.filter(regex='_screen')\
#         .apply(
#             lambda col: col.astype('Int64')).apply(lambda col: col.astype('category'))
#     outcome_names = test_binned.filter(regex='(_actrhythm|_actcomp|_sleep)').columns.tolist()

#     # NOTE drop screener columns
#     test_binned = test_binned.drop(test_binned.filter(regex='_screen').columns, axis=1)

#     try:
#         use_pca = bool(int(preproc_method[0]))
#         use_corr = bool(int(preproc_method[1]))
#         use_aug = bool(int(preproc_method[2]))
#     except ValueError:
#         raise ValueError("invalid preproc_method string")
#     assert use_pca is not None
#     assert use_corr is not None
#     assert use_aug is not None

#     class PreprocessingPipeline:
#         """
#         A preprocessing pipeline that follows the fit/transform pattern for train/test consistency
#         """
#         def __init__(self, given_out, outcome_names):
#             self.out = given_out
#             self.other_outs = [out for out in outcome_names if out != given_out]

#             self.numeric_imputer = SimpleImputer(strategy='mean')
#             # self.pca_transformer = PCA(n_components=30)
#             self.pca_transformer = None
#             self.scaler = StandardScaler()

#             self.numeric_columns = None
#             self.factor_levels = {}
#             self.zero_var_cols = None
#             self.correlated_features = None

#         def _get_column_types(self, data):
#             self.numeric_columns = data.select_dtypes(include=[np.number]).columns.tolist()
#             self.factor_columns = data.select_dtypes(include=['category']).columns.tolist()

#         def _create_dummies(self, data, fit=True):
#             # Create dummy variables
#             dummies = pd.get_dummies(data[self.factor_columns],
#                                      prefix_sep='_X', dummy_na=True, drop_first=True)

#             # Combine with original data
#             data = data.drop(columns=self.factor_columns)
#             return pd.concat([data, dummies], axis=1)

#         def _remove_zero_variance(self, data, fit=True):
#             """Remove zero variance predictors"""
#             if fit:
#                 variance = data[[col for col in data.columns if col != "BiPs_DID"]].var()
#                 self.zero_var_cols = variance[variance == 0].index.tolist()

#             if self.zero_var_cols:
#                 data = data.drop(columns=self.zero_var_cols)
#             return data

#         # def _select_correlated_features(self, data, outcome, fit=True, correlation_threshold=0.2):
#         #     """Select features correlated with any outcome above the threshold"""
#         #     if fit:
#         #         # determine the selected featues **with respect to the training set only**
#         #         # if this is the testing set, then use whatever features were determined when
#         #         # fitted on the training data
#         #         correlation_with_outcome = data.corr()[outcome]
#         #         correlated_features = correlation_with_outcome[
#         #             correlation_with_outcome.abs() >= correlation_threshold].index.tolist()

#         #         correlated_features = [col for col in correlated_features if col != outcome]

#         #         if not correlated_features:
#         #            correlation_threshold = 0.2
#         #            correlated_features = correlation_with_outcome[
#         #                correlation_with_outcome.abs() >= correlation_threshold].index.tolist()
#         #            correlated_features = [col for col in correlated_features if col != outcome]

#         #         self.correlated_features = correlated_features
#         #         # corr_data = data[correlated_features + [outcome]]

#         #     assert self.correlated_features is not None
#         #     # combine selected features data with the outcome variable
#         #     return data[self.correlated_features + [outcome]]

#         #     # Return the resulting DataFrame (correlated features + outcome)
#         #     # return correlated_data

#         def _select_correlated_features(self, data, outcome, fit=True, q_threshold=0.1, other_threshold=0.1):
#             if fit:
#                 # Calculate correlation with the outcome
#                 correlation_with_outcome = data.corr()[outcome]
                
#                 # Separate features into q features and other features
#                 # q_features = [col for col in data.columns if 'PC' in col]
#                 # q_features = [col for col in data.columns if 'Factor' in col]
#                 q_features = [col for col in data.columns if '_q' in col]
#                 other_features = [col for col in data.columns if col not in q_features and col != outcome]
                
#                 # Apply thresholds
#                 q_correlated = correlation_with_outcome[q_features][
#                     correlation_with_outcome[q_features].abs() >= q_threshold
#                 ].index.tolist()
                
#                 if not q_correlated:
#                     q_correlated = correlation_with_outcome[q_features][
#                         correlation_with_outcome[q_features].abs() >= 0.05
#                     ].index.tolist()
                     
#                 other_correlated = correlation_with_outcome[other_features][
#                     correlation_with_outcome[other_features].abs() >= other_threshold
#                 ].index.tolist()
                
#                 # Combine correlated features, excluding the outcome column itself
#                 correlated_features = q_correlated + other_correlated
                
#                 # Save the correlated features during fit
#                 self.correlated_features = correlated_features
            
#             assert self.correlated_features is not None, "No correlated features selected"
            
#             # Return data with correlated features and the outcome column
#             return data[self.correlated_features + [outcome]]


#         def fit_transform(self, data, q_threshold=0.1, other_threshold=0.1):
#         #def fit_transform(self, data, correlation_threshold=0.2):
#             """Fit the pipeline to training data and transform it"""
#             data = data.copy()
#             data = data.drop(columns=self.other_outs)

#             self._get_column_types(data)

#             numeric_cols = data[self.numeric_columns]
#             self.numeric_imputer.fit(numeric_cols)
#             data[self.numeric_columns] = self.numeric_imputer.transform(numeric_cols)
            
#             # log_cols = [col for col in data.columns if col.endswith('cardio')] #or col == self.out]
#             # log_cols = [col for col in data.columns if col.endswith('cardio') or col.endswith('_clo')]
#             log_cols = [col for col in data.columns if col.endswith('cardio')]
                        
#             for col in log_cols:
#                 data[col] = np.log1p(data[col])
#             # step_normalize
#             data[self.numeric_columns] = self.scaler.fit_transform(data[self.numeric_columns])
#             # step_dummy
#             data = self._create_dummies(data, fit=True)
#             # there should be no missing values beyond this point
#             assert not data.isnull().values.any()
#             # step_zv
#             data = self._remove_zero_variance(data, fit=True)
#             data.pop('BiPs_DID')
            
#             ## step_pca
#             if use_pca:
#                 data_X = data.filter(regex='_q') #data.drop(columns=self.out)
#                 data_y = data.drop(data.filter(regex='_q').columns, axis=1) #data[self.out]
                
#                 pca = PCA(n_components=40, svd_solver='full')
#                 pca.fit(data_X)
#                 cum_variance = np.cumsum(pca.explained_variance_ratio_)
#                 final_n = (cum_variance >= 0.4).argmax() + 1
#                 self.pca_transformer = PCA(n_components=final_n, svd_solver='full')

#                 X_pca = self.pca_transformer.fit_transform(data_X)
#                 X_pca_df = pd.DataFrame(X_pca, columns=[f'PC_q{i+1}' for i in range(X_pca.shape[1])])
#                 data = pd.concat([X_pca_df, data_y.reset_index(drop=True)], axis=1)

#                 # data_X_q = data.filter(regex='_q')
#                 # data_X_clo = data.filter(regex='_clo')

#                 # # Exclude the PCA-transformed columns from the rest of the dataset
#                 # data_y = data.drop(data_X_q.columns, axis=1).drop(data_X_clo.columns, axis=1)

#                 # # PCA for '_q' features
#                 # pca_q = PCA(n_components=40, svd_solver='full')
#                 # pca_q.fit(data_X_q)
#                 # cum_variance_q = np.cumsum(pca_q.explained_variance_ratio_)
#                 # final_n_q = (cum_variance_q >= 0.4).argmax() + 1
#                 # self.pca_transformer_q = PCA(n_components=final_n_q, svd_solver='full')
#                 # X_pca_q = self.pca_transformer_q.fit_transform(data_X_q)
#                 # X_pca_q_df = pd.DataFrame(X_pca_q, columns=[f'PC_q{i+1}' for i in range(X_pca_q.shape[1])])

#                 # # PCA for '_clo' features
#                 # pca_clo = PCA(n_components=20, svd_solver='full')
#                 # pca_clo.fit(data_X_clo)
#                 # cum_variance_clo = np.cumsum(pca_clo.explained_variance_ratio_)
#                 # final_n_clo = (cum_variance_clo >= 0.4).argmax() + 1
#                 # self.pca_transformer_clo = PCA(n_components=final_n_clo, svd_solver='full')
#                 # X_pca_clo = self.pca_transformer_clo.fit_transform(data_X_clo)
#                 # X_pca_clo_df = pd.DataFrame(X_pca_clo, columns=[f'PC_clo{i+1}' for i in range(X_pca_clo.shape[1])])

#                 # # Combine the transformed features with the rest of the data
#                 # data = pd.concat([X_pca_q_df, X_pca_clo_df, data_y.reset_index(drop=True)], axis=1)
#                 # data.to_csv("after_pca.csv")

#                 # # step EFA (check select correlated function if using)
#                 # data_X = data.filter(regex='PC')  # Questionnaire features
#                 # self.fa = FactorAnalyzer(rotation='varimax')  # Orthogonal rotation
#                 # self.fa.fit(data_X)  # Fit the factor model
#                 # eigenvalues, _ = self.fa.get_eigenvalues()  # Get eigenvalues
#                 # n_factors = sum(eigenvalues > 1)  # Factors with eigenvalue > 1
                
#                 # self.fa = FactorAnalyzer(n_factors=n_factors, rotation='varimax')
#                 # self.fa.fit(data_X)
#                 # factor_scores = self.fa.transform(data_X)
#                 # factor_scores_df = pd.DataFrame(factor_scores, columns=[f'Factor_q{i+1}' for i in range(factor_scores.shape[1])])
#                 # data_y = data.drop(data.filter(regex='PC').columns, axis=1)
#                 # data = pd.concat([factor_scores_df, data_y.reset_index(drop=True)], axis=1)
#                 # # data.to_csv("after_EFA.csv")

#             ## step_correlated
#             if use_corr:
#                 data = self._select_correlated_features(data, self.out,
#                                                 fit=True, 
#                                                 q_threshold=q_threshold, 
#                                                 other_threshold=other_threshold)
#             # if returning without data augmentation:
#             # y = data.pop(self.out)
#             # return data, y

#             ## LINES 243-251 data augmentation
#             # the use of data augmentation here also functions as an alternative to the log
#             # transform that was originally applied to the outcome variable to overcome skewness
#             # Initialize the resampler object and generate pseudo-classes
#             if use_aug:
#                 np.random.seed(2025)    
#                 rs = resampler()
#                 y_classes = rs.fit(data, target=self.out, bins = 5,verbose=0)
#                 unq_classes = np.unique(y_classes)
#                 X_res, y_res = rs.resample(
#                     RandomOverSampler(sampling_strategy={clss_lbl: 300 for clss_lbl in unq_classes}),
#                     trainX=data,
#                     trainY=y_classes
#                 )
#                 return X_res, y_res
#             else:
#                 y = data.pop(self.out)
#                 return data, y

#         #def transform(self, data, correlation_threshold=0.2):
#         def transform(self, data, q_threshold=0.1, other_threshold=0.1):    
#             if self.numeric_columns is None:
#                 raise ValueError("Pipeline must be fitted before transform")
#             data = data.copy()

#             if self.other_outs:
#                 data = data.drop(columns=self.other_outs)
#             data[self.numeric_columns] = self.numeric_imputer.transform(data[self.numeric_columns])

#             # step log -- all predictor variables, *not* outcomes
#             # log_cols = [col for col in data.columns if col.endswith('cardio') or col == self.out]
#             # log_cols = [col for col in data.columns if col.endswith('cardio') or col.endswith('_clo')]
#             log_cols = [col for col in data.columns if col.endswith('cardio')]
#             for col in log_cols:
#                 data[col] = np.log1p(data[col])

#             data[self.numeric_columns] = self.scaler.transform(data[self.numeric_columns])
#             data = self._create_dummies(data, fit=False)
#             assert not data.isnull().values.any()
#             data = self._remove_zero_variance(data, fit=False)
#             data.pop('BiPs_DID')
            
#             # step_pca
#             if use_pca:
#                 data_X = data.filter(regex='_q') #data.drop(columns=self.out)
#                 data_y = data.drop(data.filter(regex='_q').columns, axis=1) #data[self.out]
#                 X_pca = self.pca_transformer.transform(data_X)
#                 X_pca_df = pd.DataFrame(X_pca, columns=[f'PC_q{i+1}' for i in range(X_pca.shape[1])])
#                 data = pd.concat([X_pca_df, data_y.reset_index(drop=True)], axis=1)

#                 # data_X_q = data.filter(regex='_q')
#                 # data_X_clo = data.filter(regex='_clo')
#                 # data_y = data.drop(data_X_q.columns, axis=1).drop(data_X_clo.columns, axis=1)

#                 # # PCA for '_q' features
#                 # X_pca_q = self.pca_transformer_q.transform(data_X_q)
#                 # X_pca_q_df = pd.DataFrame(X_pca_q, columns=[f'PC_q{i+1}' for i in range(X_pca_q.shape[1])])

#                 # # PCA for '_clo' features
#                 # X_pca_clo = self.pca_transformer_clo.transform(data_X_clo)
#                 # X_pca_clo_df = pd.DataFrame(X_pca_clo, columns=[f'PC_clo{i+1}' for i in range(X_pca_clo.shape[1])])

#                 # # Combine the transformed features with the rest of the data
#                 # data = pd.concat([X_pca_q_df, X_pca_clo_df, data_y.reset_index(drop=True)], axis=1)

#                 # # Apply fitted EFA model to transform data
#                 # data_X = data.filter(regex='PC')  # Questionnaire features
#                 # factor_scores = self.fa.transform(data_X)
#                 # factor_scores_df = pd.DataFrame(factor_scores, columns=[f'Factor_q{i+1}' for i in range(factor_scores.shape[1])])
#                 # data_y = data.drop(data.filter(regex='PC').columns, axis=1)
#                 # data = pd.concat([factor_scores_df, data_y.reset_index(drop=True)], axis=1)

#             ## step_correlated 286-289
#             if use_corr:
#                 data = self._select_correlated_features(data, self.out,
#                                                 fit=False, 
#                                                 q_threshold=q_threshold, 
#                                                 other_threshold=other_threshold)
            
#             y = data.pop(self.out)
#             return data, y
        
    
#     loo = LeaveOneOut()
#     train_index, test_index = list(loo.split(test_binned))[loocv_id]
#     # bootstrapped = test_binned.sample(n = len(test_binned), replace=True)
#     # oob_ids = set(test_binned['BiPs_DID']) - set(bootstrapped['BiPs_DID'])
#     # oob_sample = test_binned[test_binned['BiPs_DID'].isin(oob_ids)]
#     test_outcomes = None
#     if outcome_test_id == 0:
#         test_outcomes = outcome_names
#     elif outcome_config_id == 1:
#         test_outcomes = outcome_tests[outcome_test_id]
#     else:
#         raise ValueError("unknown id")

#     results = []

#     # Define base models for stacking
#     base_models = [
#         ('RIDGE', Ridge(alpha=1.0, random_state=2025)),
#         ('SVR', SVR(C=0.4, kernel='linear')),
#         ('RF', RandomForestRegressor(random_state=2025))
#     ]

#     # Final model (meta-model) for stacking
#     final_model = ElasticNet(alpha=1.0, l1_ratio=0.5, random_state=2025)

#     # Stacking regressor
#     stacked_model = StackingRegressor(estimators=base_models, final_estimator=final_model)

#     for one_outcome in test_outcomes:
#         np.random.seed(2025)

#         # Create preprocessing pipeline
#         pipeline = PreprocessingPipeline(given_out=one_outcome, outcome_names=outcome_names)
        
#         # Transform data
#         X_train, y_train = pipeline.fit_transform(test_binned.iloc[train_index, :])
#         X_test, y_test = pipeline.transform(test_binned.iloc[test_index, :])

#         # Train and predict for individual models
#         individual_predictions = {}
#         for name, model in base_models:
#             model.fit(X_train, y_train)
#             pred = model.predict(X_test).tolist()[0]
#             individual_predictions[name] = pred

#             # Collect weights (if applicable)
#             if name == 'RIDGE':
#                 weights = model.coef_
#             elif name == 'SVR':
#                 weights = model.coef_[0] if hasattr(model, 'coef_') else None
#             elif name == 'RF':
#                 weights = model.feature_importances_

#             results.append({
#                 'loocv_id': loocv_id,
#                 'out': one_outcome,
#                 'model': name,
#                 'pred': pred,
#                 'y': y_test.tolist()[0],
#                 'feat_names': X_train.columns.tolist(),
#                 'abs_weights': np.abs(weights).tolist() if weights is not None else None,
#                 'n_components': int(pipeline.pca_transformer.n_components_)
#                 if pipeline.pca_transformer is not None else -1,
#                 'preproc': preproc_method
#             })
        
#         # Stack the predictions from the individual models
#         stacked_train_features = np.array([model.predict(X_train) for _, model in base_models]).T
#         stacked_test_features = np.array(list(individual_predictions.values())).reshape(1, -1)

#         # Train the stacked model
#         stacked_model.fit(stacked_train_features, y_train)

#         # Predict using the stacked model
#         stacked_pred = stacked_model.predict(stacked_test_features).tolist()[0]

#         # Append the stacked model results
#         results.append({
#             'loocv_id': loocv_id,
#             'out': one_outcome,
#             'model': 'stack_elastic',
#             'pred': stacked_pred,
#             'y': y_test.tolist()[0],
#             'feat_names': list(individual_predictions.keys()),  # Names of base models
#             'abs_weights': stacked_model.final_estimator_.coef_.tolist(),  # Stacked model weights
#             'n_components': -1,
#             'preproc': preproc_method
#         })

#     # Print the results as JSON
#     print(json.dumps(results))

# if __name__ == '__main__':
#     loocv_id = int(sys.argv[1])
#     outcome_config_id = int(sys.argv[2])
#     run_loocv_fold(loocv_id, outcome_config_id, sys.argv[3])
# library(reticulate)
# use_python("C:/Users/nikhi/anaconda3/envs/myenv/python.exe", required = TRUE)
# use_python("/home/nikhi/miniconda3/envs/myenv", required = TRUE)

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
    # cols = [c for c in list(test_binned.columns) if c not in list(test_binned.filter(regex='_clo').columns[12:])]
    # cols = [c for c in list(test_binned.columns) if not c.startswith("eta_")]
    # cols = [c for c in list(test_binned.columns) if c in list(test_binned.filter(regex='(BiPs_DID)|(_screen)|(_q)|(_actcomp)|(_actrhythm)|(_sleep)|(_co$)|(_cardio)|(gamma_sp2_pt1_clo)|(gamma_sp2_fm1_clo)'))]
    # cols = [c for c in list(test_binned.columns) if c in list(test_binned.filter(regex='(BiPs_DID)|(_screen)|(_q)|(_actcomp)|(_actrhythm)|(_sleep)|(_co$)|(_cardio)|(gamma_)'))]
    # cols = [c for c in list(test_binned.columns) if c in list(test_binned.filter(regex='(BiPs_DID)|(_screen)|(_q)|(_actcomp)|(_actrhythm)|(_sleep)|(_co$)|(_cardio)|(_clo$)'))]

    # cols = [c for c in list(test_binned.columns) if c in list(test_binned.filter(regex='(BiPs_DID)|(_screen)|(_q)|(_actcomp)|(_actrhythm)|(_sleep)|(_co$)|(_cardio)|(gamma_sp1)'))]

    # cols = [c for c in list(test_binned.columns) if c in list(test_binned.filter(regex='(BiPs_DID)|(_screen)|(_q)|(_actcomp)|(_actrhythm)|(_sleep)|(coreg_b)|(coag_b_fm)|(coreg_s_)|(coag_s_)|(_cardio)|(gamma_sp1)'))]

    cols = [c for c in list(test_binned.columns) if c in list(test_binned.filter(regex='(BiPs_DID)|(_screen)|(_q)|(_actcomp)|(_actrhythm)|(_sleep)|(coag_b_fm)|(coag_p_fm)|(coag_sp1_fm)|(coag_r_fm)|(_cardio)|(gamma_sp1)'))]
    
    # cols = [
    #     c for c in list(test_binned.columns)
    #     if c in list(test_binned.filter(regex='(BiPs_DID)|(_screen)|(_q)|(_actcomp)|(_actrhythm)|(_sleep)|(_co$)|(_cardio)|(gamma_)'))
    #     and not c.startswith('gamma_b')
    # ]

    test_binned = test_binned[cols]
    # test_binned = pd.read_csv("../integrated/BiPs_PTFM_T1_integrated_sleep_binned_101424.csv")

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

                # if not correlated_features:
                #    correlation_threshold = 0.1
                #    correlated_features = correlation_with_outcome[
                #        correlation_with_outcome.abs() >= correlation_threshold].index.tolist()
                #    correlated_features = [col for col in correlated_features if col != outcome]

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


                # # # data.to_csv("after_pca.csv")

                # data_X_q = data.filter(regex='_q')
                # data_X_clo = data.filter(regex='_clo')

                # # Exclude the PCA-transformed columns from the rest of the dataset
                # data_y = data.drop(data_X_q.columns, axis=1).drop(data_X_clo.columns, axis=1)

                # # PCA for '_q' features
                # pca_q = PCA(n_components=40, svd_solver='full')
                # pca_q.fit(data_X_q)
                # cum_variance_q = np.cumsum(pca_q.explained_variance_ratio_)
                # final_n_q = (cum_variance_q >= 0.4).argmax() + 1
                # self.pca_transformer_q = PCA(n_components=final_n_q, svd_solver='full')
                # X_pca_q = self.pca_transformer_q.fit_transform(data_X_q)
                # X_pca_q_df = pd.DataFrame(X_pca_q, columns=[f'PC_q{i+1}' for i in range(X_pca_q.shape[1])])

                # # PCA for '_clo' features
                # pca_clo = PCA(n_components=10, svd_solver='full')
                # pca_clo.fit(data_X_clo)
                # cum_variance_clo = np.cumsum(pca_clo.explained_variance_ratio_)
                # final_n_clo = (cum_variance_clo >= 0.5).argmax() + 1
                # self.pca_transformer_clo = PCA(n_components=final_n_clo, svd_solver='full')
                # X_pca_clo = self.pca_transformer_clo.fit_transform(data_X_clo)
                # X_pca_clo_df = pd.DataFrame(X_pca_clo, columns=[f'PC_clo{i+1}' for i in range(X_pca_clo.shape[1])])

                # # Combine the transformed features with the rest of the data
                # data = pd.concat([X_pca_q_df, X_pca_clo_df, data_y.reset_index(drop=True)], axis=1)
                data.to_csv("after_pca.csv")


                # data_X_pt = data.filter(regex='_pt.*_q')

                # # Select all _q features that are not _pt features (for fm)
                # data_X_fm = data.filter(regex='_q').drop(columns=data_X_pt.columns)

                # # Separate the rest of the data
                # data_y = data.drop(data_X_pt.columns, axis=1).drop(data_X_fm.columns, axis=1)

                # # PCA for '_pt_q' features
                # pca_pt = PCA(n_components=40, svd_solver='full')
                # pca_pt.fit(data_X_pt)
                # cum_variance_pt = np.cumsum(pca_pt.explained_variance_ratio_)
                # final_n_pt = (cum_variance_pt >= 0.2).argmax() + 1
                # self.pca_transformer_pt = PCA(n_components=final_n_pt, svd_solver='full')
                # X_pca_pt = self.pca_transformer_pt.fit_transform(data_X_pt)
                # X_pca_pt_df = pd.DataFrame(X_pca_pt, columns=[f'PC_pt1_q{i+1}' for i in range(X_pca_pt.shape[1])])

                # # PCA for '_fm_q' features
                # pca_fm = PCA(n_components=40, svd_solver='full')
                # pca_fm.fit(data_X_fm)
                # cum_variance_fm = np.cumsum(pca_fm.explained_variance_ratio_)
                # final_n_fm = (cum_variance_fm >= 0.2).argmax() + 1
                # self.pca_transformer_fm = PCA(n_components=final_n_fm, svd_solver='full')
                # X_pca_fm = self.pca_transformer_fm.fit_transform(data_X_fm)
                # X_pca_fm_df = pd.DataFrame(X_pca_fm, columns=[f'PC_fm1_q{i+1}' for i in range(X_pca_fm.shape[1])])

                # # Combine the transformed features with the rest of the data
                # data = pd.concat([X_pca_pt_df, X_pca_fm_df, data_y.reset_index(drop=True)], axis=1)

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

                # data_X_q = data.filter(regex='_q')
                # data_X_clo = data.filter(regex='_clo')
                # data_y = data.drop(data_X_q.columns, axis=1).drop(data_X_clo.columns, axis=1)

                # # PCA for '_q' features
                # X_pca_q = self.pca_transformer_q.transform(data_X_q)
                # X_pca_q_df = pd.DataFrame(X_pca_q, columns=[f'PC_q{i+1}' for i in range(X_pca_q.shape[1])])

                # # PCA for '_clo' features
                # X_pca_clo = self.pca_transformer_clo.transform(data_X_clo)
                # X_pca_clo_df = pd.DataFrame(X_pca_clo, columns=[f'PC_clo{i+1}' for i in range(X_pca_clo.shape[1])])

                # # Combine the transformed features with the rest of the data
                # data = pd.concat([X_pca_q_df, X_pca_clo_df, data_y.reset_index(drop=True)], axis=1)

                # Select _pt1_q (or _pt_q) features
                # data_X_pt = data.filter(regex='_pt.*_q')
                # data_X_fm = data.filter(regex='_q').drop(columns=data_X_pt.columns)
                # data_y = data.drop(data_X_pt.columns, axis=1).drop(data_X_fm.columns, axis=1)

                # # Transform for '_pt_q' features
                # X_pca_pt = self.pca_transformer_pt.transform(data_X_pt)
                # X_pca_pt_df = pd.DataFrame(X_pca_pt, columns=[f'PC_pt1_q{i+1}' for i in range(X_pca_pt.shape[1])])

                # # Transform for '_fm_q' features
                # X_pca_fm = self.pca_transformer_fm.transform(data_X_fm)
                # X_pca_fm_df = pd.DataFrame(X_pca_fm, columns=[f'PC_fm1_q{i+1}' for i in range(X_pca_fm.shape[1])])

                # # Combine the transformed features with the rest of the data
                # data = pd.concat([X_pca_pt_df, X_pca_fm_df, data_y.reset_index(drop=True)], axis=1)

            # step_EFA
            # data_X = data.filter(regex='PC')
            # factor_scores = self.fa.transform(data_X)
            # factor_scores_df = pd.DataFrame(factor_scores, columns=[f'Factor{i+1}' for i in range(factor_scores.shape[1])])
            # data_y = data.drop(data.filter(regex='PC').columns, axis=1)
            # data = pd.concat([factor_scores_df, data_y.reset_index(drop=True)], axis=1)

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
            # 'BayesRidge': BayesianRidge(),
            # 'LASSO': Lasso(alpha=0.005, random_state=2025, max_iter=10000), #0.005 causes errors
            # 'ENET': ElasticNet(alpha=0.005, l1_ratio=0.2, random_state=2025, max_iter=10000)
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

    # # STACKING
    # loo = LeaveOneOut()
    # train_index, test_index = list(loo.split(test_binned))[loocv_id]
    # # bootstrapped = test_binned.sample(n = len(test_binned), replace=True)
    # # oob_ids = set(test_binned['BiPs_DID']) - set(bootstrapped['BiPs_DID'])
    # # oob_sample = test_binned[test_binned['BiPs_DID'].isin(oob_ids)]
    # test_outcomes = None
    # if outcome_test_id == 0:
    #     test_outcomes = outcome_names
    # elif outcome_config_id == 1:
    #     test_outcomes = outcome_tests[outcome_test_id]
    # else:
    #     raise ValueError("unknown id")

    # results = []

    # # Define base models for stacking
    # base_models = [
    #     ('RIDGE', Ridge(alpha=1.0, random_state=2025)),
    #     ('SVR', SVR(C=0.4, kernel='linear')),
    #     ('LASSO', Lasso(alpha=0.005, random_state=2025, max_iter=10000))
    #     # ('Bayesian', BayesianRidge())
    #     # ('RF', RandomForestRegressor(random_state=2025)),
    #     # ('XGB', XGBRegressor(n_estimators=100, learning_rate=0.1, max_depth=3, random_state=2025))
    # ]

    # # Final model (meta-model) for stacking
    # final_model = Ridge(alpha=1.0)
    # # final_model = ElasticNet(alpha=1.0, l1_ratio=0.5) 
    # # final_model = LinearRegression()
    # # final_model = BayesianRidge()

    # # Stacking regressor
    # stacked_model = StackingRegressor(estimators=base_models, final_estimator=final_model)

    # for one_outcome in test_outcomes:
    #     np.random.seed(2025)

    #     # Create preprocessing pipeline
    #     pipeline = PreprocessingPipeline(given_out=one_outcome, outcome_names=outcome_names)
        
    #     # Transform data
    #     X_train, y_train = pipeline.fit_transform(test_binned.iloc[train_index, :])
    #     X_test, y_test = pipeline.transform(test_binned.iloc[test_index, :])

    #     # Train and predict for individual models
    #     individual_predictions = {}
    #     for name, model in base_models:
    #         model.fit(X_train, y_train)
    #         pred = model.predict(X_test).tolist()[0]
    #         individual_predictions[name] = pred

    #         # Extract model coefficients
    #         if name in ['RIDGE', 'BayesRidge', 'LR', 'LASSO']:
    #             weights = model.coef_
    #         elif name == 'SVR':
    #             weights = model.coef_[0]
    #         elif name == 'RF':
    #             weights = model.feature_importances_

    #         results.append({
    #             'loocv_id': loocv_id,
    #             'out': one_outcome,
    #             'model': name,
    #             'pred': pred,
    #             'y': y_test.tolist()[0],
    #             'feat_names': X_train.columns.tolist(),
    #             'abs_weights': np.abs(weights).tolist() if weights is not None else None,
    #             'n_components': int(pipeline.pca_transformer.n_components_)
    #             if pipeline.pca_transformer is not None else -1,
    #             'preproc': preproc_method
    #         })
        
    #     # Stack the predictions from the individual models
    #     stacked_train_features = np.array([model.predict(X_train) for _, model in base_models]).T
    #     stacked_test_features = np.array(list(individual_predictions.values())).reshape(1, -1)

    #     # Train the stacked model
    #     stacked_model.fit(stacked_train_features, y_train)

    #     # Predict using the stacked model
    #     stacked_pred = stacked_model.predict(stacked_test_features).tolist()[0]

    #     # Append the stacked model results
    #     results.append({
    #         'loocv_id': loocv_id,
    #         'out': one_outcome,
    #         'model': 'stack_elastic',
    #         'pred': stacked_pred,
    #         'y': y_test.tolist()[0],
    #         'feat_names': list(individual_predictions.keys()),  # Names of base models
    #         'abs_weights': stacked_model.final_estimator_.coef_.tolist(),  # Stacked model weights
    #         # 'abs_weights': [], 
    #         'n_components': -1,
    #         'preproc': preproc_method
    #     })

    # # Print the results as JSON
    # print(json.dumps(results))

    ## BAGGING
    # loo = LeaveOneOut()
    # train_index, test_index = list(loo.split(test_binned))[loocv_id]

    # test_outcomes = None
    # if outcome_test_id == 0:
    #     test_outcomes = outcome_names
    # elif outcome_config_id == 1:
    #     test_outcomes = outcome_tests[outcome_test_id]
    # else:
    #     raise ValueError("unknown id")

    # results = []

    # # Define base models for bagging
    # base_models = [
    #     ('SVR', SVR(C=0.4, kernel='linear')),
    #     # ('RIDGE', Ridge(alpha=1.0, random_state=2025))
    #     ('Bayesian', BayesianRidge())
    # ]

    # # Final model (meta-model) for bagging - Bagging regressor doesn't require a meta-model like stacking
    # bagging_model = BaggingRegressor(n_estimators=15, random_state=2025)

    # for one_outcome in test_outcomes:
    #     np.random.seed(2025)

    #     # Create preprocessing pipeline
    #     pipeline = PreprocessingPipeline(given_out=one_outcome, outcome_names=outcome_names)
        
    #     # Transform data
    #     X_train, y_train = pipeline.fit_transform(test_binned.iloc[train_index, :])
    #     X_test, y_test = pipeline.transform(test_binned.iloc[test_index, :])

    #     # Train and predict for individual models
    #     individual_predictions = {}
    #     for name, model in base_models:
    #         model.fit(X_train, y_train)
    #         pred = model.predict(X_test).tolist()[0]
    #         individual_predictions[name] = pred

    #         # Collect weights (if applicable)
    #         if name == 'RIDGE':
    #             weights = model.coef_
    #         elif name == 'SVR':
    #             weights = model.coef_[0] if hasattr(model, 'coef_') else None
    #         elif name == 'RF':
    #             weights = model.feature_importances_

    #         results.append({
    #             'loocv_id': loocv_id,
    #             'out': one_outcome,
    #             'model': name,
    #             'pred': pred,
    #             'y': y_test.tolist()[0],
    #             'feat_names': X_train.columns.tolist(),
    #             'abs_weights': np.abs(weights).tolist() if weights is not None else [],
    #             'n_components': int(pipeline.pca_transformer.n_components_)
    #             if pipeline.pca_transformer is not None else -1,
    #             'preproc': preproc_method
    #         })
        
    #     # Prepare the predictions for bagging
    #     bagging_train_features = np.array([model.predict(X_train) for _, model in base_models]).T
    #     bagging_test_features = np.array(list(individual_predictions.values())).reshape(1, -1)

    #     # Train the bagging model
    #     bagging_model.fit(bagging_train_features, y_train)

    #     # Predict using the bagging model
    #     bagging_pred = bagging_model.predict(bagging_test_features).tolist()[0]

    #     # Append the bagging model results
    #     results.append({
    #         'loocv_id': loocv_id,
    #         'out': one_outcome,
    #         'model': 'bagging',
    #         'pred': bagging_pred,
    #         'y': y_test.tolist()[0],
    #         'feat_names': list(individual_predictions.keys()),  # Names of base models
    #         'abs_weights': bagging_model.estimators_[0].coef_.tolist() if hasattr(bagging_model.estimators_[0], 'coef_') else [],  # Weights from the first estimator
    #         'n_components': -1,
    #         'preproc': preproc_method
    #     })

    # # Print the results as JSON
    # print(json.dumps(results))

    ## BOOSTING
    # loo = LeaveOneOut()
    # train_index, test_index = list(loo.split(test_binned))[loocv_id]

    # test_outcomes = None
    # if outcome_test_id == 0:
    #     test_outcomes = outcome_names
    # elif outcome_config_id == 1:
    #     test_outcomes = outcome_tests[outcome_test_id]
    # else:
    #     raise ValueError("unknown id")

    # results = []

    # # Define base models for boosting
    # base_models = [
    #     ('SVR', SVR(C=0.4, kernel='linear')),
    #     ('RIDGE', Ridge(alpha=1.0, random_state=2025))
    #     #('Bayesian', BayesianRidge())
    # ]

    # # Final model (meta-model) for boosting - Switch from Bagging to Boosting
    # boosting_model = GradientBoostingRegressor(n_estimators=100, random_state=2025)

    # for one_outcome in test_outcomes:
    #     np.random.seed(2025)

    #     # Create preprocessing pipeline
    #     pipeline = PreprocessingPipeline(given_out=one_outcome, outcome_names=outcome_names)
        
    #     # Transform data
    #     X_train, y_train = pipeline.fit_transform(test_binned.iloc[train_index, :])
    #     X_test, y_test = pipeline.transform(test_binned.iloc[test_index, :])

    #     # Train and predict for individual models
    #     individual_predictions = {}
    #     for name, model in base_models:
    #         model.fit(X_train, y_train)
    #         pred = model.predict(X_test).tolist()[0]
    #         individual_predictions[name] = pred

    #         # Collect weights (if applicable)
    #         if name == 'RIDGE':
    #             weights = model.coef_
    #         elif name == 'SVR':
    #             weights = model.coef_[0] if hasattr(model, 'coef_') else None
    #         elif name == 'RF':
    #             weights = model.feature_importances_

    #         results.append({
    #             'loocv_id': loocv_id,
    #             'out': one_outcome,
    #             'model': name,
    #             'pred': pred,
    #             'y': y_test.tolist()[0],
    #             'feat_names': X_train.columns.tolist(),
    #             'abs_weights': np.abs(weights).tolist() if weights is not None else [],
    #             'n_components': int(pipeline.pca_transformer.n_components_)
    #             if pipeline.pca_transformer is not None else -1,
    #             'preproc': preproc_method
    #         })
        
    #     # Prepare the predictions for boosting
    #     boosting_train_features = np.array([model.predict(X_train) for _, model in base_models]).T
    #     boosting_test_features = np.array(list(individual_predictions.values())).reshape(1, -1)

    #     # Train the boosting model
    #     boosting_model.fit(boosting_train_features, y_train)

    #     # Predict using the boosting model
    #     boosting_pred = boosting_model.predict(boosting_test_features).tolist()[0]

    #     # Append the boosting model results
    #     results.append({
    #         'loocv_id': loocv_id,
    #         'out': one_outcome,
    #         'model': 'boosting',
    #         'pred': boosting_pred,
    #         'y': y_test.tolist()[0],
    #         'feat_names': list(individual_predictions.keys()),  # Names of base models
    #         'abs_weights': boosting_model.feature_importances_.tolist(),  # Feature importances from the boosting model
    #         'n_components': -1,
    #         'preproc': preproc_method
    #     })

    # # Print the results as JSON
    # print(json.dumps(results))

    ## WEIGHTED STACKING
    # loo = LeaveOneOut()
    # train_index, test_index = list(loo.split(test_binned))[loocv_id]
    # # bootstrapped = test_binned.sample(n = len(test_binned), replace=True)
    # # oob_ids = set(test_binned['BiPs_DID']) - set(bootstrapped['BiPs_DID'])
    # # oob_sample = test_binned[test_binned['BiPs_DID'].isin(oob_ids)]
    # test_outcomes = None
    # if outcome_test_id == 0:
    #     test_outcomes = outcome_names
    # elif outcome_config_id == 1:
    #     test_outcomes = outcome_tests[outcome_test_id]
    # else:
    #     raise ValueError("unknown id")

    # results = []

    # base_models = [
    #     # ('RIDGE', Ridge(alpha=1.0, random_state=2025)),
    #     ('SVR', SVR(C=0.4, kernel='linear')),
    #     ('Bayesian', BayesianRidge()),
    #     ('LinearReg', LinearRegression()),
    #     ('RF', RandomForestRegressor(random_state=2025))
    #     # ('XGB', XGBRegressor(n_estimators=100, learning_rate=0.1, max_depth=3, random_state=2025))
    # ]

    # # Final model (meta-model) for stacking
    # # final_model = LinearRegression()
    # final_model = Ridge(alpha=1.0, random_state=2025)

    # # Stacking regressor
    # stacked_model = StackingRegressor(estimators=base_models, final_estimator=final_model)

    # for one_outcome in test_outcomes:
    #     np.random.seed(2025)

    #     # Create preprocessing pipeline
    #     pipeline = PreprocessingPipeline(given_out=one_outcome, outcome_names=outcome_names)
        
    #     # Transform data
    #     X_train, y_train = pipeline.fit_transform(test_binned.iloc[train_index, :])
    #     X_test, y_test = pipeline.transform(test_binned.iloc[test_index, :])

    #     # Train and predict for individual models
    #     individual_predictions = {}
    #     model_rsq = {}  # To store R-squared values of each model
    #     for name, model in base_models:
    #         model.fit(X_train, y_train)
    #         pred = model.predict(X_test).tolist()[0]
    #         individual_predictions[name] = pred

    #         # Calculate R-squared for each model
    #         rsq = model.score(X_train, y_train)
    #         model_rsq[name] = rsq

    #         # Collect weights (if applicable)
    #         if name == 'RIDGE':
    #             weights = model.coef_
    #         elif name == 'SVR':
    #             weights = model.coef_[0] if hasattr(model, 'coef_') else None
    #         elif name == 'RF':
    #             weights = model.feature_importances_

    #         results.append({
    #             'loocv_id': loocv_id,
    #             'out': one_outcome,
    #             'model': name,
    #             'pred': pred,
    #             'y': y_test.tolist()[0],
    #             'feat_names': X_train.columns.tolist(),
    #             'abs_weights': np.abs(weights).tolist() if weights is not None else None,
    #             'n_components': int(pipeline.pca_transformer.n_components_)
    #             if pipeline.pca_transformer is not None else -1,
    #             'preproc': preproc_method
    #         })
        
    #     # Normalize R-squared values to use as weights (if negative, set to 0)
    #     total_rsq = sum([max(rsq, 0) for rsq in model_rsq.values()])
    #     normalized_weights = {model: max(rsq, 0) / total_rsq for model, rsq in model_rsq.items()}

    #     # Stack the predictions from the individual models, weighted by R-squared
    #     stacked_train_features = np.array([
    #         model.predict(X_train) * normalized_weights[name] for name, model in base_models
    #     ]).T
    #     stacked_test_features = np.array([
    #         individual_predictions[name] * normalized_weights[name] for name in individual_predictions
    #     ]).reshape(1, -1)

    #     # Train the stacked model
    #     stacked_model.fit(stacked_train_features, y_train)

    #     # Predict using the stacked model
    #     stacked_pred = stacked_model.predict(stacked_test_features).tolist()[0]

    #     # Append the stacked model results
    #     results.append({
    #         'loocv_id': loocv_id,
    #         'out': one_outcome,
    #         'model': 'stacked_weighted',
    #         'pred': stacked_pred,
    #         'y': y_test.tolist()[0],
    #         'feat_names': list(individual_predictions.keys()),  # Names of base models
    #         'abs_weights': stacked_model.final_estimator_.coef_.tolist(),  # Stacked model weights
    #         'n_components': -1,
    #         'preproc': preproc_method
    #     })

    # # Print the results as JSON
    # print(json.dumps(results))

    ## GRID SEARCH CV TUNING
    # loo = LeaveOneOut()
    # train_index, test_index = list(loo.split(test_binned))[loocv_id]
    # # bootstrapped = test_binned.sample(n = len(test_binned), replace=True)
    # # oob_ids = set(test_binned['BiPs_DID']) - set(bootstrapped['BiPs_DID'])
    # # oob_sample = test_binned[test_binned['BiPs_DID'].isin(oob_ids)]
    # test_outcomes = None
    # if outcome_test_id == 0:
    #     test_outcomes = outcome_names
    # elif outcome_config_id == 1:
    #     test_outcomes = outcome_tests[outcome_test_id]
    # else:
    #     raise ValueError("unknown id")

    # # Define parameter grids
    # param_grid_ridge = {'alpha': [0.001, 0.01, 0.1, 1, 10, 100]}
    # param_grid_svr = {'C': [0.01, 0.1, 0.4, 1, 10, 100]}

    # results = []
    # for one_outcome in test_outcomes:
    #     np.random.seed(2025)
        
    #     # Define base models
    #     base_models = {
    #         'RIDGE': Ridge(random_state=2025),
    #         'SVR': SVR(kernel='linear'),
    #         'Bayesian': BayesianRidge(),
    #         'LinReg': LinearRegression()
    #     }

    #     pipeline = PreprocessingPipeline(
    #         given_out=one_outcome,
    #         outcome_names=outcome_names
    #     )

    #     # Fit and transform training data
    #     X_train, y_train = pipeline.fit_transform(test_binned.iloc[train_index, :])
    #     X_test, y_test = pipeline.transform(test_binned.iloc[test_index, :])
        
    #     feature_names = X_train.columns.tolist()

    #     grid_ridge = GridSearchCV(base_models['RIDGE'], param_grid_ridge, scoring='r2', cv=5)
    #     grid_svr = GridSearchCV(base_models['SVR'], param_grid_svr, scoring='r2', cv=5)

    #     # Fit tuned models
    #     grid_ridge.fit(X_train, y_train)
    #     grid_svr.fit(X_train, y_train)

    #     # Get best models and parameters
    #     best_ridge = grid_ridge.best_estimator_
    #     best_svr = grid_svr.best_estimator_
    #     best_ridge_params = grid_ridge.best_params_
    #     best_svr_params = grid_svr.best_params_

    #     models = {
    #         'RIDGE': best_ridge,
    #         'SVR': best_svr,
    #         'Bayesian': base_models['Bayesian'],
    #         'LinReg': base_models['LinReg']
    #     }

    #     for name, model in models.items():
    #         model.fit(X_train, y_train)

    #         if name == 'RIDGE':
    #             weights = model.coef_
    #             best_params = best_ridge_params  # Store best params for Ridge
    #         elif name == 'SVR':
    #             weights = model.coef_[0]
    #             best_params = best_svr_params  # Store best params for SVR
    #         else:
    #             best_params = None

    #         results.append({
    #             'loocv_id': loocv_id,
    #             'out': one_outcome,
    #             'model': name,
    #             'best_params': best_params,  # Log the best parameters here
    #             'pred': model.predict(X_test).tolist()[0],
    #             'y': y_test.tolist()[0],
    #             'feat_names': feature_names,
    #             'abs_weights': np.abs(weights).tolist(),
    #             'n_components': int(pipeline.pca_transformer.n_components_)
    #                 if pipeline.pca_transformer is not None else -1,
    #             'preproc': preproc_method
    #         })

    #     print(json.dumps(results))


if __name__ == '__main__':
    loocv_id = int(sys.argv[1])
    outcome_config_id = int(sys.argv[2])
    run_loocv_fold(loocv_id, outcome_config_id, sys.argv[3])