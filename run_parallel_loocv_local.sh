#!/bin/bash

conda activate myenv

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PYTHON_SCRIPT="${SCRIPT_DIR}/loocv_worker.py"
N_FOLDS=149
N_JOBS=30
OUTPUT_FILE="${SCRIPT_DIR}/loocv_results.jsonl"

# Remove output file if it exists
rm -f $OUTPUT_FILE

parallel --jobs $N_JOBS \
    --progress \
    --bar \
    --eta \
    "python ${PYTHON_SCRIPT} {1} 0 {2}" \
    ::: $(seq 0 $(($N_FOLDS-1))) \
    ::: "000" "001" "010" "011" "100" "101" "110" "111" \
    >> $OUTPUT_FILE

python - <<EOF
import json
import numpy as np
import pandas as pd
from tqdm import tqdm
from sklearn.metrics import root_mean_squared_error
from sklearn.metrics import r2_score
from datetime import datetime

# PREPROC_ID = ${PREPROC_ID}

results = {
    'loocv_id': [], 'out': [], 'model': [],
    'pred': [], 'y': [],
    'feat_names': [], 'abs_weights': [], 'n_components': [], 'preproc': [] #, 'best_params': []
}
total_lines = sum(1 for line in open('loocv_results.jsonl', 'r'))
# assert total_lines == 149 * 8

with open('loocv_results.jsonl', 'r') as f:
    for line in tqdm(f, total=total_lines):
        batch_results = json.loads(line)
        for result in batch_results:
            for key in results:
                results[key].append(result[key])

res = pd.DataFrame(results)
print(res.head())

grouped_wt_results = []
for (outcome, model_type, preproc), group in tqdm(res.groupby(['out', 'model', 'preproc'])):
    all_features = set().union(*group['feat_names'])
    all_features = sorted(all_features)

    group_new = []
    # compute average weight for each feature over the LOOCV trial; then
    # later collect those results into the final CSV file
    for _, row in group.iterrows():
        feat_names = row['feat_names']
        weights = row['abs_weights']
        # shap_vals = row['shap_values']

        feat_weights_dict = dict(zip(feat_names, weights))
        # feat_shap_dict = dict(zip(feat_names, shap_vals))

        # new_features = []
        # new_weights = []
        # new_features, new_weights, new_shap_vals = [], [], []
        new_features, new_weights = [], []

        # For each feature, retrieve its weight, defaulting to 0 if it's missing???
        for feature in all_features:
            new_features.append(feature)
            new_weights.append(feat_weights_dict.get(feature, 0))
            # new_shap_vals.append(feat_shap_dict.get(feature, 0))
        # update feature names and weights so all LOOCV folds are guaranteed
        # to have same number of each
        row['feat_names'] = new_features
        row['abs_weights'] = new_weights
        # row['shap_values'] = new_shap_vals
        group_new.append(row)

    group_new = pd.DataFrame(group_new)

    arrays_wt_mean = np.mean(np.array(group_new['abs_weights'].tolist()), axis=0)
    # arrays_shap_mean = np.mean(np.array(group_new['shap_values'].tolist()), axis=0)
    arrays_names = np.array(group_new['feat_names'].tolist())[0]
    ncomp_mean = np.mean(group_new['n_components'].tolist())

    # for feature, weight, shap_val in zip(arrays_names, arrays_wt_mean, arrays_shap_mean):
    for feature, weight in zip(arrays_names, arrays_wt_mean):
        grouped_wt_results.append({
            'out': outcome,
            'model': model_type,
            'var': feature,
            'val': weight,
            # 'shap_val': shap_val,
            'preproc': preproc,
            'n_components': ncomp_mean
            # 'best_params': group['best_params'].iloc[0]
        })

timestamp = datetime.now().strftime("%m%d%y")
var_imp_df = pd.DataFrame(grouped_wt_results)
var_imp_df.to_csv(f"loocv_var_imp{timestamp}.csv", index=False)
print(f"\nloocv_var_imp{timestamp}_test.csv")

grouped = res.groupby(['out', 'model', 'preproc'])
np.random.seed(2025)
new_df_dic = {
    'loocv_id': [],
    'out': [],
    'model': [],
    'rsq': [],
    'rmse': [],
    'num_feats': [],
    'num_q':[],
    'num_cardio':[],
    'num_co':[],
    'num_clo':[],
    'preproc': []
    # 'best_params': []
}

for (feature, model, preproc), group in tqdm(grouped):
    n_bootstraps = 1000

    all_feats = sorted(set().union(*group['feat_names']))
    num_feats = len(all_feats)

    num_q = sum(1 for feat in all_feats if '_q' in feat)
    num_cardio = sum(1 for feat in all_feats if feat.endswith('_cardio'))
    num_co = sum(1 for feat in all_feats if feat.endswith('_co'))
    num_clo = sum(1 for feat in all_feats if '_clo' in feat)

    # best_params = group['best_params'].iloc[0] 

    for i in range(n_bootstraps):
        model_preds = np.full(149, np.nan)
        model_y = np.full(149, np.nan)
        for idx, pred in zip(group['loocv_id'].to_numpy(), group['pred'].to_numpy()):
            model_preds[idx] = pred
        for idx, y in zip(group['loocv_id'].to_numpy(), group['y'].to_numpy()):
            model_y[idx] = y

        indices = np.random.randint(0, 149, 149)
        correlation = np.corrcoef(model_y[indices],
                                model_preds[indices])[0, 1]
        rsq = correlation ** 2 # should follow yardstick's rsq computation
        rmse = root_mean_squared_error(model_y[indices], model_preds[indices])
        new_df_dic['loocv_id'].append(i)
        new_df_dic['out'].append(group['out'].unique()[0])
        assert len(group['preproc'].unique()) == 1
        new_df_dic['preproc'].append(group['preproc'].unique()[0])
        new_df_dic['model'].append(group['model'].unique()[0])
        # my_r2 = r2_score(model_y[indices],  model_preds[indices])
        new_df_dic['rsq'].append(rsq)
        new_df_dic['rmse'].append(rmse)
        new_df_dic['num_feats'].append(num_feats)
        new_df_dic['num_q'].append(num_q)
        new_df_dic['num_cardio'].append(num_cardio)
        new_df_dic['num_co'].append(num_co)
        new_df_dic['num_clo'].append(num_clo)
        # new_df_dic['best_params'].append(best_params)

# Save to CSV
new_df = pd.DataFrame(new_df_dic)
output_csv = f"loocv_bstrap_results{timestamp}_test.csv"
new_df.to_csv(output_csv, index=False)
print(f"\nResults saved to {output_csv}")
EOF
