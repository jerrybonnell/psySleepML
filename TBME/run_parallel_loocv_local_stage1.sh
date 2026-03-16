#!/bin/bash

conda activate myenv

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PYTHON_SCRIPT="${SCRIPT_DIR}/loocv_worker_2.py"
N_FOLDS=149
N_JOBS=10
OUTPUT_FILE="${SCRIPT_DIR}/loocv_results.jsonl"

OUTCOME_CONFIG_ID=${1:-1}
PREPROC_METHOD=${2:-"100"}

# Remove output file if it exists
rm -f "$OUTPUT_FILE"

export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export NUMEXPR_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export PYTHONHASHSEED=0

parallel --jobs $N_JOBS \
    --progress \
    --bar \
    --eta \
    "python \"${PYTHON_SCRIPT}\" {1} ${OUTCOME_CONFIG_ID} {2}" \
    ::: $(seq 0 $(($N_FOLDS-1))) \
    ::: "000" "001" "010" "011" "100" "101" "110" "111" \
    >> "$OUTPUT_FILE"

python - <<EOF
import json
import numpy as np
import pandas as pd
from tqdm import tqdm
from sklearn.metrics import root_mean_squared_error
from sklearn.metrics import r2_score
from datetime import datetime
from scipy.stats import spearmanr
import zlib
from numpy.random import default_rng

BASE_SEED = 2025

def seed_for_group(outcome, model, preproc):
    key = f"{outcome}|{model}|{preproc}".encode("utf-8")
    return (BASE_SEED + zlib.crc32(key)) % (2**32)

results = {
    'loocv_id': [], 'out': [], 'model': [],
    'pred': [], 'y': [], 'n_components': [],
    'feat_names': [], 'abs_weights': [], 'preproc': []
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
for (outcome, model_type, preproc), group in tqdm(res.groupby(['out', 'model', 'preproc'], sort=True)):
    all_features = set().union(*group['feat_names'])
    all_features = sorted(all_features)

    group_new = []
    # compute average weight for each feature over the LOOCV trial; then
    # later collect those results into the final CSV file
    for _, row in group.iterrows():
        feat_names = row['feat_names']
        weights = row['abs_weights']
        feat_weights_dict = dict(zip(feat_names, weights))

        new_features = []
        new_weights = []
        # For each feature, retrieve its weight, defaulting to 0 if it's missing???
        for feature in all_features:
            new_features.append(feature)
            new_weights.append(feat_weights_dict.get(feature, 0))
        # update feature names and weights so all LOOCV folds are guaranteed
        # to have same number of each
        row['feat_names'] = new_features
        row['abs_weights'] = new_weights
        group_new.append(row)
    group_new = pd.DataFrame(group_new)

    arrays_wt_mean = np.mean(np.array(group_new['abs_weights'].tolist()), axis=0)
    arrays_names = np.array(group_new['feat_names'].tolist())[0]
    ncomp_mean = np.mean(group_new['n_components'].tolist())

    for feature, weight in zip(arrays_names, arrays_wt_mean):
        grouped_wt_results.append({
            'out': outcome,
            'model': model_type,
            'var': feature,
            'val': weight,
            'preproc': preproc,
            'n_components': ncomp_mean
        })

timestamp = datetime.now().strftime("%m%d%y_%H%M%S")
var_imp_df = pd.DataFrame(grouped_wt_results)
var_imp_df.to_csv(f"loocv_var_imp{timestamp}.csv", index=False)
print(f"\nloocv_var_imp{timestamp}.csv")

grouped = res.groupby(['out', 'model', 'preproc'], sort=True)
new_df_dic = {'loocv_id': [], 'out': [], 'model': [], 'rsq': [], 'rmse': [], 'spearman': [],
              'preproc': []}

for (outcome, model, preproc), group in tqdm(grouped):
    rng = default_rng(seed_for_group(outcome, model, preproc))
    n_bootstraps = 1000
    for i in range(n_bootstraps):
        model_preds = np.full(149, np.nan)
        model_y = np.full(149, np.nan)
        for idx, pred in zip(group['loocv_id'].to_numpy(), group['pred'].to_numpy()):
            model_preds[idx] = pred
        for idx, y in zip(group['loocv_id'].to_numpy(), group['y'].to_numpy()):
            model_y[idx] = y

        indices = rng.integers(0, 149, 149)
        correlation = np.corrcoef(model_y[indices],
                                model_preds[indices])[0, 1]
        rsq = correlation ** 2 # should follow yardstick's rsq computation
        spearman_corr, _ = spearmanr(model_y[indices], model_preds[indices])
        rmse = root_mean_squared_error(model_y[indices], model_preds[indices])
        new_df_dic['loocv_id'].append(i)
        new_df_dic['out'].append(group['out'].unique()[0])
        assert len(group['preproc'].unique()) == 1
        new_df_dic['preproc'].append(group['preproc'].unique()[0])
        new_df_dic['model'].append(group['model'].unique()[0])
        # my_r2 = r2_score(model_y[indices],  model_preds[indices])
        new_df_dic['rsq'].append(rsq)
        new_df_dic['rmse'].append(rmse)
        new_df_dic['spearman'].append(spearman_corr)

# Save to CSV
new_df = pd.DataFrame(new_df_dic)
output_csv = f"loocv_bstrap_results{timestamp}.csv"
new_df.to_csv(output_csv, index=False)
print(f"\nResults saved to {output_csv}")
EOF
