#!/bin/bash

# activate environment
conda activate myenv

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PYTHON_SCRIPT="${SCRIPT_DIR}/loocv_worker_stage2.py"

N_FOLDS=149
N_JOBS=10

MANIFEST_CSV="${SCRIPT_DIR}/loocv_bstrap_results021026_combined_stage2.csv"
TASKS_FILE="${SCRIPT_DIR}/stage2_tasks.tsv"

OUTPUT_FILE="${SCRIPT_DIR}/loocv_stage2_results.jsonl"
rm -f "$OUTPUT_FILE" "$TASKS_FILE"

# ---- build tasks: outcome, model, preproc, view ----
python - <<PY
import pandas as pd

manifest_path = "${MANIFEST_CSV}"
df = pd.read_csv(manifest_path)

required = {"preproc_org","model","feat","db"}
missing = required - set(df.columns)
if missing:
    raise SystemExit(f"Manifest missing columns: {missing}")

views = ["pt", "fm", "both"]

rows = []
for _, r in df.iterrows():
    outcome = f"{r['feat']}_{r['db']}"
    model = str(r["model"]).strip()
    preproc = str(int(r["preproc_org"])).zfill(3)  # e.g., 110/111
    for v in views:
        rows.append((outcome, model, preproc, v))

out = pd.DataFrame(rows, columns=["outcome","model","preproc","view"])
out.to_csv("stage2_tasks.tsv", sep="\t", index=False, header=False)
print(out.head(10))
print(f"Wrote stage2_tasks.tsv with {len(out)} tasks (outcome*model*preproc*view)")
PY

export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export NUMEXPR_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export PYTHONHASHSEED=0

# ---- run LOOCV for each fold × task ----
parallel --jobs "$N_JOBS" \
    --progress \
    --bar \
    --eta \
    --colsep '\t' \
    "python \"${PYTHON_SCRIPT}\" {1} 0 {4} {3} {2} {5}" \
    ::: $(seq 0 $(($N_FOLDS-1))) \
    :::: "$TASKS_FILE" \
    >> "$OUTPUT_FILE"

# ---- aggregate results ----
python - <<'EOF'
import json
import numpy as np
import pandas as pd
from tqdm import tqdm
from sklearn.metrics import root_mean_squared_error
from datetime import datetime
from scipy.stats import spearmanr
import zlib
from numpy.random import default_rng
from scipy.stats import rankdata

BASE_SEED = 2025

def seed_for_group(outcome, model, preproc, view):
    key = f"{outcome}|{model}|{preproc}|{view}".encode("utf-8")
    return (BASE_SEED + zlib.crc32(key)) % (2**32)

infile = "loocv_stage2_results.jsonl"

results = {
    "loocv_id": [], "out": [], "model": [], "preproc": [], "view": [],
    "pred": [], "y": [], "n_components": [],
    "feat_names": [], "abs_weights": []  # keep weights for optional var-imp
}

total_lines = sum(1 for _ in open(infile, "r"))

with open(infile, "r") as f:
    for line in tqdm(f, total=total_lines):
        batch_results = json.loads(line)
        for result in batch_results:
            for key in results:
                results[key].append(result.get(key))

res = pd.DataFrame(results)
print(res.head())

dup = res.groupby(["loocv_id","out","model","preproc","view"]).size()
bad = dup[dup > 1]
assert not len(bad)

timestamp = datetime.now().strftime("%m%d%y_%H%M%S")

# ---- (optional) var importance: average weights by group ----
grouped_wt_results = []
for (outcome, model_type, preproc, view), group in tqdm(res.groupby(["out","model","preproc","view"], sort=True)):
    all_features = sorted(set().union(*group["feat_names"]))

    group_new = []
    for _, row in group.iterrows():
        feat_names = row["feat_names"]
        weights = row["abs_weights"]
        feat_weights_dict = dict(zip(feat_names, weights))

        row["feat_names"] = all_features
        row["abs_weights"] = [feat_weights_dict.get(f, 0) for f in all_features]
        group_new.append(row)

    group_new = pd.DataFrame(group_new)
    arrays_wt_mean = np.mean(np.array(group_new["abs_weights"].tolist()), axis=0)
    ncomp_mean = np.mean(group_new["n_components"].tolist())

    for feature, weight in zip(all_features, arrays_wt_mean):
        grouped_wt_results.append({
            "out": outcome,
            "model": model_type,
            "preproc": preproc,
            "view": view,
            "var": feature,
            "val": weight,
            "n_components": ncomp_mean
        })

var_imp_df = pd.DataFrame(grouped_wt_results)
var_imp_df.to_csv(f"loocv_stage2_var_imp_{timestamp}.csv", index=False)
print(f"\nloocv_stage2_var_imp_{timestamp}.csv")

# ---- bootstrap summary per (out, model, preproc, view) ----
grouped = res.groupby(["out","model","preproc","view"])
new_df_dic = {
    "boot_id": [], "out": [], "model": [], "preproc": [], "view": [],
    "rsq": [], "rmse": [], 'spearman': []
}

for (outcome, model, preproc, view), group in tqdm(grouped):
    rng = default_rng(seed_for_group(outcome, model, preproc, view))
    n_bootstraps = 1000
    for b in range(n_bootstraps):
        model_preds = np.full(149, np.nan)
        model_y = np.full(149, np.nan)
        for idx, pred in zip(group["loocv_id"].to_numpy(), group["pred"].to_numpy()):
            model_preds[idx] = pred
        for idx, y in zip(group["loocv_id"].to_numpy(), group["y"].to_numpy()):
            model_y[idx] = y

        indices = rng.integers(0, 149, 149)
        y_s = model_y[indices]
        p_s = model_preds[indices]
        m = np.isfinite(y_s) & np.isfinite(p_s)
        assert m.sum() >= 3
        spearman_corr, _ = spearmanr(y_s[m], p_s[m])
        rsq = float(spearman_corr) ** 2
        rmse = root_mean_squared_error(y_s[m], p_s[m])

        new_df_dic["boot_id"].append(b)
        new_df_dic["out"].append(outcome)
        new_df_dic["model"].append(model)
        new_df_dic["preproc"].append(preproc)
        new_df_dic["view"].append(view)
        new_df_dic["rsq"].append(rsq)
        new_df_dic["rmse"].append(rmse)
        new_df_dic["spearman"].append(spearman_corr)

boot_df = pd.DataFrame(new_df_dic)
boot_csv = f"loocv_stage2_boot_{timestamp}.csv"
boot_df.to_csv(boot_csv, index=False)
print(f"\nResults saved to {boot_csv}")

def actor_partner_views(outcome: str):
    o = str(outcome).lower()
    if "_pt1_" in o:
        return "pt", "fm"
    if "_fm1_" in o:
        return "fm", "pt"
    raise ValueError(f"Cannot infer actor from outcome name: {outcome}")

def _rank(x: np.ndarray) -> np.ndarray:
    """
    Rank transform with average ranks for ties; preserves NaNs.
    Spearman uses ranks; we do this explicitly for partial Spearman.
    """
    x = np.asarray(x, dtype=float)
    out = np.full_like(x, np.nan, dtype=float)
    m = np.isfinite(x)
    if m.sum() == 0:
        return out
    out[m] = rankdata(x[m], method="average")
    return out

def _residualize(y: np.ndarray, x: np.ndarray) -> np.ndarray:
    """
    Residualize y on x with intercept using least squares; preserves NaNs.
    Returns residuals aligned with original indices (NaN where missing).
    """
    y = np.asarray(y, dtype=float)
    x = np.asarray(x, dtype=float)
    r = np.full_like(y, np.nan, dtype=float)
    m = np.isfinite(y) & np.isfinite(x)
    if m.sum() < 3:
        return r
    X = np.column_stack([np.ones(m.sum()), x[m]])
    beta, *_ = np.linalg.lstsq(X, y[m], rcond=None)
    yhat = X @ beta
    r[m] = y[m] - yhat
    return r



resid_rows = {
    "boot_id": [], "out": [], "model": [], "preproc": [],
    "resid_spearman": [], "resid_rsq": []
}

resid_delta_rows = {
    "boot_id": [], "out": [], "model": [], "preproc": [],
    "delta_resid_spearman": [], "delta_resid_rsq": []
}

partial_rows = {
    "boot_id": [], "out": [], "model": [], "preproc": [],
    "partial_spearman_add": [], "partial_rsq_add": []
}

grouped_ap = res.groupby(["out","model","preproc"], sort=True)
for (outcome, model, preproc), group in tqdm(grouped_ap, desc="Residual dyad test"):
    # Build full-length arrays aligned by loocv_id for each view
    y_all   = np.full(149, np.nan, dtype=float)
    pt_all  = np.full(149, np.nan, dtype=float)
    fm_all  = np.full(149, np.nan, dtype=float)
    both_all= np.full(149, np.nan, dtype=float)

    for _, row in group.iterrows():
        i = int(row["loocv_id"])
        y_all[i] = float(row["y"])
        v = str(row["view"])
        if v == "pt":
            pt_all[i] = float(row["pred"])
        elif v == "fm":
            fm_all[i] = float(row["pred"])
        elif v == "both":
            both_all[i] = float(row["pred"])

    actor_v, partner_v = actor_partner_views(outcome)
    actor_pred   = pt_all if actor_v == "pt" else fm_all
    partner_pred = fm_all if partner_v == "fm" else pt_all

    # residuals from actor-only model
    resid = y_all - actor_pred
    delta = both_all - actor_pred

    rng = default_rng(seed_for_group(outcome, model, preproc, "RESID_PARTNER"))
    n_bootstraps = 1000
    for b in range(n_bootstraps):
        idx = rng.integers(0, 149, 149)
        r_s = resid[idx]
        p_s = partner_pred[idx]
        m = np.isfinite(r_s) & np.isfinite(p_s)
        if m.sum() < 3:
            rho = np.nan
        else:
            rho, _ = spearmanr(r_s[m], p_s[m])
        resid_rows["boot_id"].append(b)
        resid_rows["out"].append(outcome)
        resid_rows["model"].append(model)
        resid_rows["preproc"].append(preproc)
        resid_rows["resid_spearman"].append(rho)
        resid_rows["resid_rsq"].append(np.nan if not np.isfinite(rho) else float(rho)**2)

    # delta-residual test (suppression-in-BOTH): Spearman(resid, delta)
    rng2 = default_rng(seed_for_group(outcome, model, preproc, "RESID_DELTA"))
    for b in range(n_bootstraps):
        idx = rng2.integers(0, 149, 149)
        r_s = resid[idx]
        d_s = delta[idx]
        m = np.isfinite(r_s) & np.isfinite(d_s)
        if m.sum() < 3:
            rho2 = np.nan
        else:
            rho2, _ = spearmanr(r_s[m], d_s[m])
        resid_delta_rows["boot_id"].append(b)
        resid_delta_rows["out"].append(outcome)
        resid_delta_rows["model"].append(model)
        resid_delta_rows["preproc"].append(preproc)
        resid_delta_rows["delta_resid_spearman"].append(rho2)
        resid_delta_rows["delta_resid_rsq"].append(np.nan if not np.isfinite(rho2) else float(rho2)**2)

    # partial Spearman incremental value:
    # ρ( y , ŷ_both | ŷ_actor ) computed on ranks
    rng3 = default_rng(seed_for_group(outcome, model, preproc, "PARTIAL_ADD"))
    for b in range(n_bootstraps):
        idx = rng3.integers(0, 149, 149)
        y_s = y_all[idx]
        a_s = actor_pred[idx]
        b_s = both_all[idx]
        m = np.isfinite(y_s) & np.isfinite(a_s) & np.isfinite(b_s)
        if m.sum() < 3:
            prho = np.nan
        else:
            ry = _rank(y_s)
            ra = _rank(a_s)
            rb = _rank(b_s)
            ey = _residualize(ry, ra)
            eb = _residualize(rb, ra)
            mm = np.isfinite(ey) & np.isfinite(eb)
            if mm.sum() < 3:
                prho = np.nan
            else:
                # Pearson on residualized ranks (equivalently partial Spearman)
                prho = float(np.corrcoef(ey[mm], eb[mm])[0, 1])

        partial_rows["boot_id"].append(b)
        partial_rows["out"].append(outcome)
        partial_rows["model"].append(model)
        partial_rows["preproc"].append(preproc)
        partial_rows["partial_spearman_add"].append(prho)
        partial_rows["partial_rsq_add"].append(np.nan if not np.isfinite(prho) else float(prho)**2)



resid_df = pd.DataFrame(resid_rows)
resid_csv = f"loocv_stage2_resid_boot_{timestamp}.csv"
resid_df.to_csv(resid_csv, index=False)
print(f"\nResidual dyad results saved to {resid_csv}")

resid_delta_df = pd.DataFrame(resid_delta_rows)
resid_delta_csv = f"loocv_stage2_resid_delta_boot_{timestamp}.csv"
resid_delta_df.to_csv(resid_delta_csv, index=False)
print(f"\nDelta-residual dyad results saved to {resid_delta_csv}")

partial_df = pd.DataFrame(partial_rows)
partial_csv = f"loocv_stage2_partial_boot_{timestamp}.csv"
partial_df.to_csv(partial_csv, index=False)
print(f"\nPartial Spearman add-value results saved to {partial_csv}")
EOF