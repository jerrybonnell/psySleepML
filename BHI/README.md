# BHI: Explainable ML Framework for Sleep Functioning in Oncology Dyads

Code accompanying "An Explainable Machine Learning Framework to Inform 
Integrative Psychosocial Correlates of Sleep Functioning in Adults with 
Cancer and their Caregivers", presented at IEEE International Conference 
on Biomedical and Health Informatics (BHI).

This study utilized multi-modal, multi-level data from patients with 
colorectal cancer and spousal caregivers to identify critical dyadic stress 
regulatory factors and psychosocial predictors of sleep health using 
supervised machine learning and SHAP analysis.

## Scripts

**`ml/`**  
LOOCV with bootstrap resampling to evaluate predictive performance across 
preprocessing configurations and sleep outcomes.  
`run_parallel_loocv_local.sh` `loocv_worker.py`

**`eda/`**  
SHAP-based predictor importance and commonality analysis to identify key 
dyadic stress regulatory factors.  
`loocv_shap_exp.py` `commonality_analysis.R` `supervized_viz.R`
