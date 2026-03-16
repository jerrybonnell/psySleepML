# TBME: Dyadic Endocrine Regulation in Oncology Dyads

Code accompanying "Dyadic Interdependence in Endocrine Functioning: A 
Multilevel Machine Learning Study of Adults with Cancer and Their Caregivers", 
submitted to IEEE Transactions on Biomedical Engineering (TBME).

This study evaluated dyadic interdependence in endocrine regulation among 
adults with cancer and their spousal caregivers using a machine learning 
framework that identified whether and how partner-level information improves 
prediction of individuals' stress biomarker responses (cortisol, alpha-amylase, 
DHEAS).

## Scripts

**Stage 1: Comparative Model Performance**  
LOOCV with bootstrap resampling to identify the optimal modeling pipeline 
across preprocessing configurations and outcome variables.  
`run_parallel_loocv_local_stage1.sh` `loocv_worker_stage1.py` `supervized_viz_stage1.R`

**Stage 2: Incremental Dyadic Contributions**  
Compares actor-only and dyadic predictor specifications to evaluate whether 
partner-level predictors incrementally improve prediction of endocrine outcomes.  
`run_stage2.sh` `loocv_worker_stage2.py` `supervized_viz_stage2.R`

**Stage 3: Redistribution of Predictive Importance**  
SHAP values computed under actor-only and dyadic specifications to examine 
how predictor importance redistributes after incorporating partner information.  
`loocv_shap_exp_stage3.py` `shap_viz_stage3_summary.R` `shap_viz_stage3_both.R` `shap_viz_stage3.R`
