# evidence-factor-simulation
```bash
  .
  ├── evidence_factors_backdoor_frontdoor.R — simulation study of the hypothesis test using data generated by dgp_backdoor_frontdoor.R
  ├── evidence_factors_backdoor_iv.R — simulation study of the hypothesis test using data generated by dgp_backdoor_IV.R
  ├── evidence_factors_frontdoor_iv.R — simulation study of the hypothesis test using data generated by dgp_frontdoor_IV.R
  ├── evidence_factors_backdoor_frontdoor_iv.R — simulation study of the hypothesis test using data generated by dgp_backdoor_frontdoor_IV.R
  ├── R                             
  │   ├── dgp_backdoor_frontdoor.R - data generation functions for the union model of backdoor and front-door when assumptions of zero/one model is violated 
  │   ├── dgp_backdoor_IV.R - data generation functions for the union model of backdoor and IV when assumptions of zero/one model is violated
  │   ├── dgp_frontdoor_IV.R - data generation functions for the union model of front-door and IV when assumptions of zero/one model is violated
  │   ├── dgp_backdoor_frontdoor_IV.R - data generation functions for the union model of backdoor, front-door, and IV when assumptions of zero/one/two models are violated
  │   ├── estimators.R - functions of backdoor, front-door, and IV asymptotic linear estimators
  │   ├── functions.R - other functions used in the study
  │   └── tests                
  │       ├── test_estimators.R - test the correctness of the functions in estimators.R 
  │       └── test_functions.R - test estimating and evidence factor functions in functions.R
  └── README.md   
  
```