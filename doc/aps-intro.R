## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=F------------------------------------------------------------
library(qps)

## ---- eval=F------------------------------------------------------------------
#  qps <- estimate_qps(data, algorithm_object, ml_type="library of algorithm object", Xc=c("vector", "of", "continuous", "variable", "names"), Xd=c("vector", "of", "discrete", "variable", "names"))
#  ivreg_model <- estimate_treatment_effect(qps = qps, Y = outcome_vector, Z=treatment_reccomendation, D = treatment_assignment)
#  summary(ivreg_model)

## ---- eval=F------------------------------------------------------------------
#  L <- list("varname1" = c(discrete, values), "varname2" = c(discrete, values))
#  qps <- estimate_qps(data, algorithm_object, ml_type="library of algorithm object", infer=T, L = L, parallel = T, seed=1)

## ---- echo=F, eval=F----------------------------------------------------------
#  output <- estimate_counterfactual_ml(Y=outcome_vector, Z=treatment_recommendation,ML=original_algo_output, ML_new=counterfactual_algo_output)

## ---- eval=F------------------------------------------------------------------
#  mlm <- covariate_balance_test(qps = qps, X = algo_inputs, Z = treatment_recommendation)

