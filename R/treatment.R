#' Main treatment effect estimation function
#'
#' @param data Dataset containing QPS, Y, Z, and D data.
#' @param qps Column name of QPS variable. Defaults to "QPS".
#' @param Y Column name of Y variable. Defaults to "Y".
#' @param Z Column name of Z variable. Defaults to "Z".
#' @param D Column name of D variable. Defaults to "D".
#' @param estimator String method of IV estimation. Defaults to "2SLS".
#' @param verbose Boolean indicator for whether to print summary output of estimation. Defaults to True.
#' @return Fitted ivreg model object.
#' @examples
#'
#' @details
#' Treatment effect is estimated using IV estimation The default is to use the 2SLS method
#' of estimation, with the equations illustrated below.
#' \mjdeqn{D_i = \gamma_0(1-I) + \gamma_1 Z_i + \gamma_2 p^s(X_i;\delta) + v_i}{D_i = gamma_0(1-I) + gamma_1 Z_i + gamma_2 p^s(X_i;delta) + v_i}
#' \mjdeqn{Y_i = \beta_0(1-I) + \beta_1 D_i + \beta_2 p^s(X_i;\delta) + \epsilon_i}{Y_i = beta_0(1-I) + beta_1 D_i + beta_2 p^s(X_i;delta) + epsilon_i}
#' \mjeqn{\beta_1}{beta_1} is our causal estimation of the treatment effect. I is an indicator for if the ML funtion takes only a single
#' nondegenerate value in the sample.
#' @export
estimate_treatment_effect <- function(data, qps="QPS", Y = "Y", Z = "Z", D = "D", estimator = "2SLS", verbose=T){
  setDT(data)
  # Keep only values with non-degenerate QPS
  print(paste0("Estimating ", estimator, " on ", nrow(data[!(get(qps) %in% c(0,1))]), " out of ",
               nrow(data), " observations where QPS is non-degenerate..."))
  model <- ivreg::ivreg(get(Y) ~ get(D) + get(qps)|get(Z) + get(qps), data[!(get(qps) %in% c(0,1))])
  if (verbose == T){
    summary(model)
  }
  return(model)
}

#' Estimate counterfactual performance of another algorithm
#'
#' @param data Dataset containing QPS, Y, Z, the original ML algorithm predictions,
#' and the counterfactual ML' algorithm predictions.
#' @param qps Column name of QPS variable. Defaults to "QPS".
#' @param Y Column name of Y variable. Defaults to "Y".
#' @param Z Column name of Z variable. Defaults to "Z".
#' @param ml_out Column name of original ML prediction variable. Defaults to "ML1".
#' @param cf_ml_out Column name of counterfactual ML' prediction variable. Defaults to "ML2".
#' @param verbose Boolean indicator for whether to print summary output of estimation. Defaults to True.
#' @return List containing counterfactual predictions and fitted lm model object.
#' @examples
#' @details
#' The process of estimating counterfactual value works as follows. First we we fit the
#' below OLS regression using historical recommendations and outcome \code{Z} and \code{Y}.
#' \mjdeqn{Y_i = \beta_0 + \beta_1 Z_i + \beta_2 p^s(X_i; \delta) + \epsilon_i}{Y_i = beta_0 + beta_1 Z_i + beta_2 p^s(X_i; delta) + epsilon_i}
#' where \mjeqn{\beta_1} is our estimated effect of treatment recommendation.
#'
#' Then we take the original algorithm output \code{ml_out} and the counterfactual algorithm output \code{cf_ml_out}
#' and estimate the below value equation.
#' \mjdeqn{\hat{V}(ML') = \frac{1}{n} \sum_{i=1}^n (Y_i + \hat{\beta_{ols}}(ML'(X_i) - ML(X_i)))}
#' @export
estimate_counterfactual_ml <- function(data, qps = "QPS", Y = "Y", Z = "Z", ml_out = "ML1", cf_ml_out="ML2",
                                       verbose=T){
  setDT(data)
  print(paste0("Estimating counterfactual value on ", nrow(data[!(get(qps) %in% c(0,1))]), " out of ",
               nrow(data), " observations where QPS is non-degenerate..."))
  model <- lm(get(Y) ~ get(Z) + get(qps), data[!(get(qps) %in% c(0,1))])
  if (verbose == T){
    summary(model)
  }
  val <- data[,get(Y)] + model$coefficients[Z] * (data[,get(cf_ml_out)] - data[,get(ml_out)])
  val_score <- mean(val, na.rm=T)
  if (verbose == T){
    print(paste0("Counterfactual value of new ML function: ", val_score))
  }
  ret <- list(val, model)
  return(ret)
}
