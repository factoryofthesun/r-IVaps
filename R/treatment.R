#' Main treatment effect estimation function
#'
#' @param data Dataset containing QPS, Y, Z, and D data.
#' @param qps Vector of QPS values.
#' @param Y Vector of outcome values.
#' @param Z Vector of ML recommendation values.
#' @param D Vector of treatment assignment values.
#' @param degen Vector of values for which QPS is degenerate. Defaults to c(0,1).
#' @param qps_lab Column name of QPS variable. Defaults to "QPS".
#' @param Y_lab Column name of Y variable. Defaults to "Y".
#' @param Z_lab Column name of Z variable. Defaults to "Z".
#' @param D_lab Column name of D variable. Defaults to "D".
#' @param estimator String method of IV estimation. Defaults to "2SLS".
#' @param verbose Boolean indicator for whether to print summary output of estimation. Defaults to True.
#' @return Fitted ivreg model object.
#' @examples
#' # Iris data
#' data("iris")
#' assign_cutoff <- function(X, cutoff){
#'   ret <- as.integer(X > cutoff)
#'   return(ret)
#' }
#' model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=iris)
#' qps <- estimate_qps(iris, model, Xc = names(iris)[2:4],
#'           infer=FALSE, S=400, delta=0.8, fcn=assign_cutoff, cutoff=6)
#'
#' # Can send treatment effect inputs in two different ways
#' Y_val <- iris$Sepal.Length
#' Z_val <- assign_cutoff(iris$Sepal.Length, 6)
#' D_val <- predict(model, iris)
#' iris[, Y := Y_val]
#' iris[, Z := Z_val]
#' iris[, D := D_val]
#' iris[, QPS := qps]
#' estimate_treatment_effect(qps = qps, Y = Y_val, Z = Z_val, D = D_val)
#' estimate_treatment_effect(iris, qps_lab = "QPS", Y_lab = "Sepal.Length", Z_lab = "Z",
#'                           D_lab = "D")
#' @details
#' If the primary data vectors (qps, Y, Z, D) are not passed, then the fallback is to search for the
#' `_lab` variables in the \code{data} object.
#'
#' Treatment effect is estimated using IV estimation The default is to use the 2SLS method
#' of estimation, with the equations illustrated below.
#' \deqn{D_i = \gamma_0(1-I) + \gamma_1 Z_i + \gamma_2 p^s(X_i;\delta) + v_i}
#' \deqn{Y_i = \beta_0(1-I) + \beta_1 D_i + \beta_2 p^s(X_i;\delta) + \epsilon_i}
#' \eqn{\beta_1} is our causal estimation of the treatment effect. I is an indicator for if the ML funtion takes only a single
#' nondegenerate value in the sample.
#' @export
estimate_treatment_effect <- function(data = NULL, qps = NULL, Y = NULL, Z = NULL, D = NULL, degen=c(0,1),
                                      qps_lab ="QPS", Y_lab = "Y", Z_lab = "Z", D_lab = "D", estimator = "2SLS",
                                      verbose=T){
  if (is.null(data) & (is.null(qps) | is.null(Y) | is.null(Z) | is.null(D))){
    stop("If not passing any of the individual data vectors, then `data` object must be passed.")
  } else if (is.null(data)){
    QPS <- qps
  } else{
    data.table::setDT(data)
    if (is.null(qps) & qps_lab %in% names(data)){
      QPS <- data[,get(qps_lab)]
    } else if (!is.null(qps)){
      QPS <- qps
    } else {
      stop(paste0(qps_lab, " not found in data."))
    }
    if (is.null(Y) & Y_lab %in% names(data)){
      Y <- data[,get(Y_lab)]
    } else if (!is.null(Y)){
    } else {
      stop(paste0(Y_lab, " not found in data."))
    }
    if (is.null(Z) & Z_lab %in% names(data)){
      Z <- data[,get(Z_lab)]
    } else if (!is.null(Z)){
    } else {
      stop(paste0(Z_lab, " not found in data."))
    }
    if (is.null(D) & D_lab %in% names(data)){
      D <- data[,get(D_lab)]
    } else if (!is.null(D)){
    } else {
      stop(paste0(D_lab, " not found in data."))
    }
  }
  # Keep only values with non-degenerate QPS
  og_length <- length(QPS)
  keep_inds <- which(!(QPS %in% degen))
  QPS <- QPS[keep_inds]
  Y <- Y[keep_inds]
  Z <- Z[keep_inds]
  D <- D[keep_inds]
  if (verbose == T){
    print(paste0("Estimating counterfactual value on ", length(keep_inds), " out of ",
                 og_length, " observations where QPS is non-degenerate..."))
  }
  data <- data[!(get(qps_lab) %in% degen)]
  model <- ivreg::ivreg(Y ~ D + QPS|Z + QPS)
  if (verbose == T){
    print(summary(model))
  }
  return(model)
}

#' Estimate counterfactual performance of another algorithm
#'
#' @param data Dataset containing QPS, Y, Z, the original ML algorithm predictions,
#' and the counterfactual ML' algorithm predictions.
#' @param qps Vector of QPS values.
#' @param Y Vector of outcome values.
#' @param Z Vector of ML recommendation values.
#' @param ML Vector of ML predictions.
#' @param ML_new Vector of counterfactual ML predictions.
#' @param degen Vector of values for which QPS is degenerate. Defaults to c(0,1).
#' @param qps_lab Column name of QPS variable. Defaults to "QPS".
#' @param Y_lab Column name of Y variable. Defaults to "Y".
#' @param Z_lab Column name of Z variable. Defaults to "Z".
#' @param ML_lab Column name of original ML prediction variable. Defaults to "ML1".
#' @param ML_new_lab Column name of counterfactual ML prediction variable. Defaults to "ML2".
#' @param verbose Boolean indicator for whether to print summary output of estimation. Defaults to True.
#' @return List containing counterfactual predictions and fitted lm model object.
#' @examples
#' library(mlr3)
#' data("iris")
#' assign_cutoff <- function(X, cutoff){
#'   ret <- as.integer(X > cutoff)
#'   return(ret)
#' }
#' test_data <- iris
#' model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=test_data)
#' ML <- predict(model, test_data)
#' task_iris <- TaskRegr$new(id="iris", backend=test_data[,1:4], target="Sepal.Length")
#' learner = lrn("regr.rpart")
#' learner$train(task_iris)
#' ML_new <- predict(learner, test_data)
#' qps <- estimate_qps(test_data, model, Xc = names(test_data)[2:4],
#'            infer=FALSE, S=400, delta=0.8, fcn=assign_cutoff, cutoff = 6)
#' # Can send counterfactual estimation inputs in two different ways
#' Y_val <- test_data$Sepal.Length
#' Z_val <- assign_cutoff(iris$Sepal.Length, 6)
#' test_data[, Y := Y_val]
#' test_data[, Z := Z_val]
#' test_data[, ml := ML]
#' test_data[, ml_new := ML_new]
#' test_data[, QPS := qps]
#' estimate_counterfactual_ml(qps = qps, Y = Y_val, Z = Z_val, ML = ML, ML_new = ML_new)
#' estimate_counterfactual_ml(test_data, qps_lab = "QPS", Y_lab = "Y", Z_lab = "Z",
#'                       ML_lab = "ml", ML_new_lab = "ml_new")
#' @details
#' The process of estimating counterfactual value works as follows. First we we fit the
#' below OLS regression using historical recommendations and outcome \code{Z} and \code{Y}.
#' \deqn{Y_i = \beta_0 + \beta_1 Z_i + \beta_2 p^s(X_i; \delta) + \epsilon_i}
#' where \eqn{\beta_1} is our estimated effect of treatment recommendation.
#'
#' Then we take the original algorithm output \code{ML} and the counterfactual algorithm output \code{ML_new}
#' and estimate the below value equation.
#' \deqn{\hat{V}(ML') = \frac{1}{n} \sum_{i=1}^n (Y_i + \hat{\beta_{ols}}(ML'(X_i) - ML(X_i)))}
#' @export
estimate_counterfactual_ml <- function(data = NULL, qps = NULL, Y = NULL, Z = NULL, ML = NULL, ML_new = NULL,
                                       degen=c(0,1), qps_lab = "QPS", Y_lab = "Y", Z_lab = "Z",
                                       ML_lab = "ML1", ML_new_lab ="ML2", verbose=T){
  if (is.null(data) & (is.null(qps) | is.null(Y) | is.null(Z) | is.null(D) | is.null(ML) | is.null(ML_new))){
    stop("If not passing any of the individual data vectors, then `data` object must be passed.")
  } else if (is.null(data)){
    QPS <- qps
  } else{
    data.table::setDT(data)
    if (is.null(qps) & qps_lab %in% names(data)){
      QPS <- data[,get(qps_lab)]
    } else if (!is.null(qps)){
      QPS <- qps
    } else {
      stop(paste0(qps_lab, " not found in data."))
    }
    if (is.null(Y) & Y_lab %in% names(data)){
      Y <- data[,get(Y_lab)]
    } else if (!is.null(Y)){
    } else {
      stop(paste0(Y_lab, " not found in data."))
    }
    if (is.null(Z) & Z_lab %in% names(data)){
      Z <- data[,get(Z_lab)]
    } else if (!is.null(Z)){
    } else {
      stop(paste0(Z_lab, " not found in data."))
    }
    if (is.null(ML) & ML_lab %in% names(data)){
      ML <- data[,get(ML_lab)]
    } else if (!is.null(ML)){
    } else {
      stop(paste0(ML_lab, " not found in data."))
    }
    if (is.null(ML_new) & ML_new_lab %in% names(data)){
      ML_new <- data[,get(ML_new_lab)]
    } else if (!is.null(ML_new)){
    } else {
      stop(paste0(ML_new_lab, " not found in data."))
    }
  }
  keep_inds <- which(!(QPS %in% degen))
  Y_og <- Y
  QPS <- QPS[keep_inds]
  Y <- Y[keep_inds]
  Z <- Z[keep_inds]
  if (verbose == T){
    print(paste0("Estimating counterfactual value on ", length(keep_inds), " out of ",
                 length(QPS), " observations where QPS is non-degenerate..."))
  }
  model <- lm(Y ~ Z + QPS)
  if (verbose == T){
    print(summary(model))
  }
  val <- Y_og + model$coefficients["Z"] * (ML_new - ML)
  val_score <- mean(val, na.rm=T)
  if (verbose == T){
    print(paste0("Counterfactual value of new ML function: ", val_score))
  }
  ret <- list(value = val, model = model)
  return(ret)
}
