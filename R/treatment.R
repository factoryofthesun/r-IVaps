#' Main treatment effect estimation function
#'
#' @param data Dataset containing APS, Y, Z, and D data.
#' @param aps Vector of APS values.
#' @param Y Vector of outcome values.
#' @param Z Vector of ML recommendation values.
#' @param D Vector of treatment assignment values.
#' @param degen Vector of values for which APS is degenerate. Defaults to c(0,1).
#' @param apslab Column name of APS variable. Defaults to "APS".
#' @param ylab Column name of Y variable. Defaults to "Y".
#' @param zlab Column name of Z variable. Defaults to "Z".
#' @param dlab Column name of D variable. Defaults to "D".
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
#' aps <- estimate_aps(iris, model, xc = names(iris)[2:4],
#'           infer=FALSE, s=400, delta=0.8, fcn=assign_cutoff, cutoff=6)
#'
#' # Can send treatment effect inputs in two different ways
#' Y_val <- iris$Sepal.Length
#' Z_val <- assign_cutoff(iris$Sepal.Length, 6)
#' D_val <- predict(model, iris)
#' iris[, Y := Y_val]
#' iris[, Z := Z_val]
#' iris[, D := D_val]
#' iris[, APS := aps]
#' estimate_treatment_effect(aps = aps, Y = Y_val, Z = Z_val, D = D_val)
#' estimate_treatment_effect(iris, apslab = "APS", ylab = "Sepal.Length", zlab = "Z",
#'                           dlab = "D")
#' @details
#' If the primary data vectors (aps, Y, Z, D) are not passed, then the fallback is to search for the
#' `_lab` variables in the \code{data} object.
#'
#' Treatment effect is estimated using IV estimation The default is to use the 2SLS method
#' of estimation, with the equations illustrated below.
#' \deqn{D_i = \gamma_0(1-I) + \gamma_1 Z_i + \gamma_2 p^s(X_i;\delta) + v_i}
#' \deqn{Y_i = \beta_0(1-I) + \beta_1 D_i + \beta_2 p^s(X_i;\delta) + \epsilon_i}
#' \eqn{\beta_1} is our causal estimation of the treatment effect. I is an indicator for if the ML funtion takes only a single
#' nondegenerate value in the sample.
#' @export
estimate_treatment_effect <- function(data = NULL, aps = NULL, Y = NULL, Z = NULL, D = NULL, degen=c(0,1),
                                      apslab ="APS", ylab = "Y", zlab = "Z", dlab = "D", estimator = "2SLS",
                                      verbose=T){
  if (is.null(data) & (is.null(aps) | is.null(Y) | is.null(Z) | is.null(D))){
    stop("If not passing any of the individual data vectors, then `data` object must be passed.")
  } else if (is.null(data)){
    APS <- aps
  } else{
    data.table::setDT(data)
    if (is.null(aps) & apslab %in% names(data)){
      APS <- data[,get(apslab)]
    } else if (!is.null(aps)){
      APS <- aps
    } else {
      stop(paste0(apslab, " not found in data."))
    }
    if (is.null(Y) & ylab %in% names(data)){
      Y <- data[,get(ylab)]
    } else if (!is.null(Y)){
    } else {
      stop(paste0(ylab, " not found in data."))
    }
    if (is.null(Z) & zlab %in% names(data)){
      Z <- data[,get(zlab)]
    } else if (!is.null(Z)){
    } else {
      stop(paste0(zlab, " not found in data."))
    }
    if (is.null(D) & dlab %in% names(data)){
      D <- data[,get(dlab)]
    } else if (!is.null(D)){
    } else {
      stop(paste0(dlab, " not found in data."))
    }
  }
  # Keep only values with non-degenerate APS
  og_length <- length(APS)
  keep_inds <- which(!(APS %in% degen))
  APS <- APS[keep_inds]
  Y <- Y[keep_inds]
  Z <- Z[keep_inds]
  D <- D[keep_inds]
  if (verbose == T){
    print(paste0("Estimating counterfactual value on ", length(keep_inds), " out of ",
                 og_length, " observations where APS is non-degenerate..."))
  }
  data <- data[!(get(apslab) %in% degen)]
  model <- ivreg::ivreg(Y ~ D + APS|Z + APS)
  if (verbose == T){
    print(summary(model))
  }
  return(model)
}

#' Estimate counterfactual performance of another algorithm
#'
#' @param data Dataset containing APS, Y, Z, the original ML algorithm predictions,
#' and the counterfactual ML' algorithm predictions.
#' @param aps Vector of APS values.
#' @param Y Vector of outcome values.
#' @param Z Vector of ML recommendation values.
#' @param ml Vector of ML predictions.
#' @param mlnew Vector of counterfactual ML predictions.
#' @param degen Vector of values for which APS is degenerate. Defaults to c(0,1).
#' @param apslab Column name of APS variable. Defaults to "APS".
#' @param ylab Column name of Y variable. Defaults to "Y".
#' @param zlab Column name of Z variable. Defaults to "Z".
#' @param mllab Column name of original ML prediction variable. Defaults to "ML1".
#' @param mlnewlab Column name of counterfactual ML prediction variable. Defaults to "ML2".
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
#' ml <- predict(model, test_data)
#' task_iris <- TaskRegr$new(id="iris", backend=test_data[,1:4], target="Sepal.Length")
#' learner = lrn("regr.rpart")
#' learner$train(task_iris)
#' mlnew <- predict(learner, test_data)
#' aps <- estimate_aps(test_data, model, xc = names(test_data)[2:4],
#'            infer=FALSE, s=400, delta=0.8, fcn=assign_cutoff, cutoff = 6)
#' # Can send counterfactual estimation inputs in two different ways
#' Y_val <- test_data$Sepal.Length
#' Z_val <- assign_cutoff(iris$Sepal.Length, 6)
#' test_data[, Y := Y_val]
#' test_data[, Z := Z_val]
#' test_data[, ml := ml]
#' test_data[, ml_new := mlnew]
#' test_data[, APS := aps]
#' estimate_counterfactual_ml(aps = aps, Y = Y_val, Z = Z_val, ml = ml, mlnew = mlnew)
#' estimate_counterfactual_ml(test_data, apslab = "APS", ylab = "Y", zlab = "Z",
#'                       mllab = "ml", mlnewlab = "ml_new")
#' @details
#' The process of estimating counterfactual value works as follows. First we we fit the
#' below OLS regression using historical recommendations and outcome \code{Z} and \code{Y}.
#' \deqn{Y_i = \beta_0 + \beta_1 Z_i + \beta_2 p^s(X_i; \delta) + \epsilon_i}
#' where \eqn{\beta_1} is our estimated effect of treatment recommendation.
#'
#' Then we take the original algorithm output \code{ml} and the counterfactual algorithm output \code{mlnew}
#' and estimate the below value equation.
#' \deqn{\hat{V}(ML') = \frac{1}{n} \sum_{i=1}^n (Y_i + \hat{\beta_{ols}}(ML'(X_i) - ML(X_i)))}
#' @export
estimate_counterfactual_ml <- function(data = NULL, aps = NULL, Y = NULL, Z = NULL, ml = NULL, mlnew = NULL,
                                       degen=c(0,1), apslab = "APS", ylab = "Y", zlab = "Z",
                                       mllab = "ML1", mlnewlab ="ML2", verbose=T){
  if (is.null(data) & (is.null(aps) | is.null(Y) | is.null(Z) | is.null(D) | is.null(ml) | is.null(mlnew))){
    stop("If not passing any of the individual data vectors, then `data` object must be passed.")
  } else if (is.null(data)){
    APS <- aps
  } else{
    data.table::setDT(data)
    if (is.null(aps) & apslab %in% names(data)){
      APS <- data[,get(apslab)]
    } else if (!is.null(aps)){
      APS <- aps
    } else {
      stop(paste0(apslab, " not found in data."))
    }
    if (is.null(Y) & ylab %in% names(data)){
      Y <- data[,get(ylab)]
    } else if (!is.null(Y)){
    } else {
      stop(paste0(ylab, " not found in data."))
    }
    if (is.null(Z) & zlab %in% names(data)){
      Z <- data[,get(zlab)]
    } else if (!is.null(Z)){
    } else {
      stop(paste0(zlab, " not found in data."))
    }
    if (is.null(ml) & mllab %in% names(data)){
      ml <- data[,get(mllab)]
    } else if (!is.null(ml)){
    } else {
      stop(paste0(mllab, " not found in data."))
    }
    if (is.null(mlnew) & mlnewlab %in% names(data)){
      mlnew <- data[,get(mlnewlab)]
    } else if (!is.null(mlnew)){
    } else {
      stop(paste0(mlnewlab, " not found in data."))
    }
  }
  keep_inds <- which(!(APS %in% degen))
  Y_og <- Y
  APS <- APS[keep_inds]
  Y <- Y[keep_inds]
  Z <- Z[keep_inds]
  if (verbose == T){
    print(paste0("Estimating counterfactual value on ", length(keep_inds), " out of ",
                 length(APS), " observations where APS is non-degenerate..."))
  }
  model <- lm(Y ~ Z + APS)
  if (verbose == T){
    print(summary(model))
  }
  val <- Y_og + model$coefficients["Z"] * (mlnew - ml)
  val_score <- mean(val, na.rm=T)
  if (verbose == T){
    print(paste0("Counterfactual value of new ML function: ", val_score))
  }
  ret <- list(value = val, model = model)
  return(ret)
}
