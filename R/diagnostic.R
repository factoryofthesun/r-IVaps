#' Covariate Balance Test
#'
#' @param data Dataset containing ML input variables X, treatment assignment Z, and qps.
#' @param qps Column name of QPS variable. Defaults to "QPS".
#' @param X Character vector of column names of ML inputs X. Defaults to c("X1").
#' @param Z Column name of Z variable. Defaults to "Z".
#' @param verbose Boolean indicator for whether to print summary output of estimation. Defaults to True.
#' @return List containing fitted multivariate multiple linear model and the joint hypothesis test.
#' @examples
#' @details
#' This function estimates a multivariate multiple regression system, varying each of the ML input variables
#' in \code{X} against the common covariates \code{qps} and \code{Z}. The covariate balance test reports the
#' results of each individual regression, as well as the joint hypothesis result for the coefficient on \code{Z}.
#' This helps to establish whether \code{qps} performs adequately as a control for differences created through
#' treatment selection.
#' @export
covariate_balance_test <- function(data, qps = "QPS", X = c("X1"), Z = "Z", verbose=T){
  dep <- c()
  for (var in X){
    assign(var, data[,get(var)])
    dep <- cbind(dep, get(var))
  }
  colnames(dep) <- X
  Z <- data[,get(Z)]
  qps <- data[,get(qps)]
  mlm <- lm(dep ~ Z + qps)
  if (requireNamespace("car", quietly=T)){
    lh.out <- car::linearHypothesis(mlm, hypothesis.matrix=c("Z = 0"))
  } else{ # TODO: manual implementation of MANOVA hypothesis test
    lh.out <- NA
  }
  if (verbose == T){
    summary(mlm)
    summary(lh.out)
  }
  return(list(model = mlm, test = lh.out))
}
