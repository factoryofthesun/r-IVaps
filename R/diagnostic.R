#' Covariate Balance Test
#'
#' @param data Dataset containing ML input variables X, treatment assignment Z, and aps.
#' @param aps Vector of APS values.
#' @param X Object containing ML inputs X.
#' @param Z Vector of ML recommendation values Z.
#' @param degen Vector of values for which APS is degenerate. Defaults to c(0,1).
#' @param apslab Column name of APS variable. Defaults to "APS".
#' @param xlab Character vector of column names of ML inputs X. Defaults to c("X1").
#' @param zlab Column name of Z variable. Defaults to "Z".
#' @param verbose Boolean indicator for whether to print summary output of estimation. Defaults to True.
#' @return List containing fitted multivariate multiple linear model and the results of the joint hypothesis test,
#' including the F statistic and p-value of the Pillai's trace statistic.
#' @examples
#' # Iris data
#' data("iris")
#' assign_cutoff <- function(X, cutoff){
#'   ret <- as.integer(X > cutoff)
#'   return(ret)
#' }
#' model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=iris)
#' aps <- estimate_aps(iris, model, xc = names(iris)[2:4], infer=FALSE, s=400,
#'         delta=0.8, fcn=assign_cutoff, cutoff = 6)
#' Z <- assign_cutoff(iris$Sepal.Length, 6)
#' X <- iris[,2:4]
#' iris[, APS := aps]
#' iris[, Z := Z]
#' # Two ways of sending inputs
#' out_direct <- covariate_balance_test(aps = aps, X = iris[, 2:4], Z = Z)
#' out_indirect <- covariate_balance_test(data = iris, apslab = "APS",
#'            xlab = names(iris)[2:4])
#' @details
#' If the primary data vectors (aps, X, Z) are not passed, then the fallback is to search for the
#' `_lab` variables in the \code{data} object.
#'
#' This function estimates a multivariate multiple regression system, varying each of the ML input variables
#' in \code{X} against the common covariates \code{APS} and \code{Z}, where **\code{APS} is non-degenerate**.
#' The covariate balance test reports the
#' results of each individual regression, as well as the joint hypothesis result for the coefficient on \code{Z}.
#' This helps to establish whether \code{APS} performs adequately as a control for differences created through
#' treatment selection. The regression system is estimated on the sample for which \code{APS} is non-degenerate.
#'
#' @export
covariate_balance_test <- function(data = NULL, aps = NULL, X = NULL, Z = NULL, degen=c(0,1),
                                   apslab = "APS", xlab = c("X1"), zlab = "Z", verbose=T){
  if (is.null(data) & (is.null(aps) | is.null(X) | is.null(Z))){
    stop("If not passing any of the individual data vectors, then `data` object must be passed.")
  } else if (is.null(data)){
    X <- as.matrix(X)
    if (length(xlab) == length(names(X))){
      setnames(X, xlab)
    }
  } else{
    data.table::setDT(data)
    if (!is.null(X)){
      X <- as.matrix(X)
      if (length(xlab) == length(names(X))){
        setnames(X, xlab)
      }
    } else {
      X <- as.matrix(data[, xlab, with = F])
    }
    if (is.null(aps)){
      aps <- data[, get(apslab)]
    }
    if (is.null(Z)){
      Z <- data[, get(zlab)]
    }
  }
  keep_inds <- which(!(aps %in% degen))
  aps <- aps[keep_inds]
  X <- X[keep_inds,]
  Z <- Z[keep_inds]
  mlm <- lm(X ~ Z + aps)
  if (requireNamespace("car", quietly=T)){
    lh.out <- car::linearHypothesis(mlm, hypothesis.matrix=c("Z = 0"))
    # Derive F statistic and p-values
    H <- lh.out$SSPH
    E <- lh.out$SSPE
    h <- lh.out$df
    e <- lh.out$df.residual
    p <- lh.out$r
    s <- min(p, h)
    m <- (abs(p-h) - 1)/2
    n <- (e - p - 1)/2
    V <- sum(diag(H %*% solve(E + H)))
    F <- (2 * n + s + 1) * V/((2 * m + s + 1) * (s - V))
    p <- 1 - pf(F, s * (2 * m + s + 1), s*(2 * n + s + 1))
  } else{ # TODO: manual implementation of MANOVA hypothesis test
    lh.out <- NA
    stop("`car` package required for this function.")
  }
  if (verbose == T){
    print(summary(mlm))
    print(lh.out)
  }
  return(list(model = mlm, test = lh.out, F_stat = F, p_val = p))
}
