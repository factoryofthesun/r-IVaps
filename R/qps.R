#' Main QPS estimation function.
#'
#' @param data Dataset containing ML input variables.
#' @param ML ML function for treatment recommendation
#' @param ml_type String indicating ML function type. See details for supported packages. Defaults to "stats".
#' @param Xc Character vector of column names of the continuous variables.
#' @param Xd Character vector of column names of the discrete variables.
#' @param S Number of draws for each QPS estimation. Defaults to 100.
#' @param delta Radius of sampling ball. Can be either numeric or numeric vector. Defaults to 0.8.
#' @param L Named list where the names correspond to the names of the mixed
#'   variables in the data, and the values are numeric vectors indicating the
#'   set of discrete values for the variable.
#' @param seed Random seed
#' @param fcn Function to apply to output of ML function
#' @param parallel Boolean indicator for whether to parallelize the QPS
#'   estimation.
#' @return
#' @examples
#' @details
#' ML packages currently supported: mlr3, caret, stats (base)
#'
#' Quasi-propensity score estimation involves taking draws \mjeqn{X_c^1,
#' \ldots,X_c^S}{X_c1, ..., X_cS} from the uniform distribution on
#' \mjeqn{N(X_{ci}, \delta)}, where \mjeqn{N(X_{ci},\delta)} is the \mjeqn{p_c}
#' dimensional ball centered at \mjeqn{X_{ci}} with radius \mjeqn{\delta}.
#' \mjeqn{X_c^1, \ldots,X_c^S} are destandardized before passed for ML
#' inference. The estimation equation is \mjeqn{p^s(X_i;\delta) = \frac{1}{S}
#' \sum_{s=1}^{S} ML(X_c^s, X_{di})}.
#'
#' If neither \code{Xc} nor \code{Xd} are passed then \code{data} is assumed to
#' be all relevant continuous inputs. If only one is passed, then the remaining
#' variables in \code{data} are assumed to be inputs of the other type. It is
#' recommended to pass both \code{Xc} and \code{Xd} in the case that not all the
#' variables in \code{data} are relevant for the ML input.
#' @export
estimate_qps <- function(data, ML, ml_type = NA, Xc = NA, Xd = NA, S = 100, delta = 0.8, L = NA, seed = NA, fcn = NA,
                         parallel = F, ...){
  data.table::setDT(data)
  if (all(is.na(Xc)) & all(is.na(Xd))){
    warning("In estimate_qps:\n Neither `Xc` nor `Xd` passed. Assuming all variables in `data` are continuous...")
    X_c <- data
    X_d <- NA
  } else if(all(is.na(Xc))){
    warning("In estimate_qps:\n `Xd` passed but `Xc` not passed. Assuming all remaining variables in `data` are continuous...")
    Xc <- setdiff(names(data), Xd)
    X_d <- data[, ..Xd]
    X_c <- data[, ..Xc]
  } else if(all(is.na(Xd))){
    warning("In estimate_qps:\n `Xc` passed but `Xd` not passed. Assuming all remaining variables in `data` are discrete...")
    Xd <- setdiff(names(data), Xc)
    X_d <- data[, ..Xd]
    X_c <- data[, ..Xc]
  } else{
    X_c <- data[, ..Xc]
    X_d <- data[, ..Xd]
  }

  # Preprocess mixed variables
  mixed_inds <- list()
  mixed_vals <- list()
  if (length(names(L)) != 0){
    for (var in names(L)){
      tmp_inds <- which(X_c[, get(var)] %in% L[[var]])
      tmp_vals <- X_c[tmp_inds, get(var)]
      mixed_inds[[var]] <- tmp_inds
      mixed_vals[[var]] <- tmp_vals
      X_c[tmp_inds, (var) := NA]
    }
  }

  # Standardized continuous variables
  cts_names <- names(X_c)
  mu <- X_c[, lapply(.SD, function(x) mean(x, na.rm=T))]
  sigma <- X_c[, lapply(.SD, function(x) sd(x, na.rm=T))]
  X_c[, (cts_names) := lapply(.SD, function(x) (x - mean(x,na.rm=T))/sd(x, na.rm=T)), .SDcols = cts_names]

  # Set seed
  if (!is.na(seed)){
    set.seed(seed)
  }

  # TODO: Parallelize???
  qps <- computeQPS(X_c, X_d, ML, ml_type, S, delta, mu, sigma, mixed_inds, mixed_vals, fcn, ...)
  return(qps)
}

computeQPS <- function(X_c, X_d, ML, ml_type, S, delta, mu, sigma, mixed_inds, mixed_vals, fcn, ...){
  draws <- drawQPS(X_c, X_d, S, delta, mu, sigma, mixed_inds, mixed_vals)
  nobs <- nrow(X_c)
  p_c <- length(names(X_c))

  # Run ML prediction depending on type of function
  X_d_long <- X_d[rep(X_d[,.I], S)]
  if (typeof(draws) == "list"){
    qps_list <- list()
    for (d in as.character(delta)){
      # Convert draws and discrete values back to data table
      tmp_draws <- aperm(draws[[d]], c(2,1,3))
      tmp_draws <- data.table::setDT(matrix(tmp_draws, S*nobs, p_c))
      setnames(tmp_draws, names(X_c))
      input <- cbind(tmp_draws, X_d_long)
      if (ml_type %in% c("mlr3", "caret", "stats")){ # All these packages extend 'predict' function
        pred_out <- predict(ML, input)
      }
      # Optional: additional post-processing function
      if (typeof(pred_out) != "logical"){
        pred_out <- fcn(pred_out)
      }
      # Avg every S values of prediction output
      qps <- .colMeans(pred_out, S, length(pred_out)/S)
      qps_list[[d]] <- qps
    }
    return(qps_list)
  } else{
    # Convert draws back to data table
    draws <- data.table::setDT(matrix(aperm(draws, c(2,1,3)), S*nobs, p_c))
    setnames(draws, names(X_c))
    input <- cbind(draws, X_d_long)
    if (ml_type %in% c("mlr3", "caret", "stats")){ # All these packages extend 'predict' function
      pred_out <- predict(ML, input)
    }
    # Optional: additional post-processing function
    if (typeof(pred_out) != "logical"){
      pred_out <- fcn(pred_out)
    }
    # Avg every S values of prediction output
    qps <- .colMeans(pred_out, S, length(pred_out)/S)
    return(qps)
  }
}

drawQPS <- function(X_c, X_d, S, delta, mu, sigma, mixed_inds, mixed_vals){
  nobs <- nrow(X_c)
  p_c <- length(X_c)
  draws <- array(rnorm(nobs * p_c * S), dim=c(nobs, S, p_c), dimnames=list(1:nobs, 1:S, names(X_c)))
  u_draws <- array(runif(nobs * S), dim=c(nobs, S))
  multi_delta <- F

  # Set relevant obs to NA
  for (var in names(mixed_inds)){
    draws[mixed_inds[[var]],,var] <- NA
  }

  # Row-wise scale draws
  sd <- apply(draws, 1, function(x) sqrt(sum(x^2, na.rm=T)))
  draws <- draws/sd

  # Set u -- check for all NA rows
  na_counts <- as.array(apply(X_c, 1, function(x) sum(is.na(x))))
  ct <- p_c - na_counts
  u <- u_draws
  for (i in 1:nrow(u)){
    if (ct[i] == 0){
      u[i,] <- NA
    }
    u[i,] <- u[i,]^(1/ct[i])
  }
  u <- array(rep(u, p_c), dim=c(nobs, S, p_c)) # Increase dimension to be conformable with draws

  if (length(delta) > 1){ # Loop through deltas
    draws_list <- list()
    for (d in delta){
      tmp_draws <- draws
      tmp_draws <- tmp_draws * u * d + array(unlist(rep(X_c, each=S)), dim=c(nobs, S, p_c))
      tmp_draws <- tmp_draws*array(unlist(rep(sigma, each=S*nobs)), dim=c(nobs, S, p_c)) +
        array(unlist(rep(mu, each=S*nobs)), dim=c(nobs, S, p_c))
      for (var in names(mixed_inds)){
        tmp_draws[mixed_inds[[var]],,var] <- mixed_vals[[var]]
      }
      draws_list[[as.character(d)]] <- tmp_draws
    }
    return(draws_list)
  } else{
    # draws from uniform ball centered at standardized Xc
    draws <- draws * u * delta + array(unlist(rep(X_c, each=S)), dim=c(nobs, S, p_c))
    # draws from ball centered at original Xc
    draws <- draws*array(unlist(rep(sigma, each=S*nobs)), dim=c(nobs, S, p_c)) +
      array(unlist(rep(mu, each=S*nobs)), dim=c(nobs, S, p_c))
    # add back discrete values
    for (var in names(mixed_inds)){
      draws[mixed_inds[[var]],,var] <- mixed_vals[[var]]
    }
    return(draws)
  }
}


