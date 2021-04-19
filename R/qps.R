.datatable.aware=T

#' Main QPS estimation function.
#' @import data.table
#' @param data Dataset containing ML input variables.
#' @param ML ML function for treatment recommendation
#' @param ml_type String indicating the ML object source package name, or "custom" for a user-defined
#' algorithm that runs prediction upon function call with a single input data.table object.
#' See details for supported packages. Defaults to "stats".
#' @param Xc Character vector of column names of the continuous variables.
#' @param Xd Character vector of column names of the discrete variables.
#' @param infer Boolean whether to infer continuous/discrete variables from remaining columns of data. Defaults to False.
#' @param S Number of draws for each QPS estimation. Defaults to 100.
#' @param delta Radius of sampling ball. Can be either numeric or numeric vector. Defaults to 0.8.
#' @param L Named list where the names correspond to the names of the mixed
#'   variables in the data, and the values are numeric vectors indicating the
#'   set of discrete values for the variable.
#' @param seed Random seed
#' @param fcn Function to apply to output of ML function
#' @param parallel Boolean indicator for whether to parallelize the QPS
#'   estimation. Defaults to FALSE.
#' @param cores Integer number of cores for parallelization. If NA, then detectCores() is called.
#' @param \dots Additional inputs to be passed to \code{fcn}
#' @return If a single \code{delta} value is passed, then the function returns a vector of
#' estimated Quasi-Propensity Scores of the same length as the input data. If multiple
#' \code{delta} are passed, then a list of estimated QPS vectors are returned, where the
#' keys are each \code{delta} value.
#'
#' @examples
#' # Iris examples
#' model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=iris)
#' estimate_qps(iris, model, Xc = names(iris)[2:3], Xd = names(iris)[4],
#'             infer=FALSE, S=50, delta=0.1)
#'
#' # Estimate QPS while applying decision function assign_cutoff to model output with input cutoff=0.5
#' assign_cutoff <- function(X, cutoff){
#'   ret <- as.integer(X > cutoff)
#'   return(ret)
#' }
#' estimate_qps(iris, model, Xc = names(iris)[2:3], Xd = names(iris)[4],
#'              infer=FALSE, S=50, delta=0.1, fcn=assign_cutoff, cutoff=0.5)
#'
#' # Multiple deltas
#' estimate_qps(iris, model, Xc = names(iris)[2:3], Xd = names(iris)[4],
#'              infer=FALSE, S=50, delta=c(0.1,0.5,1))
#'
#' # Define mixed continuous/discrete variables
#' estimate_qps(iris, model, Xc = names(iris)[2:3], Xd = names(iris)[4],
#'              infer=FALSE, S=50, delta=0.1,
#'              L = list("Sepal.Width" = c(2, 3), "Petal.Length" = c(3, 4)))
#' @details
#' ML packages currently supported:
#' "mlr3", "caret", "stats" (base), "randomForest", "e1071", "bestridge", "rpart", "tree", "custom"
#'
#' Quasi-propensity score estimation involves taking draws \eqn{X_c^1,\ldots,X_c^S}
#' from the uniform distribution on
#' \eqn{N(X_{ci}, \delta)}, where \eqn{N(X_{ci},\delta)} is the
#' \eqn{p_c} dimensional ball centered at \eqn{X_{ci}} with radius \eqn{\delta}.
#' \eqn{X_c^1, \ldots,X_c^S} are destandardized before passed for ML
#' inference. The estimation equation is \eqn{p^s(X_i;\delta) = \frac{1}{S}\sum_{s=1}^{S} ML(X_c^s, X_{di})}.
#'
#' If neither \code{Xc} nor \code{Xd} are passed then \code{data} is assumed to
#' be all relevant continuous inputs. If only one is passed, then the remaining
#' variables in \code{data} are assumed to be inputs of the other type. It is
#' recommended to pass both \code{Xc} and \code{Xd} in the case that not all the
#' variables in \code{data} are relevant for the ML input.
#' @export
estimate_qps <- function(data, ML, ml_type = "stats", Xc = NA, Xd = NA, infer=FALSE, S = 100, delta = 0.8,
                         L = NA, seed = NA, fcn = NA, parallel = F, cores = NA, ...){
  data.table::setDT(data)
  if (infer == T){
    if (all(is.na(Xc)) & all(is.na(Xd))){
      warning("(Inference on) Neither `Xc` nor `Xd` passed. Assuming all variables in `data` are continuous...")
      X_c <- data
      X_d <- NA
    } else if(all(is.na(Xc))){
      warning("(Inference on) `Xd` passed but `Xc` not passed. Assuming all remaining variables in `data` are continuous...")
      Xc <- setdiff(names(data), Xd)
      X_d <- data[, ..Xd]
      X_c <- data[, ..Xc]
      if (length(X_c) < 1){ # If no cts columns then raise error
        stop(paste0("No continuous variables left in input data after removing discrete.",
                    " QPS estimation requires at least one continuous variable."))
      }
    } else if(all(is.na(Xd))){
      warning("(Inference on) `Xc` passed but `Xd` not passed. Assuming all remaining variables in `data` are discrete...")
      Xd <- setdiff(names(data), Xc)
      X_d <- data[, ..Xd]
      X_c <- data[, ..Xc]
    } else{
      X_c <- data[, ..Xc]
      X_d <- data[, ..Xd]
    }
  } else{
    if (all(is.na(Xc))|length(Xc) == 0){
      stop(paste0("No continuous variables passed and variable inference is off.",
                  " QPS estimation requres at least one continuous variable."))
    }
    X_c <- data[,..Xc]
    X_d <- NA
    if (!all(is.na(Xd)) & length(Xd) > 0){
      X_d <- data[,..Xd]
    }
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
  mu <- unlist(X_c[, lapply(.SD, function(x) mean(x, na.rm=T))])
  sigma <- unlist(X_c[, lapply(.SD, function(x) sd(x, na.rm=T))])
  X_c[, (cts_names) := lapply(.SD, function(x) (x - mean(x,na.rm=T))/sd(x, na.rm=T)), .SDcols = cts_names]

  # Set seed
  if (!is.na(seed)){
    set.seed(seed, kind = "L'Ecuyer-CMRG")
  }
  qps <- computeQPS(X_c, X_d, ML, ml_type, S, delta, mu, sigma, mixed_inds, mixed_vals, fcn, parallel,
                    cores, ...)
  return(qps)
}

computeQPS <- function(X_c, X_d, ML, ml_type, S, delta, mu, sigma, mixed_inds, mixed_vals, fcn,
                       parallel, cores, ...){
  draws <- drawQPS(X_c, X_d, S, delta, mu, sigma, mixed_inds, mixed_vals, parallel, cores)
  nobs <- nrow(X_c)
  p_c <- length(names(X_c))

  # Run ML prediction depending on type of function
  if (!all(is.na(X_d)) & length(X_d) > 0){
    X_d_long <- X_d[rep(X_d[,.I], each=S)]
  } else{
    X_d_long <- NA
  }
  if (typeof(draws) == "list"){
    qps_list <- list()
    for (d in as.character(delta)){
      # Convert draws and discrete values back to data table
      tmp_draws <- aperm(draws[[d]], c(2,1,3))
      tmp_draws <- data.table::as.data.table(matrix(tmp_draws, S*nobs, p_c))
      data.table::setnames(tmp_draws, names(X_c))
      if (!all(is.na(X_d_long))){
        input <- cbind(tmp_draws, X_d_long)
      } else{
        input <- tmp_draws
      }
      if (ml_type %in% c("mlr3", "caret", "stats", "randomForest", "e1071", "bestridge")){ # All these packages extend 'predict' function
        pred_out <- predict(ML, input)
      } else if (ml_type %in% c("rpart", "tree")){
        pred_out <- predict(ML, input, type="vector")
      } else if (ml_type == "custom"){
          pred_out <- ML(input)
        } else {
        message(paste0("Package ", ml_type, " not found in internal library. Attempting to apply `predict()` syntax..."))
        pred_out <- tryCatch({
          predict(ML, input)
        },
        error=function(cond){
          message(paste0("Function call raised error: ", cond))
          return(NA)
        },
        warning=function(cond){
          message(paste0("Warning: ", cond))
          return(NULL)
        })
      }
      # Optional: additional post-processing function
      if (typeof(fcn) != "logical"){
        pred_out <- fcn(pred_out, ...)
      }
      # Avg every S values of prediction output
      qps <- .colMeans(pred_out, S, length(pred_out)/S, na.rm=T)
      qps_list[[d]] <- qps
    }
    return(qps_list)
  } else{
    # Convert draws back to data table
    draws <- data.table::as.data.table(matrix(aperm(draws, c(2,1,3)), S*nobs, p_c))
    data.table::setnames(draws, names(X_c))
    if (!all(is.na(X_d_long))){
      input <- cbind(draws, X_d_long)
    } else{
      input <- draws
    }
    if (ml_type %in% c("mlr3", "caret", "stats", "randomForest", "e1071", "bestridge")){ # All these packages extend 'predict' function
      pred_out <- predict(ML, input)
    } else if (ml_type %in% c("rpart", "tree")){
      pred_out <- predict(ML, input, type="vector")
    } else if (ml_type == "custom"){
      pred_out <- ML(input)
    } else {
      message(paste0("Package ", ml_type, " not found in internal library. Attempting to apply `predict()` syntax..."))
      pred_out <- tryCatch({
        predict(ML, input)
      },
      error=function(cond){
        message(paste0("Function call raised error: ", cond))
        return(NA)
      },
      warning=function(cond){
        message(paste0("Warning: ", cond))
        return(NULL)
      })
    }
    # Optional: additional post-processing function
    if (typeof(fcn) != "logical"){
      pred_out <- fcn(pred_out, ...)
    }
    # Avg every S values of prediction output
    qps <- .colMeans(pred_out, S, length(pred_out)/S, na.rm=T)
    return(qps)
  }
}

drawQPS <- function(X_c, X_d, S, delta, mu, sigma, mixed_inds, mixed_vals, parallel, cores){
  nobs <- nrow(X_c)
  p_c <- length(X_c)
  draws <- array(rnorm(nobs * p_c * S), dim=c(nobs, S, p_c), dimnames=list(1:nobs, 1:S, names(X_c)))
  u <- array(runif(nobs * S), dim=c(nobs, S))
  multi_delta <- F
  # Set up parallel clusters if toggled
  if (parallel == T){
    if (is.na(cores)){
      cl <- parallel::makeCluster(parallel::detectCores())
    } else{
      cl <- parallel::makeCluster(cores)
    }
    doParallel::registerDoParallel(cl)
    `%dopar%` <- foreach::`%dopar%`
    `%:%` <- foreach::`%:%`
  }
  # Set relevant obs to NA
  if (length(names(mixed_inds)) > 0){
    if (parallel == T){
      old_names <- names(X_c)
      draws <- foreach::foreach(var = names(X_c), .combine = function(x,y) abind::abind(x,y,along=3)) %dopar%{
        temp <- draws[,,var]
        if (var %in% names(mixed_inds)){
          temp[mixed_inds[[var]],] <- NA
        }
        temp
      }
      # Recover old names
      dimnames(draws)[[3]] <- old_names
    } else{
      for (var in names(mixed_inds)){
        draws[mixed_inds[[var]],,var] <- NA
      }
    }
  }

  # Row-wise scale draws
  if (parallel == T){
    # Save dimnames
    old_dim <- dimnames(draws)
    # Nested dopar
    draws <- foreach::foreach(i = dimnames(draws)[[1]], .combine=function(x,y) abind::abind(x,y,along=1)) %:%
      foreach::foreach(j = dimnames(draws)[[3]], .combine=function(x,y) abind::abind(x,y,along=3)) %dopar% {
        tmp <- draws[i,,j]
        sd <- sqrt(sum(tmp^2, na.rm=T))
        tmp <- tmp/sd
        array(tmp, dim=c(1,length(tmp), 1))
      }
    # Recover dimnames
    dimnames(draws) <- old_dim
  } else{
    sd <- apply(draws, c(1,3), function(x) sqrt(sum(x^2, na.rm=T)))
    for (i in 1:dim(sd)[1]){
      for (j in 1:dim(sd)[2]){
        draws[i,,j] <- draws[i,,j]/sd[i,j]
      }
    }
  }

  # Set u -- check for all NA rows
  if (parallel == T){
    u <- foreach::foreach(i = 1:nrow(X_c), .combine=cbind) %dopar% {
      na_count <- sum(is.na(X_c[i,]))
      ct <- p_c - na_count
      if (ct == 0){
        rep(NA, S)
      } else{
        u[i,]^(1/ct)
      }
    }
  } else{
    na_counts <- as.array(apply(X_c, 1, function(x) sum(is.na(x))))
    ct <- p_c - na_counts
    for (i in 1:nrow(u)){
      if (ct[i] == 0){
        u[i,] <- NA
      }
      u[i,] <- u[i,]^(1/ct[i])
    }
  }
  u <- array(rep(u, p_c), dim=c(nobs, S, p_c), dimnames=list(1:nobs, 1:S, names(X_c))) # Increase dimension to be conformable with draws

  X_c <- array(unlist(X_c), dim=c(nobs,p_c), dimnames=list(1:nobs, names(X_c)))
  if (length(delta) > 1){ # Loop through deltas
    draws_list <- list()
    for (d in delta){
      tmp_draws <- draws
      if (parallel == T){
        # Save original colnames
        old_names <- dimnames(X_c)[[2]]
        tmp_draws <- foreach::foreach(j = dimnames(X_c)[[2]], .combine= function(x,y) abind::abind(x,y,along=3)) %dopar%{
          temp <- tmp_draws[,,j] * u[,,j] * delta + X_c[,j]
          temp <- temp * sigma[j] + mu[j]
          temp
        }
        dimnames(tmp_draws)[[3]] <- old_names
      }
      else{
        for (j in dimnames(X_c)[[2]]){
          # tmp_draws from uniform ball centered at standardized Xc
          tmp_draws[,,j] <- tmp_draws[,,j] * u[,,j] * d + X_c[,j]
          # tmp_draws from ball centered at original Xc
          tmp_draws[,,j] <- tmp_draws[,,j] * sigma[j] + mu[j]
        }
        # tmp_draws <- tmp_draws * u * d + array(unlist(rep(X_c, each=S)), dim=c(nobs, S, p_c))
        # tmp_draws <- tmp_draws*array(unlist(rep(sigma, each=S*nobs)), dim=c(nobs, S, p_c)) +
        #   array(unlist(rep(mu, each=S*nobs)), dim=c(nobs, S, p_c))
      }
      for (var in names(mixed_inds)){
        tmp_draws[mixed_inds[[var]],,var] <- mixed_vals[[var]]
      }
      draws_list[[as.character(d)]] <- tmp_draws
    }
    if (parallel == T){
      parallel::stopCluster(cl)
    }
    return(draws_list)
  } else{
    if (parallel == T){
      # Save original colnames
      old_names <- dimnames(X_c)[[2]]
      draws <- foreach::foreach(j = old_names, .combine= function(x,y) abind::abind(x,y,along=3)) %dopar%{
        temp <- draws[,,j] * u[,,j] * delta + X_c[,j]
        temp <- temp * sigma[j] + mu[j]
        temp
      }
      dimnames(draws)[[3]] <- old_names
    } else{
      for (j in dimnames(X_c)[[2]]){
        # draws from uniform ball centered at standardized Xc
        draws[,,j] <- draws[,,j] * u[,,j] * delta + X_c[,j]
        # draws from ball centered at original Xc
        draws[,,j] <- draws[,,j] * sigma[j] + mu[j]
      }
    }
    if (parallel == T){
      parallel::stopCluster(cl)
    }
    # draws <- draws * u * delta + array(unlist(rep(X_c, each=S)), dim=c(nobs, S, p_c))
    # draws <- draws*array(unlist(rep(sigma, each=S*nobs)), dim=c(nobs, S, p_c)) +
    #   array(unlist(rep(mu, each=S*nobs)), dim=c(nobs, S, p_c))
    # add back discrete values
    for (var in names(mixed_inds)){
      draws[mixed_inds[[var]],,var] <- mixed_vals[[var]]
    }
    return(draws)
  }
}

