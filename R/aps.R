.datatable.aware=T

#' Main APS estimation function.
#' @import data.table
#' @import stats
#' @param data Dataset containing ML input variables.
#' @param ml ML function for treatment recommendation
#' @param ml_type String indicating the ML object source package name, or "custom" for a user-defined
#' algorithm that runs prediction upon function call with a single input data.table object.
#' See details for supported packages. Defaults to "stats".
#' @param xc Character vector of column names of the continuous variables.
#' @param xd Character vector of column names of the discrete variables.
#' @param infer Boolean whether to infer continuous/discrete variables from remaining columns of data. Defaults to False.
#' @param s Number of draws for each APS estimation. Defaults to 100.
#' @param delta Radius of sampling ball. Can be either numeric or numeric vector. Defaults to 0.8.
#' @param L Named list where the names correspond to the names of the mixed
#'   variables in the data, and the values are numeric vectors indicating the
#'   set of discrete values for the variable.
#' @param seed Random seed
#' @param fcn Function to apply to output of ML function
#' @param parallel Boolean indicator for whether to parallelize the APS
#'   estimation. Defaults to FALSE.
#' @param cores Integer number of cores for parallelization. If NA, then detectCores() is called.
#' @param \dots Additional inputs to be passed to \code{fcn}
#' @return If a single \code{delta} value is passed, then the function returns a vector of
#' estimated Approximate Propensity Scores of the same length as the input data. If multiple
#' \code{delta} are passed, then a list of estimated APS vectors are returned, where the
#' keys are each \code{delta} value.
#'
#' @examples
#' data("iris")
#' # Iris examples
#' model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=iris)
#' estimate_aps(iris, model, xc = names(iris)[2:3], xd = names(iris)[4],
#'             infer=FALSE, s=50, delta=0.1)
#'
#' # Estimate APS while applying decision function assign_cutoff to model output with input cutoff=0.5
#' assign_cutoff <- function(X, cutoff){
#'   ret <- as.integer(X > cutoff)
#'   return(ret)
#' }
#' estimate_aps(iris, model, xc = names(iris)[2:3], xd = names(iris)[4],
#'              infer=FALSE, s=50, delta=0.1, fcn=assign_cutoff, cutoff=0.5)
#'
#' # Multiple deltas
#' estimate_aps(iris, model, xc = names(iris)[2:3], xd = names(iris)[4],
#'              infer=FALSE, s=50, delta=c(0.1,0.5,1))
#'
#' # Define mixed continuous/discrete variables
#' estimate_aps(iris, model, xc = names(iris)[2:3], xd = names(iris)[4],
#'              infer=FALSE, s=50, delta=0.1,
#'              L = list("Sepal.Width" = c(2, 3), "Petal.Length" = c(3, 4)))
#' @details
#' ML packages currently supported:
#' "mlr3", "caret", "stats" (base), "randomForest", "e1071", "bestridge", "rpart", "tree",
#' "custom" (for user-defined functions)
#'
#' Approximate propensity score estimation involves taking draws \eqn{X_c^1,\ldots,X_c^s}
#' from the uniform distribution on
#' \eqn{N(X_{ci}, \delta)}, where \eqn{N(X_{ci},\delta)} is the
#' \eqn{p_c} dimensional ball centered at \eqn{X_{ci}} with radius \eqn{\delta}.
#' \eqn{X_c^1, \ldots,X_c^s} are destandardized before passed for ML
#' inference. The estimation equation is \eqn{p^s(X_i;\delta) = \frac{1}{s}\sum_{s=1}^{s} ML(X_c^s, X_{di})}.
#'
#' If neither \code{xc} nor \code{xd} are passed and \code{infer=T}, then \code{data} is assumed to
#' be all relevant continuous inputs. If only one is passed, then the remaining
#' variables in \code{data} are assumed to be inputs of the other type. It is
#' recommended to pass both \code{xc} and \code{xd} in the case that not all the
#' variables in \code{data} are relevant for the ML input, instead of relying on inference.
#' @export
estimate_aps <- function(data, ml, ml_type = "stats", xc = NA, xd = NA, infer=FALSE, s = 100, delta = 0.8,
                         L = NA, seed = NA, fcn = NA, parallel = F, cores = NA, ...){
  data.table::setDT(data)
  if (infer == T){
    if (all(is.na(xc)) & all(is.na(xd))){
      warning("(Inference on) Neither `xc` nor `xd` passed. Assuming all variables in `data` are continuous...")
      X_c <- data
      X_d <- NA
    } else if(all(is.na(xc))){
      warning("(Inference on) `xd` passed but `xc` not passed. Assuming all remaining variables in `data` are continuous...")
      xc <- setdiff(names(data), xd)
      X_d <- data[, xd, with = F]
      X_c <- data[, xc, with = F]
      if (length(X_c) < 1){ # If no cts columns then raise error
        stop(paste0("No continuous variables left in input data after removing discrete.",
                    " APS estimation requires at least one continuous variable."))
      }
    } else if(all(is.na(xd))){
      warning("(Inference on) `xc` passed but `xd` not passed. Assuming all remaining variables in `data` are discrete...")
      xd <- setdiff(names(data), xc)
      X_d <- data[, xd, with = F]
      X_c <- data[, xc, with = F]
    } else{
      X_c <- data[, xc, with = F]
      X_d <- data[, xd, with = F]
    }
  } else{
    if (all(is.na(xc))|length(xc) == 0){
      stop(paste0("No continuous variables passed and variable inference is off.",
                  " APS estimation requres at least one continuous variable."))
    }
    X_c <- data[, xc, with = F]
    X_d <- NA
    if (!all(is.na(xd)) & length(xd) > 0){
      X_d <- data[, xd, with = F]
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
  aps <- computeAPS(X_c, X_d, ml, ml_type, s, delta, mu, sigma, mixed_inds, mixed_vals, fcn, parallel,
                    cores, ...)
  return(aps)
}

computeAPS <- function(X_c, X_d, ml, ml_type, s, delta, mu, sigma, mixed_inds, mixed_vals, fcn,
                       parallel, cores, ...){
  draws <- drawAPS(X_c, X_d, s, delta, mu, sigma, mixed_inds, mixed_vals, parallel, cores)
  nobs <- nrow(X_c)
  p_c <- length(names(X_c))

  # Run ML prediction depending on type of function
  if (!all(is.na(X_d)) & length(X_d) > 0){
    X_d_long <- X_d[rep(X_d[,.I], each=s)]
  } else{
    X_d_long <- NA
  }
  if (typeof(draws) == "list"){
    aps_list <- list()
    for (d in as.character(delta)){
      # Convert draws and discrete values back to data table
      tmp_draws <- aperm(draws[[d]], c(2,1,3))
      tmp_draws <- data.table::as.data.table(matrix(tmp_draws, s*nobs, p_c))
      data.table::setnames(tmp_draws, names(X_c))
      if (!all(is.na(X_d_long))){
        input <- cbind(tmp_draws, X_d_long)
      } else{
        input <- tmp_draws
      }
      if (ml_type %in% c("mlr3", "caret", "stats", "randomForest", "e1071", "bestridge")){ # All these packages extend 'predict' function
        pred_out <- predict(ml, input)
      } else if (ml_type %in% c("rpart", "tree")){
        pred_out <- predict(ml, input, type="vector")
      } else if (ml_type == "custom"){
          pred_out <- ml(input)
        } else {
        message(paste0("Package ", ml_type, " not found in internal library. Attempting to apply `predict()` syntax..."))
        pred_out <- tryCatch({
          predict(ml, input)
        },
        error=function(cond){
          message(paste0("`Predict` call raised error: ", cond))
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
      # Avg every s values of prediction output
      aps <- .colMeans(pred_out, s, length(pred_out)/s, na.rm=T)
      aps_list[[d]] <- aps
    }
    return(aps_list)
  } else{
    # Convert draws back to data table
    draws <- data.table::as.data.table(matrix(aperm(draws, c(2,1,3)), s*nobs, p_c))
    data.table::setnames(draws, names(X_c))
    if (!all(is.na(X_d_long))){
      input <- cbind(draws, X_d_long)
    } else{
      input <- draws
    }
    if (ml_type %in% c("mlr3", "caret", "stats", "randomForest", "e1071", "bestridge")){ # All these packages extend 'predict' function
      pred_out <- predict(ml, input)
    } else if (ml_type %in% c("rpart", "tree")){
      pred_out <- predict(ml, input, type="vector")
    } else if (ml_type == "custom"){
      pred_out <- ml(input)
    } else {
      message(paste0("Package ", ml_type, " not found in internal library. Attempting to apply `predict()` syntax..."))
      pred_out <- tryCatch({
        predict(ml, input)
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
    # Avg every s values of prediction output
    aps <- .colMeans(pred_out, s, length(pred_out)/s, na.rm=T)
    return(aps)
  }
}

drawAPS <- function(X_c, X_d, s, delta, mu, sigma, mixed_inds, mixed_vals, parallel, cores){
  nobs <- nrow(X_c)
  p_c <- length(X_c)
  draws <- array(rnorm(nobs * p_c * s), dim=c(nobs, s, p_c), dimnames=list(1:nobs, 1:s, names(X_c)))
  u <- array(runif(nobs * s), dim=c(nobs, s))
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
        rep(NA, s)
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
  u <- array(rep(u, p_c), dim=c(nobs, s, p_c), dimnames=list(1:nobs, 1:s, names(X_c))) # Increase dimension to be conformable with draws

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
          # tmp_draws from uniform ball centered at standardized xc
          tmp_draws[,,j] <- tmp_draws[,,j] * u[,,j] * d + X_c[,j]
          # tmp_draws from ball centered at original xc
          tmp_draws[,,j] <- tmp_draws[,,j] * sigma[j] + mu[j]
        }
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
        # draws from uniform ball centered at standardized xc
        draws[,,j] <- draws[,,j] * u[,,j] * delta + X_c[,j]
        # draws from ball centered at original xc
        draws[,,j] <- draws[,,j] * sigma[j] + mu[j]
      }
    }
    if (parallel == T){
      parallel::stopCluster(cl)
    }
    # add back discrete values
    for (var in names(mixed_inds)){
      draws[mixed_inds[[var]],,var] <- mixed_vals[[var]]
    }
    return(draws)
  }
}

