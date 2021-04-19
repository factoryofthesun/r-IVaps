# Complex QPS tests will all involve simple lm objects and the iris dataset.
# For ML-framework specific tests see `test_ml.R`

test_that("Raise error if no continuous", {
  expect_error(estimate_qps(data, assign_cutoff, "custom", infer=T, Xd = names(data)), "No continuous variables left")
  expect_error(estimate_qps(data, assign_cutoff, "custom", infer=F, Xd = names(data)), "No continuous variables passed")
})

test_that("QPS cts inputs with seed", {
  expect_setequal(estimate_qps(data, model, Xc = names(data)[2:4], infer=F, S=50, delta=0.1, seed=1),
               estimate_qps(data, model, Xc = names(data)[2:4], infer=T, S=50, delta=0.1, seed=1))
  expect_setequal(estimate_qps(data, model, Xc = names(data)[2:4], infer=F, S=50, delta=0.1, seed=1),
                  estimate_qps(data[,2:4], model, infer=T, S=50, delta=0.1, seed=1))
})

test_that("QPS cts and discrete inputs with seed", {
  expect_setequal(estimate_qps(data, model, Xc = names(data)[2:3], Xd=names(data)[4], infer=F, S=50, delta=0.1, seed=2),
                  estimate_qps(data, model, Xc = names(data)[2:3], infer=T, S=50, delta=0.1, seed=2))
  expect_setequal(estimate_qps(data, model, Xc = names(data)[2:3], Xd=names(data)[4], infer=F, S=50, delta=0.1, seed=2),
                  estimate_qps(data[,2:4], model, Xd=names(data)[4], infer=T, S=50, delta=0.1, seed=2))
})

test_that("QPS with decision function round", {
  expect_setequal(estimate_qps(data, model, Xc = names(data)[2:3], Xd=names(data)[4], infer=F, S=50, delta=0.1, seed=2,
                               fcn=assign_cutoff, cutoff=6),
                  estimate_qps(data[,2:4], model, Xc = names(data)[2:3], infer=T, S=50, delta=0.1, seed=2,
                               fcn=assign_cutoff, cutoff=6))
})

test_that("QPS with arbitrary nan", {
  tmp_data <- data.table::copy(data)
  cols <- sample(2:4, 20, replace=T)
  rows <- sample(1:150, 20)
  for (i in 1:20){
    tmp_data[rows[i], cols[i]] <- NA
  }
  expect_setequal(estimate_qps(tmp_data, model, Xc = names(tmp_data)[2:3], Xd=names(tmp_data)[4], infer=F, S=50, delta=0.1, seed=3),
                  estimate_qps(tmp_data[,2:4], model, Xd=names(tmp_data)[4], infer=T, S=50, delta=0.1, seed=3))
  expect_length(estimate_qps(tmp_data, model, Xc = names(tmp_data)[2:3], Xd=names(tmp_data)[4], infer=F, S=50, delta=0.1, seed=3),
                  nrow(tmp_data))
})

test_that("QPS with mixed variables", {
  L <- list("Sepal.Width" = c(2, 3), "Petal.Length" = c(3, 4))
  expect_setequal(estimate_qps(data, model, Xc = names(data)[2:3], Xd=names(data)[4], infer=F, L = L, S=50, delta=0.1, seed=3),
                  estimate_qps(data[,2:4], model, Xd=names(data)[4], infer=T, L=L, S=50, delta=0.1, seed=3))
  expect_length(estimate_qps(data, model, Xc = names(data)[2:3], Xd=names(data)[4], infer=F, L = L, S=50, delta=0.1, seed=3),
                nrow(data))
})

test_that("QPS with multiple deltas", {
  deltas <- c(0.1, 0.5, 0.8)
  expect_setequal(estimate_qps(data, model, Xc = names(data)[2:3], Xd=names(data)[4], infer=F, delta=deltas, seed=4),
                  estimate_qps(data[,2:4], model, Xd=names(data)[4], infer=T, delta=deltas, seed=4))
  expect_mapequal(estimate_qps(data, model, Xc = names(data)[2:3], Xd=names(data)[4], infer=F, delta=deltas, seed=4),
                  estimate_qps(data[,2:4], model, Xd=names(data)[4], infer=T, delta=deltas, seed=4))
})

test_that("QPS parallel execution", {
  chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  if (nzchar(chk) && chk == "TRUE") {
    # use 2 cores in CRAN/Travis/AppVeyor
    num_workers <- 2
  } else {
    # use all cores in devtools::test()
    num_workers <- parallel::detectCores()
  }
  L <- list("Sepal.Width" = c(2, 3), "Petal.Length" = c(3, 4))
  expect_setequal(estimate_qps(data, model, Xc = names(data)[2:3], Xd=names(data)[4], infer=F, L = L, S=50, delta=0.1, seed=4,
                               parallel=T, cores=num_workers),
                  estimate_qps(data[,2:4], model, Xd=names(data)[4], infer=T, L=L, S=50, delta=0.1, seed=4,
                               parallel=T, cores=num_workers))
})

test_that("QPS parallel execution with multiple deltas", {
  chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  if (nzchar(chk) && chk == "TRUE") {
    # use 2 cores in CRAN/Travis/AppVeyor
    num_workers <- 2
  } else {
    # use all cores in devtools::test()
    num_workers <- parallel::detectCores()
  }
  deltas <- c(0.1, 0.5, 0.8)
  expect_setequal(estimate_qps(data, model, Xc = names(data)[2:3], Xd=names(data)[4], infer=F, delta=deltas, seed=4,
                               parallel=T, cores=num_workers),
                  estimate_qps(data[,2:4], model, Xd=names(data)[4], infer=T, delta=deltas, seed=4,
                               parallel=T, cores=num_workers))
  expect_mapequal(estimate_qps(data, model, Xc = names(data)[2:3], Xd=names(data)[4], infer=F, delta=deltas, seed=4,
                               parallel=T, cores=num_workers),
                  estimate_qps(data[,2:4], model, Xd=names(data)[4], infer=T, delta=deltas, seed=4,
                               parallel=T, cores=num_workers))

})
