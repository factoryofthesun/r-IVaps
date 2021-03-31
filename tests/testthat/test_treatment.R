test_that("Treatment effect input errors", {
  test_data <- iris
  model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=test_data)
  qps <- estimate_qps(test_data, model, Xc = names(test_data)[2:4], infer=F, S=400, delta=0.8, fcn=assign_cutoff, c = 6)

  # Test input errors
  expect_error(covariate_balance_test(test_data))
  expect_error(covariate_balance_test(test_data, qps, X_lab = names(test_data)[2:4], Z = assign_cutoff(test_data$Sepal.Length, 6),
                                      degen = unique(qps)))
})

test_that("Treatment effect estimation", {
  test_data <- iris
  model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=test_data)
  qps <- estimate_qps(test_data, model, Xc = names(test_data)[2:4], infer=F, S=400, delta=0.8, fcn=assign_cutoff, c = 6)

  # Test: compare direct and indirect input methods
  D_val <- assign_cutoff(test_data$Sepal.Length, 6)
  Z_val <- assign_cutoff(predict(model, test_data), 6)
  Y_val <- test_data$Sepal.Length
  test_data[, QPS := qps]
  test_data[, Y := Sepal.Length]
  test_data[, Z := Z_val]
  test_data[, D := D_val]

  out_direct <- estimate_treatment_effect(test_data, qps, Y_val, Z_val, D_val)
  out_indirect <- estimate_treatment_effect(test_data)
  expect_equal(out_direct$coefficients, out_indirect$coefficients)
})

test_that("Counterfactual value estimation", {
  library(mlr3)
  test_data <- iris
  model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=test_data)
  ML <- predict(model, test_data)
  task_iris <- TaskRegr$new(id="iris", backend=test_data[,1:4], target="Sepal.Length")
  learner = lrn("regr.rpart")
  learner$train(task_iris)
  ML_new <- predict(learner, test_data)
  qps <- estimate_qps(test_data, model, Xc = names(test_data)[2:4], infer=F, S=400, delta=0.8, fcn=assign_cutoff, c = 6)

  # Test: compare direct and indirect input methods
  Z_val <- assign_cutoff(test_data$Sepal.Length, 6)
  X <- test_data[,2:4]
  Y_val <- unlist(test_data[,1])
  names(Y_val) <- NULL
  test_data[, QPS := qps]
  test_data[, Z := Z_val]
  test_data[, ml := ML]
  test_data[, ml_new := ML_new]
  test_data[, Y := Sepal.Length]
  names(ML) <- NULL
  names(ML_new) <- NULL
  out_direct <- estimate_counterfactual_ml(test_data, qps, Y_val, Z_val, ML, ML_new)
  out_indirect <- estimate_counterfactual_ml(test_data, qps_lab = "QPS", Y_lab = "Y", Z_lab = "Z", ML_lab = "ml",
                                             ML_new_lab = "ml_new")
  expect_equal(out_direct$value, out_indirect$value)
  expect_equal(out_direct$model$coefficients, out_indirect$model$coefficients)
})
