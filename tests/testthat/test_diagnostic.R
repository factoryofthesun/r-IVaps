test_that("Covariate balance", {
  test_data <- iris
  model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=test_data)
  qps <- estimate_qps(test_data, model, Xc = names(test_data)[2:4], infer=F, S=400, delta=0.8, fcn=assign_cutoff, c = 6)

  # Test input errors
  expect_error(covariate_balance_test(test_data))
  expect_error(covariate_balance_test(test_data, qps, X_lab = names(test_data)[2:4], Z = assign_cutoff(test_data$Sepal.Length, 6),
                                      degen = unique(qps)))

  # Test: compare direct and indirect input methods
  Z <- assign_cutoff(test_data$Sepal.Length, 6)
  X <- test_data[,2:4]
  test_data[, QPS := qps]
  test_data[, Z := Z]
  out_direct <- covariate_balance_test(test_data, qps, test_data[, 2:4], Z)
  out_indirect <- covariate_balance_test(test_data, X_lab = names(test_data)[2:4])
  expect_equal(out_direct$F_stat, out_indirect$F_stat)
  expect_equal(out_direct$p_val, out_indirect$p_val)
  expect_equal(out_direct$model$coefficients, out_indirect$model$coefficients)
  expect_equal(out_direct$test$SSPH, out_indirect$test$SSPH)
})
