test_that("QPS with each supported ML framework", {
  test_data <- iris
  frameworks <- c("mlr3", "caret", "stats", "randomForest", "e1071", "bestridge", "rpart", "tree", "custom")
  for (framework in frameworks){
    print(framework)
    ml <- get_ml(framework, "continuous")
    if (!is.null(ml)){
    expect_setequal(expect_warning(estimate_qps(test_data[,2:4], ml, framework, infer=T, seed = 1)),
                    expect_warning(estimate_qps(test_data[,2:4], ml, framework, infer=T,seed = 1)))
    expect_length(expect_warning(estimate_qps(test_data[,2:4], ml, framework, infer=T, seed = 1)), nrow(test_data))
    }
  }
})

test_that("QPS with each supported ML framework, 4th variable discrete", {
  test_data <- iris
  frameworks <- c("mlr3", "caret", "stats", "randomForest", "e1071", "bestridge", "rpart", "tree", "custom")
  for (framework in frameworks){
    print(framework)
    ml <- get_ml(framework, "discrete")
    if (!is.null(ml)){
      expect_setequal(expect_warning(estimate_qps(test_data[,2:5], ml, framework, Xd = "Species", infer=T, seed = 2)),
                      expect_warning(estimate_qps(test_data[,2:5], ml, framework, Xd="Species", infer=T, seed = 2)))
      expect_length(expect_warning(estimate_qps(test_data[,2:5], ml, framework, Xd="Species", infer=T, seed = 2)), nrow(test_data))
    }
  }
})
