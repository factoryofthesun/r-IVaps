test_that("APS with each supported ML framework", {
  test_data <- iris
  for (framework in frameworks){
    print(framework)
    ml <- get_ml(framework, "continuous")
    if (!is.null(ml)){
    expect_setequal(expect_warning(estimate_aps(test_data[,2:4], ml, framework, infer=T, seed = 1)),
                    expect_warning(estimate_aps(test_data[,2:4], ml, framework, infer=T,seed = 1)))
    expect_length(expect_warning(estimate_aps(test_data[,2:4], ml, framework, infer=T, seed = 1)), nrow(test_data))
    }
  }
})

test_that("APS with each supported ML framework, 4th variable discrete", {
  test_data <- iris
  for (framework in frameworks){
    print(framework)
    ml <- get_ml(framework, "discrete")
    if (!is.null(ml)){
      expect_setequal(expect_warning(estimate_aps(test_data[,2:5], ml, framework, xd = "Species", infer=T, seed = 2)),
                      expect_warning(estimate_aps(test_data[,2:5], ml, framework, xd="Species", infer=T, seed = 2)))
      expect_length(expect_warning(estimate_aps(test_data[,2:5], ml, framework, xd="Species", infer=T, seed = 2)), nrow(test_data))
    }
  }
})
