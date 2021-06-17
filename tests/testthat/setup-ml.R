library(datasets)
data("iris")

# List of frameworks currently implemented
frameworks <- c("mlr3", "caret", "stats", "randomForest", "e1071", "bestridge", "rpart", "tree", "custom")

# Custom ML algorithm: binary using median of first variable (so average always 0.5)
ML_split <- function(X){
  med <- median(unlist(X[,1]), na.rm=T)
  ret <- as.integer(unlist(X[,1]) >= med)
  return(ret)
}

# Train and return an ML object of choice
get_ml <- function(type, data_type){
  train_data <- iris
  if (type == "custom"){
    return(ML_split)
  }
  if (type == "mlr3"){
    library(mlr3)
    library(rpart)
    if (data_type == "continuous"){
      task_iris <- TaskRegr$new(id="iris", backend=train_data[,1:4], target="Sepal.Length")
      learner = lrn("regr.rpart")
      learner$train(task_iris)
      return(learner)
    } else{
      task_iris <- TaskRegr$new(id="iris", backend=train_data[,1:5], target="Sepal.Length")
      learner = lrn("regr.rpart")
      learner$train(task_iris)
      return(learner)
    }
  }
  if (type == "stats"){
    if (data_type == "continuous"){
      model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=train_data)
    } else{
      model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + factor(Species), data=train_data)
    }
    return(model)
  }
  if (type == "caret"){
    library(caret)
    fitControl_new <- trainControl(
      method = "repeatedcv"
      , number = 5
      , repeats = 1
      , verboseIter =TRUE
      , classProbs = TRUE
      , allowParallel = F     ## add this argument to overwrite the default TRUE
    )
    if(data_type == "continuous"){
      model <- train(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=train_data,
                     trControl = fitControl_new)
    } else{
      model <- train(Sepal.Length ~ ., data=train_data,
                     trControl = fitControl_new)
    }
    return(model)
  }
  if (type == "randomForest"){
    library(randomForest)
    if(data_type == "continuous"){
      model <- randomForest(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=train_data)
    } else{
      model <- randomForest(Sepal.Length ~ ., data=train_data)
    }
    return(model)
  }
  if (type == "e1071"){
    library(e1071)
    if(data_type == "continuous"){
      model <- svm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=train_data)
    } else{
      model <- svm(Sepal.Length ~ ., data=train_data)
    }
    return(model)
  }
  if (type == "bestridge"){
    library(bestridge)
    if(data_type == "continuous"){
      train_inp <- as.matrix(train_data[,2:4])
      dimnames(train_inp) <- NULL
      colnames(train_inp) <- names(train_data[,2:4])
      model <- bsrr(train_inp, train_data$Sepal.Length)
      return(model)
    } else{
      return(NULL)
    }
  }
  if (type == "rpart"){
    library(rpart)
    if(data_type == "continuous"){
      model <- rpart(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=train_data)
    } else{
      model <- rpart(Sepal.Length ~ ., data=train_data)
    }
    return(model)
  }
  if (type == "tree"){
    library(tree)
    if(data_type == "continuous"){
      model <- tree(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=data)
    } else{
      model <- tree(Sepal.Length ~ ., data=data)
    }
    return(model)
  }
}
