library(datasets)
data("iris")
data <- iris[, 1:4]

model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=data)

assign_cutoff <- function(X, c){
  ret <- as.integer(X > c)
  return(ret)
}
