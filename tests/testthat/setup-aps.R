library(datasets)
data("iris")
data <- iris[, 1:4]

model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=data)

assign_cutoff <- function(X, cutoff){
  ret <- as.integer(X > cutoff)
  return(ret)
}
