## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=F------------------------------------------------------------
library(qps)

## -----------------------------------------------------------------------------
library(datasets)
data(iris)
str(iris)

## -----------------------------------------------------------------------------
iris$target <- as.integer(iris$Species %in% c("setosa", "versicolor"))

## ---- warning=F, message=F----------------------------------------------------
library(randomForest)
linear_model <- lm(target ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
rf_model <- randomForest(target ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)

## -----------------------------------------------------------------------------
library(data.table)
n <- 10000
setDT(iris)
boot_data <- list()
for (col in c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")){
  boot_data[[col]] <- rnorm(n, mean(iris[,get(col)]), sd(iris[,get(col)]))
}
full_data <- as.data.table(boot_data)

## -----------------------------------------------------------------------------
# Simulate treatment outcomes
b1 <- rnorm(nrow(full_data), 2, 2)
b2 <- rnorm(nrow(full_data), 2, 2)
e0 <- rnorm(nrow(full_data))
treatment_effect <- rnorm(nrow(full_data), 5, 0.2)
e1 <- rnorm(nrow(full_data))
full_data$Y0 <- b1 * full_data[,Sepal.Length] + b2 * full_data[,Sepal.Width] + e0
full_data$Y1 <- full_data$Y0 + treatment_effect + e1

## -----------------------------------------------------------------------------
cutoff <- function(probs, c = 0.5){
  return(as.integer(probs > c))
}
full_data$lm_pred <- predict(linear_model, full_data)
full_data$rf_pred <- predict(rf_model, full_data)
full_data$lm_Z <- cutoff(full_data$lm_pred)
full_data$rf_Z <- cutoff(full_data$rf_pred)

# Generate treatment assignment D
full_data$lm_D <- full_data$lm_Z
lm_p <- runif(nrow(full_data))
full_data[lm_p <= 0.25 & lm_Z == 1, lm_D := 0]
full_data[,.N,.(lm_Z, lm_D)]

full_data$rf_D <- full_data$rf_Z
rf_p <- runif(nrow(full_data))
full_data[rf_p <= 0.25 & rf_Z == 1, rf_D := 0]
full_data[,.N,.(rf_Z, rf_D)]

# Realized outcomes
full_data[, lm_Y := Y1]
full_data[lm_D == 0, lm_Y := Y0]
full_data[, rf_Y := Y1]
full_data[rf_D == 0, rf_Y := Y0]

## -----------------------------------------------------------------------------
ate <- mean(full_data$Y1 - full_data$Y0)
atet <- mean(full_data[lm_D == 1, Y1] - full_data[lm_D == 1, Y0])
late <- mean(full_data[lm_D == lm_Z, Y1] - full_data[lm_D == lm_Z, Y0])
cat(paste0("ATE: ", ate, "\nATET: ", atet, "\nLATE: ", late, "\nTrue Effect: ", mean(treatment_effect)))

## -----------------------------------------------------------------------------
full_data$lm_qps <- estimate_qps(full_data, linear_model, Xc=names(full_data)[1:4], S=100, delta=1.5, fcn=cutoff, parallel=T)
lm_effect <- estimate_treatment_effect(full_data, qps_lab="lm_qps", Y_lab="lm_Y", Z_lab="lm_Z", D_lab="lm_D")

full_data$rf_qps <- estimate_qps(full_data, rf_model, Xc=names(full_data)[1:4], S=100, delta=1.5, fcn=cutoff, parallel=T)
rf_effect <- estimate_treatment_effect(full_data, qps_lab="rf_qps", Y_lab="rf_Y", Z_lab="rf_Z", D_lab="rf_D")

## -----------------------------------------------------------------------------
cat(paste0("linear model LATE: ", lm_effect$coefficients['D'], "\nrandom forest LATE: ", rf_effect$coefficients['D']))

