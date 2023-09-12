#' Obtain pit-values for the predicted function
#'
#' @param x_cal a vector, matrix or data frame of covariates/features of the calibration set
#' @param y_cal a vector of outputs of calibration set
#' @param model a fitted model
#'
#' @return a vector of pit-values
#' @export
#'
#' @examples
#' n <- 100000
#'
#' # generationg heterocedastic data
#' mu <- function(x1){
#' 10 + 5*x1^2
#' }
#'
#' sigma_v <- function(x1){
#' 30*x1
#' }
#'
#'
#' x <- runif(n, 2, 20)
#' y <- rnorm(n, mu(x), sigma_v(x))
#'
#' x_test <- x[1:80000]
#' y_test <- y[1:80000]
#'
#' x_cal <- x[80001:100000]
#' y_cal <- y[80001:100000]
#'
#' mod <- lm(y_test ~ x_test)
#'
#'PIT_values(x_cal, y_cal, model=mod )
#'
#'

PIT_values <- function(x_cal, y_cal,  model){

  df <-data.frame(x_cal)
  names(df) <- names(model$model)[-1]
  pred <- stats::predict(model, newdata = df)
  stats::pnorm(y_cal, pred, summary(model)$sigma)
  }


