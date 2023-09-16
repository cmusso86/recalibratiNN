#' Obtain pit-values for an fitted lm() model
#'
#'
#' @param y_cal a vector of outputs of calibration set
#' @param pdf A predective cumulative function
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
#' model <- lm(y_test ~ x_test)
#'
#'cdf <- CDF_model_lm(x_cal=x_cal, model= model)
#'
#'PIT_values_lm( y_cal, cdf)
#'
#'
#'
#

PIT_values_lm <- function( y_cal,pdf){
  pdf(y_cal)
  }


