#' Creates the probability predictive distribution of an lm() model
#'
#' @param x_cal Observation vector/matrix of the calibration set
#' @param model A lm() model
#'
#' @return A cumulative predictive distribution function of a normal distribution
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
#'
CDF_model_lm <- function(x_cal, model) {
  df <- data.frame(x_cal)
  names(df) <- names(model$model)[-1]
  pred <- stats::predict(model, newdata = df)
  Fx <- function(y_cal) stats::pnorm(y_cal, pred, summary(model)$sigma)
  return(Fx)
}
