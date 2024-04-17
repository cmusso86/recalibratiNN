#' Obtain the PIT-values of a Model
#'
#' @description
#' A function to calculate the Probability Integral Transform (PIT) values for any fitted model
#' that assumes a normal distribution of the output.
#'
#' @param ycal Numeric vector representing the true observations (y-values) of the response variable from the calibration dataset.
#' @param yhat Numeric vector of predicted y-values on the calibration dataset.
#' @param mse Mean Squared Error calculated from the calibration dataset.
#' @return Returns a numeric vector of PIT-values.
#' @export
#'
#' @details
#'
#' This function is designed to work with models that is, even implicitly, assuming normal distribution of the response variable.
#' This includes, but is not limited to, linear models created using `lm()` or neural networks utilizing Mean Squared Error as the loss function.
#' The OLS method is used to minimized residuals in these models. This mathematical optimization will also yield a probabilistic optimization
#' when normal distribution of the response variable is assumed, since OLS and maximum likelihood estimation are equivalent under normality.
#' Therefore, in order to render a probabilistic interpretation of the predictions, the model is intrinsically assuming a normal distribution of the response variable.
#'
#'
#' @examples
#' n <- 10000
#' split <- 0.8
#'
#' # generating heterocedastic data
#' mu <- function(x1){
#' 10 + 5*x1^2
#' }
#'
#' sigma_v <- function(x1){
#' 30*x1
#' }
#'
#'
#' x <- runif(n, 1, 10)
#' y <- rnorm(n, mu(x), sigma_v(x))
#'
#' x_train <- x[1:(n*split)]
#' y_train <- y[1:(n*split)]
#'
#' x_cal <- x[(n*split+1):n]
#' y_cal <- y[(n*split+1):n]
#'
#' model <- lm(y_train ~ x_train)
#'
#' y_hat <- predict(model, newdata=data.frame(x_train=x_cal))
#'
#' MSE_cal <- mean((y_hat - y_cal)^2)
#'
#' PIT_global(ycal=y_cal, yhat=y_hat, mse=MSE_cal)
#'


PIT_global <- function( ycal, yhat,
                        mse){
   stats::pnorm(ycal, yhat, sqrt(mse))
}
