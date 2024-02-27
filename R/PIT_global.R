
#' Obtain the PIT-values of a model.
#' @description
#' A function to obtain the (possibly uncalibrated) PIT-values of any fitted model that
#'assumes a normal distribution for the output, such as (but not limited to), a lm() or a neural network
#'that used the Mean Squared Error as the loss function.
#'
#' @param ycal observations of the recalibration set
#' @param yhat predictions of the recalibration set from the uncalibrated model
#' @param mse Mean Squared Error of validation set.
#' @return Vector of PIT-values
#' @export
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
