
#' Obtain the PIT-values of a model.
#' @description
#' A function to obtain the (possibly uncalibrated) PIT-values of any fitted model that
#'assumes a normal distribution for the output, such as (but not limited to), a lm() or a neural network
#'that used the Mean Squared Error as the loss function.
#'
#' @param xcal covariates/features of the recalibration set
#' @param ycal observations of the recalibration set
#' @param yhat predictions of the recalibration set from the uncalibrated model
#' @param mse Mean Squared Error of the trained model. Is you fitted a
#' lm() model you should square the the sigma parameter.
#'
#' @return Vector of PIT-values
#' @export
#'
#' @examples
#' #' n <- 100000
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
#' x <- runif(n, 2, 20)
#' y <- rnorm(n, mu(x), sigma_v(x))
#'
#' x_train <- x[1:80000]
#' y_train <- y[1:80000]
#'
#' xcal <- x[80001:100000]
#' ycal <- y[80001:100000]
#'
#' model <- lm(y_train ~ x_train)
#' y_hat <- predict(model, newdata=data.frame(x_train=x_cal))
#' MSE <- (summary(model)$sigma)^2
#'
#' PIT_global(ycal=y_cal, ycal=y_hat, mse=MSE)
#'


PIT_global <- function( ycal, yhat,
                        mse){
   stats::pnorm(ycal, yhat, sqrt(mse))
}
