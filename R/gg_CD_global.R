#' Plots the cumulative distributions of PIT-values for global calibration diagnostics.
#'
#'@description
#' Visualizes the predicted vs. empirical cumulative distributions of PIT-values using ggplot.
#'
#' This function creates a ggplot graph that compares the cumulative distributions of
#' predicted and empirical Probability Integral Transform (PIT) values. It shows the calibration quality of a regression model by examining how well
#' the predicted values conform to the observed values.

#'@param pit Numeric vector of global PIT-values. It is recommended to calculate these using the `PIT_global()` function.
#'@param ycal Numeric vector representing the true observations (y-values) of the response variable from the calibration dataset.
#'@param yhat Numeric vector of predicted response (y-hat-values) on the calibration dataset.
#'@param mse Mean Squared Error calculated from the calibration dataset.
#'@return A `ggplot` object displaying a point graph of the empirical versus predicted cumulative distributions of PIT-values.

#'
#' @export
#'
#' @examples
#'
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
#' pit <- PIT_global( y_cal, y_hat,  MSE_cal)
#'
#' gg_CD_global(pit,y_cal, y_hat, MSE_cal)
#'
#'


gg_CD_global <- function(pit, ycal, yhat, mse){


  df <- do.call(rbind, purrr::map(1:length(pit), ~{

    c(pit_emp = mean(ycal <= qnorm(p = pit[.] ,
                                   mean = yhat,
                                   sd = sqrt(mse))),
                   pit_hat=pit[.])


  } ))



  ggplot2:: ggplot()+
    ggplot2::geom_point(ggplot2::aes(x=df[,2], y=df[,1]),
                        color="#4B5D5D", size=0.01)+
    ggplot2::labs(x="Predicted",
                  y="Empirical")+
    ggplot2::geom_abline(slope = 1, linetype="dashed", color="red")+
    ggplot2::theme_classic(base_size = 14)
}
