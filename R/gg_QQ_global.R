#' QQPlot diagnose calibration globally
#'
#' @description
#' ggplot to visualize predicted vs empirical cumulative distributions
#'
#'
#' @param pit vector of global PIT-values
#' @param ycal vector of y calibration set
#' @param yhat vector of predicted y on calibration set
#' @param mse Mean Squared Error from calibration set
#' @return a ggplot point graph
#'
#' @export
#' @import ggplot2
#'
#' @examples
#'
#' n <- 100000
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
#' gg_QQ_global(pit,y_cal, y_hat, MSE_cal)
#'
#'


gg_QQ_global <- function(pit, ycal, yhat, mse){

  df <- purrr::map_dfr(1:length(pit), ~{

    tibble::tibble(pit_emp = mean(ycal <= qnorm(p=pit[.] ,
                                   mean=yhat,
                                   sd=sqrt(mse))),
                   pit_hat=pit[.])

  } )


  ggplot2:: ggplot()+
    ggplot2::geom_point(ggplot2::aes(x=dplyr::pull(df[,2]), y=dplyr::pull(df[,1])),
                        color="#4B5D5D", size=0.01)+
    ggplot2::labs(x="Predicted CDF",
                  y="Empirical CDF")+
    ggplot2::geom_abline(slope = 1, linetype="dashed", color="red")+
    ggplot2::theme_bw(base_size = 14)
}
