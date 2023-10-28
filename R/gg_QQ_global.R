#' QQPlot diagnose calibration globally
#'
#' @description
#' ggplot to visualize predicted vs empirical cumulative distributions
#'
#'
#' @param pit vector of global PIT-values
#' @param ycal vector of y calibration set
#' @param yhat vector of predicted y on calibration set
#' @param mse Mean Squared Error from fitted model
#' @return a ggplot point graph
#' @export
#'
#' @examples
#'
#' n <- 100000
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
#' x_cal <- x[80001:100000]
#' y_cal <- y[80001:100000]
#'
#'model <- lm(y_train ~ x_train)
#'y_hat <- predict(model, newdata=data.frame(x_test=x_cal))
#'MSE <- (summary(model)$sigma)^2
#'
#'pit <- PIT_global( y_cal, y_hat,  MSE)
#'
#'
#'gg_QQ_global(pit,y_cal, y_hat, MSE)
#'
#'


gg_QQ_global <- function(pit, ycal, yhat, mse){

  df <- purrr::map_dfr(1:length(pit), ~{

    pit_emp <- mean(ycal <= qnorm(p=pit[.] ,
                                   mean=yhat,
                                   sd=sqrt(mse)))
    pit_hat <- pit[.]
    tibble::tibble(pit_hat, pit_emp)

  } )


  ggplot2:: ggplot(df)+
    ggplot2::geom_point(ggplot2::aes(x=pit_hat, y=pit_emp), color="#4B5D5D", size=0.01)+
    ggplot2::labs(x="Predicted CDF",
                  y="Empirical CDF")+
    ggplot2::geom_abline(slope = 1, linetype="dashed", color="red")+
    ggplot2::theme_bw(base_size = 14)
}
