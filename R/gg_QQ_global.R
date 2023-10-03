#'  QQPlot to diagnose calibration globally bases on theoretical PIT_values
#' from calibrated distributions and the empirical pit_values
#'
#'
#' @param pit vector of pit values
#' @param y_cal vector of y calibration set
#'
#' @return a ggplot histogram graph
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
#'pit <- PIT_global_lm( y_cal, cdf)
#'
#'
#'gg_QQ_global(pit, y_cal)
#'
#'


gg_QQ_global <- function(pit, y_cal){
  pit_theory <- sort(stats::qunif(seq(0,1, by=(1/(length(y_cal)-1)))))
  pit_emp <- sort(pit)
  df <- data.frame(pit_theory=pit_theory, pit=pit_emp)

  ggplot2:: ggplot(df)+
    ggplot2::geom_point(ggplot2::aes(x=pit_theory, y=pit_emp), color="#4B5D5D", size=0.01)+
    ggplot2::labs(x="Predicted CDF",
                  y="Empirical CDF")+
    ggplot2::geom_abline(slope = 1, linetype="dashed", color="red")+
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(axis.title.y=ggplot2::element_text(colour="black"),
                   axis.title.x = ggplot2::element_text(colour="black"),
                   axis.text = ggplot2::element_text(colour = "black"),
                   panel.border = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black"),
                   plot.margin = ggplot2::margin(0,0.5, 0,0, "cm"))
}
