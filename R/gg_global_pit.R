#' Plot to diagnose calibration bases on  p_values
#'
#' @param pit vector of pit values
#'
#' @return a ggplot histogram graph
#' @import stats
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
#' cdf <- CDF_model_lm(x_cal=x_cal, model= model)
#'
#' pit <- PIT_global_lm( y_cal, cdf)
#'
#' gg_global_pit(pit)
#'
#'
#'
#'

gg_global_pit <- function(pit){

  ggplot2::ggplot(NULL, ggplot2::aes(pit, ggplot2::after_stat(density)))+
    ggplot2::geom_density(fill="steelblue4", color="#4a555f", alpha=0.8)+
    ggplot2::geom_hline(yintercept = 1, linetype="dashed")+
    ggplot2::labs(x="Cumulative probability", y="Density")+
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0,1.01)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, NA),
                                breaks = c(.25, .5, .75, 1)) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(axis.title.y=ggplot2::element_text(colour="black"),
          axis.title.x = ggplot2::element_text(colour="black"),
          axis.text = ggplot2::element_text(colour = "black"),
          panel.border = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(colour = "black"),
          plot.margin = ggplot2::margin(0, 0.5, 0, 0, "cm"))
}

