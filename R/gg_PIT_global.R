#' Plot to diagnose calibration bases on  p_values
#'
#' @description
#' A function based on ggplot2 to observe the global the density of PIT-values.
#' Layers can be edited like in http://cran.nexr.com/web/packages/ggpmisc/vignettes/user-guide-4.html.
#'
#'
#' @param pit vector of pit values
#' @param ... other static parameters passed to geom_density()
#'
#' @return a ggplot density graph
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
#' x_train <- x[1:80000]
#' y_train <- y[1:80000]
#'
#' xcal <- x[80001:100000]
#' ycal <- y[80001:100000]
#'
#' model <- lm(y_train ~ x_train)
#'
#' cdf <- CDF_model_lm(xcal = x_cal, model= model)
#'
#' pit <- PIT_global_lm( ycal = y_cal, cdf)
#'
#' gg_PIT_global(pit)
#'
#'
#'
#'

gg_PIT_global <- function(pit, ...){

  unif <- function(n){
    set.seed(1234)
    runif(n, 0,1)
  }
  ks <-  round(ks.test(pit,unif(length(pit)))$p.value,4)
  ks <- ifelse(ks==0,"<0.0001", ks )
  ks <- paste0("p-value ",ks)
  ggplot2::ggplot(NULL, ggplot2::aes(pit, ggplot2::after_stat(density)))+
    ggplot2::geom_density(fill="steelblue4", color="#4a555f",
                          alpha=0.8, bounds = c(0, 1),...)+
    ggplot2::geom_hline(yintercept = 1, linetype="dashed")+
    ggplot2::labs(x="Cumulative probability", y="Density")+
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0,1.01)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, NA),
                                breaks = c(.25, .5, .75, 1)) +
    ggplot2::theme_classic(base_size = 14) +
    ggplot2::geom_text(ggplot2::aes(.5, .5, label=ks))+
    ggplot2::theme(axis.title.y=ggplot2::element_text(colour="black"),
          plot.margin = ggplot2::margin(0, 0.5, 0, 0, "cm"))
}

