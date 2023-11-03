#' Plot to diagnose calibration bases on  p_values
#'
#' @description
#' A function based on ggplot2 to observe the global the density of PIT-values.
#' For more detailed edition of layers a posteriori, please refer to ggplot2 User Guide.
#'
#'
#' @param pit vector of pit values
#' @param ... other static parameters passed to geom_density()
#'
#' @return a ggplot density graph
#' @import stats
#' @import ggplot2
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
#' pit <- PIT_global(ycal=y_cal, yhat=y_hat, mse=MSE_cal)
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
  ks <- ifelse(ks<0.0001,"<0.0001", ks )
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

