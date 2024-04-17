#' Plots Density Distributions of PIT-values for Global Calibration Diagnostics
#'
#' @description
#' This function generates a ggplot visual representation of the density of Probability Integral Transform (PIT) values globally.
#' For advanced customization of the plot layers, refer to the ggplot2 User Guide.
#'
#'
#' @param pit Vector of PIT values to be plotted.
#' @param type Character string specifying the type of plot: either "density" or "histogram".
#'             This determines the representation style of the PIT values.
#' @param fill Character string defining the fill color of the plot. Default is 'steelblue4'.
#' @param alpha Numeric value for the opacity of the plot fill, with 0 being fully transparent
#'              and 1 being fully opaque. Default is 0.8.
#' @param print_p Logical value indicating whether to print the p-value from the Kolmogorov-Smirnov test.
#'                Useful for statistical diagnostics.
#' @importFrom stats density
#' @return A `ggplot` object depicting a density graph of PIT-values, which can be further customized.
#' @export
#'
#' @details
#' This function also tests the PIT-values for uniformity using the Kolmogorov-Smirnov test (`ks.test`).
#' The p-value from the test is printed on the plot if `print_p` is set to `TRUE`.
#'
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

gg_PIT_global <- function(pit,
                          type="density",
                          fill="steelblue4",
                          alpha=0.8,
                          print_p = TRUE){

  if(!type %in% c("density","histogram")){
    stop("Invalid type of graph. Please choose between density/histogram.")}

  if( print_p == TRUE) {


    ks <-  round(stats::ks.test(pit, "punif")$p.value,4)
    ks <- ifelse(ks<0.0001,"<0.0001", ks )
    ks <- paste0("p-value ",ks)
  }

  if (type=="density"){
    if( print_p == TRUE){
      g <- ggplot2::ggplot(NULL, ggplot2::aes(pit, ggplot2::after_stat(density)))+
        ggplot2::geom_density(fill=fill, color="#4a555f",
                              alpha=alpha,  bounds = c(0, 1))+
        ggplot2::geom_hline(yintercept = 1, linetype="dashed")+
        ggplot2::labs(x="Cumulative probability", y="Density")+
        ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0,1.01)) +
        ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, NA),
                                    breaks = c(.25, .5, .75, 1)) +
        ggplot2::theme_classic(base_size = 14) +
        ggplot2::geom_text(ggplot2::aes(.5, .5, label=ks))+
        ggplot2::theme(axis.title.y=ggplot2::element_text(colour="black"),
                       plot.margin = ggplot2::margin(0, 0.5, 0, 0, "cm"))
    }else{
      g <- ggplot2::ggplot(NULL, ggplot2::aes(pit, ggplot2::after_stat(density)))+
        ggplot2::geom_density(fill=fill, color="#4a555f",
                              alpha=alpha,  bounds = c(0, 1))+
        ggplot2::geom_hline(yintercept = 1, linetype="dashed")+
        ggplot2::labs(x="Cumulative probability", y="Density")+
        ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0,1.01)) +
        ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, NA),
                                    breaks = c(.25, .5, .75, 1)) +
        ggplot2::theme_classic(base_size = 14) +
        ggplot2::theme(axis.title.y=ggplot2::element_text(colour="black"),
                       plot.margin = ggplot2::margin(0, 0.5, 0, 0, "cm"))
    }
  }

  if (type=="histogram"){
    if (print_p==T){
      g <- ggplot2::ggplot(NULL, ggplot2::aes(pit, ggplot2::after_stat(density)))+
        ggplot2::geom_histogram(fill=fill, color="#4a555f",
                                alpha=alpha)+
        ggplot2::geom_hline(yintercept = 1, linetype="dashed")+
        ggplot2::labs(x="Cumulative probability", y="Density")+
        ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0,1.01)) +
        ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, NA),
                                    breaks = c(.25, .5, .75, 1)) +
        ggplot2::theme_classic(base_size = 14) +
        ggplot2::geom_text(ggplot2::aes(.5, .5, label=ks))+
        ggplot2::theme(axis.title.y=ggplot2::element_text(colour="black"),
                       plot.margin = ggplot2::margin(0, 0.5, 0, 0, "cm"))
    }else{
      g <- ggplot2::ggplot(NULL, ggplot2::aes(pit, ggplot2::after_stat(density)))+
        ggplot2::geom_histogram(fill=fill, color="#4a555f",
                                alpha=alpha)+
        ggplot2::geom_hline(yintercept = 1, linetype="dashed")+
        ggplot2::labs(x="Cumulative probability", y="Density")+
        ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0,1.01)) +
        ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, NA),
                                    breaks = c(.25, .5, .75, 1)) +
        ggplot2::theme_classic(base_size = 14) +
        ggplot2::theme(axis.title.y=ggplot2::element_text(colour="black"),
                       plot.margin = ggplot2::margin(0, 0.5, 0, 0, "cm"))
    }
  }
return(g)
}
