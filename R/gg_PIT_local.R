#' Plots Density Distributions of PIT-values for Global Calibration Diagnostics
#'
#' @description
#' A function based on ggplot2 to observe the density of PIT-values locally. It is recommended
#' to use PIT-values obtained via the `PIT_local` function from this package or an object of
#' equivalent format. For advanced customization of the plot layers, refer to the ggplot2 User Guide.This function also tests the PIT-values
#' for uniformity using the Kolmogorov-Smirnov test (`ks.test`). The p-value from the test is printed on the plot if `facet` is set to `TRUE`.
#'
#' @param pit_local A tibble with five columns: "part", "y_cal", "y_hat", "pit", and "n",
#'                  representing the partitions, calibration data, predicted values, PIT-values,
#'                  and the count of observations, respectively.
#' @param alpha Numeric value between 0 and 1 indicating the transparency of the plot fill.
#'              Default is set to 0.4.
#' @param linewidth Integer specifying the linewidth of the density line. Default is set to 1.
#' @param pal A character string specifying the RColorBrewer palette to be used for coloring
#'            the plot. Default is "Set2".
#' @param facet Logical indicating whether to use `facet_wrap()` to separate different covariate regions
#'              in the visualization. If TRUE, the p-value from the Kolmogorov-Smirnov test is printed on the plot.
#' @importFrom stats density
#' @return A `ggplot` object representing the local density distributions of PIT-values,
#'         which can be further customized through ggplot2 functions.
#' @export
#'
#' @examples
#'  n <- 10000
#'  mu <- function(x1){
#'   10 + 5*x1^2
#'   }
#'
#' sigma_v <- function(x1){
#'  30*x1
#' }
#'
#' x <- runif(n, 2, 20)
#' y <- rnorm(n, mu(x), sigma_v(x))
#'
#' x_train <- x[1:(n*0.8)]
#' y_train <- y[1:(n*0.8)]
#'
#' x_cal <- x[(n*0.8+1):n]
#' y_cal <- y[(n*0.8+1):n]
#'
#' model <- lm(y_train ~ x_train)
#'
#' y_hat <- predict(model, newdata=data.frame(x_train=x_cal))
#'
#' MSE_cal <- mean((y_hat - y_cal)^2)
#'
#' pit_local <- PIT_local(xcal = x_cal, ycal=y_cal, yhat=y_hat, mse=MSE_cal)
#'
#'gg_PIT_local(pit_local)
#'gg_PIT_local(pit_local, facet=TRUE)


gg_PIT_local <- function(pit_local,
                         alpha=0.4,
                         linewidth=1,
                         pal="Set2",
                         facet=FALSE){


  if(facet==FALSE){
    ggplot2::ggplot(pit_local)+
      ggplot2::geom_density(ggplot2::aes(x=as.numeric(dplyr::pull(pit_local[,4])),
                                         color=dplyr::pull(pit_local[,1]),
                                         fill=dplyr::pull(pit_local[,1]),
                                         ggplot2::after_stat(density)),
                            alpha=alpha,
                            linewidth=linewidth,
                            bounds = c(0, 1))+
      ggplot2::scale_color_brewer( "", palette =pal)+
      ggplot2::scale_fill_brewer("",  palette=pal)+
      ggplot2::geom_hline(yintercept = 1, linetype="dashed")+
      ggplot2::labs(x="Cumulative probability", y="Density")+
      ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0,1.01)) +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, NA),
                                  breaks = c(.25, .5, .75, 1)) +
      ggplot2::theme_classic(base_size = 12)
  }else{


    parts <- dplyr::pull(unique(pit_local[,1]))
    ks <-  do.call(cbind, purrr::map(parts, ~{
      times <- dplyr::pull(pit_local[1,5])
      ksn <- round(stats::ks.test(dplyr::pull(pit_local[(pit_local[,1])==.,4]),
                           "punif")$p.value ,3)
      ksn <- paste0("p-value ",ifelse(ksn<=0.0001,"<0.0001", ksn ))
      rep(ksn, times)
    }))
    names(ks) <- parts
    ks_l <- ks |>
      as.data.frame()|>
      tidyr::pivot_longer(dplyr::everything()) |>
      suppressMessages()

    ks_l <- ks_l |>
      dplyr::arrange(ks_l[,1])|>
      dplyr::select(-1)

    pit_local_p <- tibble::tibble(pit_local,  ks_l)

    ggplot2::ggplot(pit_local_p)+
      ggplot2::geom_density(ggplot2::aes(dplyr::pull(pit_local_p[,4]),
                                         color=dplyr::pull(pit_local_p[,1]),
                                         fill=dplyr::pull(pit_local_p[,1]),
                                         group=dplyr::pull(pit_local_p[,1]),
                                         ggplot2::after_stat(density)),
                            alpha=alpha, linewidth=linewidth,
                            bounds = c(0, 1))+
      ggplot2::geom_hline(yintercept = 1, linetype="dashed")+
      ggplot2::scale_color_brewer("", palette =pal)+
      ggplot2::scale_fill_brewer("",  palette=pal)+
      ggplot2::geom_text(ggplot2::aes(0.5, 0.5,
                                      label=dplyr::pull(pit_local_p[,6])
      ),
      inherit.aes = F,
      size=2.4)+
      ggplot2::facet_wrap(~part)+
      ggplot2::theme_classic()+
      ggplot2::labs(x="Cumulative probability", y="Density")+
      ggplot2::theme(legend.position = "none")
  }
}
