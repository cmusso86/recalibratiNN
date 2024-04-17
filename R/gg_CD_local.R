#' Plots the cumulative distributions of PIT-values for local calibration diagnostics.
#'
#'@description
#'
#' This function generates a ggplot visual representation to compare the predicted versus empirical
#' cumulative distributions of Probability Integral Transform (PIT) values at a local level. It is
#' useful for diagnosing the calibration in different regions within the dataset, since miscalibration patterns may
#' differ across the covariate space. The function allows for customization of the plot layers to suit specific needs.
#' For advanced customization of the plot layers, refer to the ggplot2 User Guide.
#'
#'@param pit_local A data frame of local PIT-values, typically obtained from `PIT_local()`.
#'@param psz Double indicating the size of the points on the plot. Default is 0.001.
#'@param pal Palette name from RColorBrewer for coloring the plot. Default is "Set2".
#'@param abline Color of the diagonal line. Default color is "red".
#'@param facet Logical value indicating if a separate visualization for each subgroup is preferred. Default is FALSE.
#'@param ... Additional parameters to customize the ggplot.
#'
#'@return A `ggplot` object displaying the cumulative distributions of PIT-values that that can be customized as needed.
#'
#'@details
#'
#' This funcion will work with the output of the `PIT_local()` function, which provides the PIT-values for each subgroup pf the covariate space in the appropriate format.
#'
#'@export
#'
#' @examples
#'
#' n <- 10000
#' split <- 0.8
#'
#' mu <- function(x1){
#' 10 + 5*x1^2
#' }
#'
#' sigma_v <- function(x1){
#' 30*x1
#' }
#'
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
#' pit_local <- PIT_local(xcal = x_cal, ycal=y_cal, yhat=y_hat, mse=MSE_cal)
#'
#' gg_CD_local(pit_local)
#' gg_CD_local(pit_local, facet=TRUE)
#'
gg_CD_local <- function(pit_local,
                        psz=0.01,
                        abline="black",
                        pal="Set2",
                        facet=FALSE,
                         ...){

  df <- do.call(rbind, purrr::map(unique(pit_local$part),~{

    loc <- pit_local |>
      dplyr::filter(part==.)

  do.call(rbind,
          purrr::map(1:nrow(loc), ~{

    c(part=loc$part[1],
      pit_emp =mean(loc[,2] <= qnorm(p=dplyr::pull(loc[.,4]),
                                       mean=dplyr::pull(loc[,3]),
                                       sd=sqrt(MSE_cal))),
      pit_pred = dplyr::pull(loc[.,4]))
    }))
  })) |>  as.data.frame()

  df[,2] <- as.numeric(df[,2])
  df[,3] <- as.numeric(df[,3])


if(facet==F){
  ggplot2:: ggplot(df)+
    ggplot2::geom_point(ggplot2::aes(x=df[,3], y=df[,2],
                                     color=df[,1]),
                        size=psz)+
    ggplot2::labs(x="Predicted",
                  y="Empirical")+
    ggplot2::scale_color_brewer( "", palette =pal)+
    ggplot2::geom_abline(slope = 1, linetype="dashed", color=abline)+
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size=2))) +
    ggplot2::theme_classic(base_size = 14)
}else{
  ggplot2:: ggplot(df)+
    ggplot2::geom_point(ggplot2::aes(x=df[,3], y=df[,2],
                                     color=df[,1]),
                        size=psz)+
    ggplot2::labs(x="Predicted",
                  y="Empirical")+
    ggplot2::scale_color_brewer( "", palette =pal)+
    ggplot2::geom_abline(slope = 1, linetype="dashed", color=abline)+
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size=2))) +
    ggplot2::facet_wrap(~part)+
    ggplot2::theme_classic(base_size = 12)


}}
