#' QQPlot to diagnose calibration locally bases on theoretical PIT_values
#' from calibrated distributions and the empirical pit_values
#'
#' @param pit_local A dataframe obtained from PIT_local_lm
#' @param psz double that indicates size of the points that compose the lines.
#' Default is 0.001
#' @param pal Palette name from RColor Brewer. Default is "Set2'
#' @param abline Color of hotizontal line that indicates density 1. Default is"red"
#' @param facet logical value in case separate visualization is prefered. Default is F
#' @param ... Other parameters to pass ggplot
#'
#' @return a ggplot graph
#' @export
#'
#' @examples
#'
#' n <- 100000
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
#' x <- runif(n, 2, 20)
#' y <- rnorm(n, mu(x), sigma_v(x))
#'
#' x_train <- x[1:80000]
#' y_train <- y[1:80000]
#'
#' x_cal <- x[80001:100000]
#' y_cal <- y[80001:100000]
#'
#' model <- lm(y_train ~ x_train)
#'
#'pit_local <-PIT_local_lm(xcal=x_cal, ycal=y_cal, mod=model)
#'
#'gg_QQ_local(pit_local)
#'gg_QQ_local(pit_local, facet=TRUE)
#'
gg_QQ_local <- function(pit_local,
                        psz=0.01,
                        abline="black",
                        pal="Set1",
                        facet=FALSE,
                         ...){

  df <- purrr::map_dfr(unique(pit_local$part),~{

    loc <- pit_local |>
      dplyr::filter(part==.)

    purrr::map_dfr(1:nrow(loc), ~{

      pit_emp <- mean(loc[,2] <= qnorm(p=dplyr::pull(loc[.,4]),
                                       mean=dplyr::pull(loc[,3]),
                                       sd=sqrt(MSE)))
      pit_theory <- dplyr::pull(loc[.,4])
      tibble::tibble(part=loc$part[1],pit_theory, pit_emp)

    } )
  })
if(facet==F){
  ggplot2:: ggplot(df)+
    ggplot2::geom_point(ggplot2::aes(x=pit_theory, y=pit_emp,
                                     color=part),
                        size=psz)+
    ggplot2::labs(x="Predicted CDF",
                  y="Empirical CDF")+
    ggplot2::scale_color_brewer( "", palette =pal)+
    ggplot2::geom_abline(slope = 1, linetype="dashed", color=abline)+
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size=2))) +
    ggplot2::theme_classic(base_size = 14)+
    ggplot2:: theme(panel.background = ggplot2::element_rect(fill = "lightgrey"))
}else{
  ggplot2:: ggplot(df)+
    ggplot2::geom_point(ggplot2::aes(x=pit_theory, y=pit_emp,
                                     color=dplyr::pull(pit_local[,1])),
                        size=psz)+
    ggplot2::labs(x="Predicted CDF",
                  y="Empirical CDF")+
    ggplot2::scale_color_brewer( "", palette =pal)+
    ggplot2::geom_abline(slope = 1, linetype="dashed", color=abline)+
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size=2))) +
    ggplot2::facet_wrap(~part)+
    ggplot2::theme_classic(base_size = 12)+
    ggplot2:: theme(panel.background = ggplot2::element_rect(fill = "lightgrey"))


}}
