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
#' x_test <- x[1:80000]
#' y_test <- y[1:80000]
#'
#' x_cal <- x[80001:100000]
#' y_cal <- y[80001:100000]
#'
#' model <- lm(y_test ~ x_test)
#'
#'pit_local <-PIT_local_lm(xcal=x_cal, ycal=y_cal, mod=model)
#'
#'gg_QQ_local(pit_local)
#'gg_QQ_local(pit_local, facet=TRUE)
#'
gg_QQ_local <- function(pit_local,
                        psz=0.01,
                        abline="red",
                        pal="Set1",
                        facet=FALSE,
                         ...){

  pit_theory <- sort(stats::qunif(seq(0,1, by=(1/(dplyr::pull(pit_local[1,3])-1)))))
  pit_emp <- purrr:::map_dfc(dplyr::pull(unique(pit_local[,1])),
              ~sort(dplyr::pull(pit_local[(pit_local[,1])==.,2]))
  )
  names(pit_emp) <- stringr::str_c("part_", (seq(1,length(pit_emp))))


  df <- data.frame(pit_theory=pit_theory, pit_emp)
  df_long <- df |>
    tidyr::pivot_longer(-pit_theory, names_to = "part",
                        values_to = "pit_emp")
if(facet==F){
  ggplot2:: ggplot(df_long)+
    ggplot2::geom_point(ggplot2::aes(x=pit_theory, y=pit_emp,
                                     color=part),
                        size=psz)+
    ggplot2::labs(x="Predicted CDF",
                  y="Empirical CDF")+
    ggplot2::scale_color_brewer( "", palette =pal)+
    ggplot2::geom_abline(slope = 1, linetype="dashed", color=abline)+
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size=2))) +
    ggplot2::theme_classic(base_size = 14)
}else{
  ggplot2:: ggplot(df_long)+
    ggplot2::geom_point(ggplot2::aes(x=pit_theory, y=pit_emp,
                                     color=part),
                        size=psz)+
    ggplot2::labs(x="Predicted CDF",
                  y="Empirical CDF")+
    ggplot2::scale_color_brewer( "", palette =pal)+
    ggplot2::geom_abline(slope = 1, linetype="dashed", color=abline)+
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size=2))) +
    ggplot2::facet_wrap(~part)+
    ggplot2::theme_classic(base_size = 12)


}}
