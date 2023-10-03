#' Density grsph for local PIT
#'
#' @param pit_local Data frame with two collumn names "part" and "pit"
#' @param alpha double 0-1 to indicate transparency of fill
#' @param linewidth integer linewidth of density line
#' @param pal a chosen RBrewer color pallete
#' @param facet Logical
#' @param ... Other parameters for ggplot()
#'
#' @return A ggplot
#' @export
#'
#' @examples
#' n <- 100000
#' mu <- function(x1){
#' 10 + 5*x1^2
#' }
#'
#'sigma_v <- function(x1){
#'  30*x1
#'}
#'
#'x <- runif(n, 2, 20)
#'y <- rnorm(n, mu(x), sigma_v(x))
#'
#'x_test <- x[1:80000]
#'y_test <- y[1:80000]
#'
#'x_cal <- x[80001:100000]
#'y_cal <- y[80001:100000]
#'
#'model <- lm(y_test ~ x_test)
#'
#'pit_local <- PIT_local_lm(xcal=x_cal, ycal=y_cal, mod=model)
#'
#'gg_local_pit(pit_local)


gg_local_pit <- function(pit_local,
                         alpha=0.4,
                         linewidth=1,
                         pal="Set2",
                         facet=F,
                         ...){

if(facet==F){
ggplot2::ggplot(pit_local)+
    ggplot2::geom_density(ggplot2::aes(x=pit, color=part, fill=part,
                                       ggplot2::after_stat(density)),
               alpha=alpha, linewidth=linewidth, ...)+
  ggplot2::scale_color_brewer( "", palette =pal)+
  ggplot2::scale_fill_brewer("",  palette=pal)+
  ggplot2::geom_hline(yintercept = 1, linetype="dashed")+
  ggplot2::labs(x="Cumulative probability", y="Density")+
  ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0,1.01)) +
  ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, NA),
                              breaks = c(.25, .5, .75, 1)) +
  ggplot2::theme_classic(base_size = 12)
}else{
  ggplot2::ggplot(pit_local)+
    ggplot2::geom_density(ggplot2::aes(pit, color=part,
                                       fill=part,
                                       ggplot2::after_stat(density)),
                 alpha=alpha, linewidth=linewidth)+
    ggplot2::geom_hline(yintercept = 1, linetype="dashed")+
    ggplot2::scale_color_brewer("", palette =pal)+
    ggplot2::scale_fill_brewer("",  palette=pal)+
    ggplot2::facet_wrap(~part)+
    ggplot2::theme_classic()+
    ggplot2::labs(x="Cumulative probability", y="Density")+
    ggplot2::theme(legend.position = "none")
}
}


