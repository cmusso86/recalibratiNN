#' Title
#'
#' @param pit vector of pit values
#'
#' @return
#' @export
#'
#' @examples
gg_globcal_hist <- function(pit, y_cal){


  ggplot2::ggplot() +
    ggplot2::geom_histogram(ggplot2::aes(pit),
                            color="black",
                            fill="steelblue3",
                            alpha=0.8,
                            linewidth=0.25) +
    ggplot2::theme_classic()+
    ggplot2::xlab("PIT values")+
    ggplot2::ylab("")
}


gg_cal_qq <- function(pit){
  pit_theory <- sort(qunif(seq(0,1, by=(1/(length(y_cal)-1)))))
  pit_emp <- sort(pit)
  df <- data_frame(pit_theory=pit_theory, pit=pit_emp)

  ggplot(df)+
    geom_point(aes(x=pit_theory, y=pit), color="#4B5D5D", size=0.01)+
    labs(x="Predicted CDF",
         y="Empirical CDF")+
    geom_abline(slope = 1, linetype="dashed", color="red")+
    theme_bw(base_size = 14) +
    theme(axis.title.y=element_text(colour="black"),
          axis.title.x = element_text(colour="black"),
          axis.text = element_text(colour = "black"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.margin = margin(0,0.5, 0,0, "cm"))
}
