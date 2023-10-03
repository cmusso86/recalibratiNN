#' Creates pit values for k partition of the calibration set
#'
#' @param xcal Covariates (features) vector/matrix of the calibration set
#' @param ycal Output/Observations vector of the calibration set
#' @param clusters Number of local clusters one whats to evaluate
#' @param n_neighboor Number of neighbors for the mean of each cluster
#' @param fx Predictive Cumulative Distribution of the fitted lm() model.
#' @param pit Function that returns pit values of a lm() model.
#'  correspond to the pit values for each partition.
#' @param mod A lm() model.
#'
#' @return Data Frame of pit-values in different partitions.
#' @export
#'
#' @examples
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
#'PIT_local_lm(xcal=x_cal, ycal=y_cal, mod=model)
#'
#'
#'
PIT_local_lm <- function(xcal, ycal,  clusters=5,
                         n_neighboor=1000, fx=CDF_model_lm,
                         pit=PIT_values_lm,
                         mod){

  # Select centroids
  cluster_means <- sort(stats::kmeans(xcal, clusters, iter.max=10000)$centers)

  #Select neighboors
  knn <- RANN::nn2(xcal, cluster_means,  k=n_neighboor)$nn.idx

  # get gorresponding neighboors in data
  x_cal_local <- purrr::map(1:nrow(knn), ~  xcal[knn[.,]] )
  y_cal_local <- purrr::map(1:nrow(knn),  ~ycal[knn[.,]])

  pit_val <- purrr::map_dfc(1:length(x_cal_local), ~{
    pit(cdf=fx(x_cal_local[[.]], model),
        y_cal=y_cal_local[[.]])
  })

  names(pit_val) <- stringr::str_c("var_", (seq(1,length(x_cal_local))))
  pit_val |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "part",
                        values_to = "pit")
}


