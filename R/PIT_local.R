#' Obtain local PIT-values from a model
#'
#' @description
#' Return local PIT-values. Centroids for localization is obtained by k-means method from stats package.
#' The vicinity of such centroids are selected though a aproximate k-nearst neighboors method from RANN package.
#'
#' @param xcal features/covariates from calibration set
#' @param ycal observations of calibration set
#' @param yhat predicted outputs from the calibrations et
#' @param clusters Number of partitions to create for local calibration. Centroids calculated by k-means method.
#' Default set to 6.
#' @param n_neighboor Number of neighboors in the KNN method.
#' @param mse Mean Squared Error of the model
#' @param PIT function to return the PIT-values. Default set to PIT_global() from this package.

#'
#' @return A tibble with five column names "part", "y_cal",
#' "y_hat", "pit" and "n"
#'
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
#'x_train <- x[1:80000]
#'y_train <- y[1:80000]
#'
#'x_cal <- x[80001:100000]
#'y_cal <- y[80001:100000]
#'
#'model <- lm(y_train ~ x_train)
#' y_hat <- predict(model, newdata=data.frame(x_test=x_cal))
#' MSE <- (summary(model)$sigma)^2
#' PIT_local(xcal = x_cal, ycal=y_cal, yhat=y_hat, mse=MSE)
#'


PIT_local <- function(xcal, ycal,
                      yhat,mse,  clusters=6,
                      n_neighboor=1000,
                      PIT=PIT_global
){

  # Select centroids
  cluster_means_cal <- data.frame(stats::kmeans(xcal,
                                                clusters)$centers)
  cluster_means_cal <- dplyr::arrange(cluster_means_cal,
                                      cluster_means_cal[,1])|>
    dplyr::pull()


  #Select neighboors
  knn_cal <- RANN::nn2(xcal, cluster_means_cal,  k=n_neighboor)$nn.idx

  # get gorresponding neighboors in data
  y_cal_local <- purrr::map(1:nrow(knn_cal),  ~ycal[knn_cal[.,]])
  y_hat_local <-purrr::map(1:nrow(knn_cal),  ~yhat[knn_cal[.,]])

  # calculate pit_local
  pit_val <- purrr::map_dfr(1:length(y_cal_local), ~{
    pit <- PIT( ycal = y_cal_local[[.]], yhat=y_hat_local[[.]],
                mse=mse)
    tibble::tibble(part=glue::glue("part_{.}"),y_cal=y_cal_local[[.]],
                   y_hat=y_hat_local[[.]], pit, n=n_neighboor)
  })
  return(pit_val)
}

