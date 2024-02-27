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
#' @param p_neighbours Proportion of xcal to localize neighboors in the KNN method. Default is 0.2.
#' @param mse Mean Squared Error of the model
#' @param PIT function to return the PIT-values. Default set to PIT_global() from this package.
#'
#' @return A tibble with five containing in the first column containing unique names for the partition, "y_cal",
#' the second column containing the yhat the third the pit-values  and the last the number of neighbors in each partition.
#'
#' @export
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
#'sigma_v <- function(x1){
#'  30*x1
#'}
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
#' PIT_local(xcal = x_cal, ycal=y_cal, yhat=y_hat, mse=MSE_cal)
#'


PIT_local <- function(xcal, ycal,
                      yhat,mse,  clusters=6,
                      p_neighbours=0.2,
                      PIT=PIT_global
){


  if(!is.matrix(xcal)){xcal<-as.matrix(xcal)}
  n_neighbours = trunc(p_neighbours*nrow(xcal))
  # Select centroids
  cluster_means_cal <- stats::kmeans(xcal,clusters)$centers
  cluster_means_cal <- cluster_means_cal[order(cluster_means_cal[,1]),]

  #Select neighboors
  knn_cal <- RANN::nn2(xcal, cluster_means_cal,  k=n_neighbours)$nn.idx

  # get corresponding neighbors in data
  y_cal_local <- purrr::map(1:nrow(knn_cal),  ~ycal[knn_cal[.,]])
  y_hat_local <-purrr::map(1:nrow(knn_cal),  ~yhat[knn_cal[.,]])
  if(length(mse)>1){mse_wt <- purrr::map(1:nrow(knn_cal),  ~mse[knn_cal[.,]])}else{mse_wt <- mse}

  # calculate pit_local
  pit_val <- purrr::map_dfr(1:length(y_cal_local), ~{

    if(length(mse_wt)>1){
    pit=PIT( ycal = y_cal_local[[.]],
             yhat=y_hat_local[[.]],
            mse_wt[[.]])
    }else{
      pit=PIT( ycal = y_cal_local[[.]],
                       yhat=y_hat_local[[.]],
                       mse_wt)
            }

    tibble::tibble(part=glue::glue("part_{.}"),
                   y_cal=y_cal_local[[.]],
                   y_hat=y_hat_local[[.]],
                   pit,
                   n=n_neighbours)
  })
  return(pit_val)
}

