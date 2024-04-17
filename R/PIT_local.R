#' Obtain local PIT-values from a model
#'
#' @description
#' This function calculates local Probability Integral Transform (PIT) values using localized subregions of the covariate space from the calibration set.
#' The output will be used for visualization of calibration quality using the `gg_CD_local()` and `gg_PIT_local()`function.
#'
#' @param xcal Numeric matrix or data frame of features/covariates (x-values) from the calibration dataset.
#' @param ycal Numeric vector representing the true observations (y-values) of the response variable from the calibration dataset.
#' @param yhat Numeric vector of predicted response (y-hat-values) from the calibration dataset.
#' @param clusters Integer specifying the number of partitions to create for local calibration using the k-means method. Default is set to 6.
#' @param p_neighbours Proportion of xcal used to localize neighbors in the KNN method. Default is 0.2.
#' @param mse Mean Squared Error calculated from the calibration dataset.
#' @param PIT Function used to calculate the PIT-values. Default is set to `PIT_global()` from this package, that assumes a Gaussian distribution.
#'
#' @return A tibble with five columns containing unique names for each partition ("part"), "y_cal" (true observations),
#'         "y_hat" (predicted values), "pit" (PIT-values), and "n" (number of neighbors) for each partition.
#'
#' @details
#'
#' It calculates local Probability Integral Transform (PIT) values using localized subregions of the covariate space from the calibration set.
#' The centroids of such regions are derived from a k-means clustering method (from the `stats` package). The local areas around these centroids
#' are defined through an approximate k-nearest neighbors method from the `RANN` package.
#' Then, for this subregion, the PIT-values are calculated using the `PIT` function provided by the user. At the moment this function is tested to
#' work with the `PIT_global()` function from this package, which assumes a Gaussian distribution. Eventually, it can be used with other distributions.
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

