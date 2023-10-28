#' Obtain recalibrated samples of the predictive distribution
#'
#' @param xcal The covariates/features of the validation set
#' @param xnew A new set of covariates, for which ther is no corresponding observation
#' @param yhat Predicted values of the calibration set
#' @param pit_values Global PIT-values of the calculated on the calibration set
#' @param mse Mean Squared Error of the calibration set
#' @param n_neighbours Number of neighbors for the local calibration. This paramether will be available when chooseing local calibration.
#' @param epsilon Approximation for the KNN method. Default epsilon=1 which returns the exact distance. This paramether will be available when chooseing local calibration
#' @param method One of the two available recalibration methods.
#' The method "gui" (Rodrigues et al 2023) is inspired in Aproximate Bayesian Computation and
#' "kuleshov1" in the recalibration method presented in (Kuleshov et al 2018).
#' @param type Choose between local of global calibration.
#' @return A list containing the calibrated predicted mean and samples of the recalibrated predictive distribution.
#' @export
#'
#' @examples
recalibrate <- function (
    xcal,
    xnew,
    yhat,
    pit_values,
    mse,
    method=c("gui","kuleshov1"),
    type=c("local", "global"),
    n_neighbours=1000,
    epsilon = 1
) {

if(!is.matrix(xnew)) {
    xnew <- matrix(xnew, nrow = 1)}

epk_kernel <- function (x) {.75 * (1 - (x / max(x))^2)}

if(method=="gui"){
  if(type=="local"){

    m <- length(yhat)
    y_hat <- numeric()
    sd <- numeric()
    y_samples <- matrix(nrow = m,
                      ncol = n_neighbours)

    knn <- RANN::nn2(
      data = matrix(xcal),
      query = matrix(xnew),
      k = n_neighbours,
      eps = epsilon
  )
    for (j in 1:m) {
      weights <- epk_kernel(knn$nn.dists[j,])
      y_samples[j,] <- qnorm(
        pit_values[knn$nn.idx[j,]],
        mean = yhat[j],
        sd = mse
    )

    y_hat[j] <- stats::weighted.mean(
      x = y_samples[j,],
      w = weights,
      na.rm = T
    )
  }

  return(
    list(
      y_hat = y_hat,
      y_sd=sd,
      y_samples = y_samples,
      y_kernel = knn$nn.dists
    )
  )

    }
  if(type=="global"){

  y_hat <- qnorm(
      pit_values,
      mean = yhat,
      sd = mse)
  return(y_hat)
  }
}
if(method=="kuleshov1"){}

}
