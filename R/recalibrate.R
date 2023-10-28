#' Obtain recalibrated samples of the predictive distribution
#'
#' @param xcal
#' @param xnew
#' @param y_pred
#' @param p_values
#' @param mse
#' @param n_neighboors
#' @param epsilon
#' @param method
#' @return
#' @export
#'
#' @examples
recalibrate <- function (
    xcal,
    xnew,
    yhat,
    pit_values,
    mse,
    n_neighboors=1000,
    epsilon = 1,
    method=c("gui","kuleshov1"),
    type=c("local", "global")
) {

if(!is.matrix(xnew)) {
    xnew <- matrix(xnew, nrow = 1)}

epk_kernel <- function (x) {.75 * (1 - (x / max(x))^2)}

if(method=="gui"){
  if(type=="local"){

    m <- length(y_pred)
    y_hat <- numeric()
    sd <- numeric()
    y_samples <- matrix(nrow = m,
                      ncol = n_neighboors)

    knn <- RANN::nn2(
      data = matrix(xcal),
      query = matrix(xnew),
      k = n_neighboors,
      eps = epsilon
  )
    for (j in 1:m) {
      weights <- epk_kernel(knn$nn.dists[j,])
      y_samples[j,] <- qnorm(
        pit_values[knn$nn.idx[j,]],
        mean = y_pred[j],
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
