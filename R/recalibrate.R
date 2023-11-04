#' Obtain recalibrated samples of the predictive distribution.
#'
#' @description
#' This function offers two approaches to obtain samples and the mean of a
#' recalibrated predictive distribution for any regression model, using Mean Squared Error (MSE) as the loss function.
#'
#' @param entry_cal The covariates/features of the calibration/validation
#' set or any representation of those covariates, such as an intermediate layer or an output layer of a neural network.
#' @param entry_new A new set of covariates or other representation of those covariates, provided they are in the same space as the ones in entry_cal.
#' @param output_new_hat Predicted values of the new (test) set
#' @param pit_values Global Probability Integral Transform (PIT) values calculated on the calibration set..
#' @param mse Mean Squared Error of the calibration/validation set
#' @param p_neighbours Proportion of the entry_cal to be used as number of neighboors for the KNN. If p_neighbours=1 calibration but weighted by distance. Default is set to 0.1.
#' @param epsilon Approximation for the K-nearest neighbors (KNN) method. Default is epsilon = 1, which returns the exact distance. This parameter is available when choosing local calibration.
#' @param method Choose one of the two available recalibration methods.
#' @param type Choose between local or global calibration.
#'
#' @return A list containing the calibrated predicted mean and samples of the recalibrated predictive distribution.
#' For local calibration, the weighted variance and the weights are also provided.
#'
#' @export
#'
#' @details
#' The methods are designed to generate recalibrated samples from regression models that have been fitted using the least-squares method.
#' It's important to note that the least-squared method will only render a probabilistic interpretation if the output to be modeled follows
#' a normal distribution, and that assumption was used to implement this methods.
#' One of the available methods, "torres" (Torres et al. 2023), draws inspiration from Approximate Bayesian Computation
#' and the Inverse Transform Theory. The other method, "kuleshov1," is based on recalibration techniques presented in (Kuleshov et al. 2018), which core concept
#' involves training an auxiliary model such that the combination of this model with the uncalibrated predictive distribution results in a calibrated distribution.
#' Both of these methods can be applied either globally or locally.
#'
#' @examples
#'
#' n <- 10000
#' split <- 0.8
#'
#' # generating heterocedastic data
#' mu <- function(x1){
#' 10 + 5*x1^2
#' }
#'
#' sigma_v <- function(x1){
#' 30*x1
#' }
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
#' x_new <- runif(n/5, 1, 10)
#'
#' model <- lm(y_train ~ x_train)
#'
#' y_hat_cal <- predict(model, newdata=data.frame(x_train=x_cal))
#' MSE_cal <- mean((y_hat_cal - y_cal)^2)
#'
#' y_hat_new <- predict(model, newdata=data.frame(x_train=x_new))
#'
#' pit <- PIT_global(ycal=y_cal, yhat= y_hat_cal, mse=MSE_cal)
#'
#' recalibrated <- recalibrate(
#'   entry_cal=x_cal,
#'   entry_new=x_new,
#'   output_new_hat=y_hat_new,
#'   pit_values=pit,
#'   mse= MSE_cal,
#'   method="torres",
#'   type="local")
#'

recalibrate <- function (
    entry_cal,
    entry_new,
    output_new_hat,
    pit_values,
    mse,
    method=c("torres","kuleshov1"),
    type=c("local", "global"),
    p_neighbours=0.1,
    epsilon = 1
) {


  if(!is.matrix(entry_cal)){entry_cal<-as.matrix(entry_cal)}
  if(!is.matrix(entry_new)){entry_new<-as.matrix(entry_new)}

m <- length(output_new_hat)
epk_kernel <- function (x) {.75 * (1 - (x / max(x))^2)}
n_neighbours <- trunc(p_neighbours*nrow(entry_new))

if(method=="torres"){


    if(type=="local"){

        knn <- RANN::nn2(
          data = entry_cal,
          query = entry_new,
          k = n_neighbours,
          eps = epsilon)

        wts <- do.call(rbind, map(1:m, ~{epk_kernel(knn$nn.dists[.,])}))


      y_samples <-  do.call(
        rbind,
        purrr::map(1:m, ~ {
          qnorm(
            pit_values[knn$nn.idx[.,]],
            mean = output_new_hat[.],
            sd = sqrt(mse)
          )}))

      y_hat_cal <-  purrr::map_dbl(1:m,~{
        stats::weighted.mean(
          x = y_samples[.,],
          w = wts[.,],
          na.rm = T)})

      mse_wt <- purrr::map_dbl(1:m,~{
        Hmisc::wtd.var(
          y_samples[.,],
          w = wts[.,],
          na.rm = T)})

    return(
      list(
        y_hat_calibrated = y_hat_cal,
        mse_weighted=mse_wt,
        y_samples_calibrated = y_samples,
        y_kernel = knn$nn.dists
      )
    )

  }
  if(type=="global"){

    # colocar um for
 y_samples_calibrated <-do.call(rbind,
                                 purrr::map(1:m, ~qnorm(
        pit_values,
        mean = output_new_hat[.],
        sd = sqrt(mse))))
 N <- ncol(y_samples_calibrated)
 y_hat_cal <- rowSums(y_samples_calibrated)/N
 y_var_cal <- rowSums((y_samples_calibrated-y_hat_cal)^2)/(N-1)

    return(
      list(
        y_hat_calibrated = y_hat_cal,
        y_var_cal=y_var_cal,
        y_samples_cal = y_samples_calibrated
      )
    )
  }
}
if(method=="kuleshov1"){
  if(type=="global"){

  }
}

}
