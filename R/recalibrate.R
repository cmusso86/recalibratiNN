#' Obtain recalibrated samples of the predictive distribution.
#'
#' @description
#' This function currently offers one recalibration technique, based on the method by Torres R. et al. (2024).
#' It offers two approaches (local and global) to obtain samples and the mean of a recalibrated predictive distribution for any regression Gaussian model that used Mean Squared Error (MSE) as the loss function.
#'
#' @param yhat_new Predicted values of the new (test) set.
#' @param space_cal Used in local recalibration. The covariates/features of the calibration/validation
#' set or any representation of those covariates, such as an intermediate layer or an output layer of a neural network.
#' @param space_new Used in local recalibration. A new set of covariates or other representation of those covariates,
#'  provided they are in the same space as the ones in space_cal.
#' @param pit_values Global Probability Integral Transform (PIT) values calculated on the calibration set.
#' @param mse Mean Squared Error of the calibration/validation set.
#'  which extremes corresponds to the usual extremes for a 95% confidence interval and the central value corresponds to the median.
#' @param p_neighbours Double between (0,1] that represents the proportion of the x_cal is to be used as the number of neighboors for the KNN.
#' If p_neighbours=1 calibration but weighted by distance. Default is set to 0.1.
#' @param epsilon Approximation for the K-nearest neighbors (KNN) method. Default is epsilon = 0, which returns the exact distance. This parameter is available when choosing local calibration.
#' @param type Choose between local or global calibration.

#' @return A list containing the calibrated predicted mean/variance along with samples
#' from the recalibrated predictive distribution with its respective weights. Weights are calculated with an Epanechnikov kernel.
#' over the distances obtained from KNN.
#'
#' @importFrom magrittr %>%
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @details
#' The method implemented here is designed to generate recalibrated samples from regression models that have been fitted using the least-squares method.
#' It's important to note that the least-squared method will only render a probabilistic interpretation if the output to be modeled follows
#' a normal distribution, and that assumption was used to implement this method.
#'
#' The current available methods, draws inspiration from Approximate Bayesian Computation and the Inverse Transform Theory.
#' The calibration methods can be applied either locally or globally. When tipe="global", the calibration will use a uniform kernel.
#'
#' Alternatively, one can choose the "local" calibration with a p_neighbours=1. This way, the calibration will use the whole calibration set (that is, globally),
#' but instead of an uniform kernel, it will use a Epanechnikov kernel.
#'
#' @references
#' \insertRef{torres2024}{recalibratiNN}
#'
#' @examples
#'
#' n <- 1000
#' split <- 0.8
#'
#' # Auxiliary functions
#' mu <- function(x1){
#' 10 + 5*x1^2
#' }
#'
#' sigma_v <- function(x1){
#' 30*x1
#' }
#'
#' # Generating heteroscedastic data.
#' x <- runif(n, 1, 10)
#' y <- rnorm(n, mu(x), sigma_v(x))
#'
#' # Train set
#' x_train <- x[1:(n*split)]
#' y_train <- y[1:(n*split)]
#'
#' # Calibration/Validation set.
#' x_cal <- x[(n*split+1):n]
#' y_cal <- y[(n*split+1):n]
#'
#' # New observations or the test set.
#' x_new <- runif(n/5, 1, 10)
#'
#' # Fitting a simple linear regression, which will not capture the heteroscedasticity
#' model <- lm(y_train ~ x_train)
#'
#' y_hat_cal <- predict(model, newdata=data.frame(x_train=x_cal))
#' MSE_cal <- mean((y_hat_cal - y_cal)^2)
#'
#' y_hat_new <- predict(model, newdata=data.frame(x_train=x_new))
#'
#' pit <- PIT_global(ycal=y_cal, yhat= y_hat_cal, mse=MSE_cal)
#'
#' recalibrate(
#'   space_cal=x_cal,
#'   space_new=x_new,
#'   yhat_new=y_hat_new,
#'   pit_values=pit,
#'   mse= MSE_cal,
#'   type="local")
#'



recalibrate <- function(
    yhat_new,
    pit_values,
    mse,
    space_cal=NULL,
    space_new=NULL,
    type=c("local", "global"),
    p_neighbours=0.1,
    epsilon = 0
) {



  if(type=="local"&(is.null(space_cal)|is.null(space_new))){stop("Local calibration needs a space both in the calibration and test/new set to find the nearst neighbors.")}
  if(!type %in% c("global","local")){stop("Invalid type of recalibration. Please choose between global/local.")}
  if(type=="local"&!is.matrix(space_cal)){space_cal<-as.matrix(space_cal)}
  if(type=="local"&!is.matrix(space_new)){space_new<-as.matrix(space_new)}
  if(!is.numeric(yhat_new)){yhat_new<-as.numeric(yhat_new)}

  m <- length(yhat_new)
  epk_kernel <- function (x) {.75 * (1 - (x / max(x))^2)}
  n_neighbours <- trunc(p_neighbours*nrow(space_cal))

    if(type=="local"){

      knn <- RANN::nn2(
        data = space_cal,
        query = space_new,
        k = n_neighbours,
        eps = epsilon)

    wts <- do.call(rbind, purrr::map(1:m, ~{epk_kernel(knn$nn.dists[.,])}))


    y_samples_raw <-  do.call(
        rbind,
        purrr::map(1:m, ~ {
          qnorm(
            pit_values[knn$nn.idx[.,]],
            mean = yhat_new[.],
            sd = sqrt(mse)
          )}))

      y_samples_wt <- do.call(rbind,
                              purrr::map(1:nrow(y_samples_raw), ~{
                                sample(y_samples_raw[.,],
                                       size=nrow(y_samples_raw),
                                       prob =wts[.,],
                                       replace=T)
      }))


      y_hat_cal <-  purrr::map_dbl(1:m,~{
         stats::weighted.mean(
          x = y_samples_raw[.,],
          w=wts[.,],
          na.rm = T)})

      y_var_cal <-  purrr::map_dbl(1:m,~{
        Hmisc::wtd.var(
          x = y_samples_raw[.,],
          w=wts[.,],
          na.rm = T)}
      )


      return(
        list(
          y_hat_calibrated = y_hat_cal,
          y_var_calibrated = y_var_cal,
          y_samples_calibrated_wt = y_samples_wt,
          y_samples_calibrated_raw = y_samples_raw,
          y_kernel = knn$nn.dists
        )
      )

    }
    if(type=="global"){

      # colocar um for
      y_samples_calibrated <-do.call(rbind,
                          purrr::map(1:m, ~qnorm(
                            pit_values,
                            mean = yhat_new[.],
                            sd = sqrt(mse))))

     N <- ncol(y_samples_calibrated)
     y_hat_cal <- rowSums(y_samples_calibrated)/N
     y_var_cal <- rowSums((y_samples_calibrated-y_hat_cal)^2)/(N-1)

      return(
        list(
          y_hat_calibrated = y_hat_cal,
          y_var_calibrated=y_var_cal,
          y_samples_calibrated = y_samples_calibrated
        )
      )
    }

  }


