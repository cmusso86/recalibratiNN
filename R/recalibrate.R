#'  Generates Recalibrated Samples of the Predictive Distribution
#'
#' @description
#' This function offers recalibration techniques for regression models that assume Gaussian distributions by using the
#' Mean Squared Error (MSE) as the loss function. Based on the work by Torres R. et al. (2024), it supports
#' both local and global recalibration approaches to provide samples from a recalibrated predictive distribution. A detailed algorithm can also be found in Musso C. (2023).
#'
#' @param yhat_new Numeric vector with predicted response values for the new (or test) set.
#' @param space_cal Numeric matrix or data frame representing the covariates/features of the calibration/validation set,
#'                  or any intermediate representation (like an intermediate layer of a neural network).
#' @param space_new Similar to space_cal, but for a new set of covariates/features, ensuring they are in the same
#'                  space as those in space_cal for effective local recalibration.
#' @param pit_values Numeric vector of Global Probability Integral Transform (PIT) values calculated on the calibration set. We recommend using the PIT_global function.
#' @param mse Mean Squared Error calculated from the calibration/validation set.
#' @param p_neighbours Proportion (0,1] of the calibration dataset to be considered for determining the number of neighbors
#'                     in the KNN method. Default is set to 0.1. With p_neighbours=1, calibration is global but weighted by distance.
#' @param epsilon Numeric value for approximation in the K-nearest neighbors (KNN) method. Default is 0, indicating exact distances.
#' @param type Character string to choose between 'local' or 'global' calibration.
#'
#' @return A list containing the calibrated predicted mean and variance, along with samples from the recalibrated predictive distribution
#'         and their respective weights calculated using an Epanechnikov kernel over the distances obtained from KNN.
#'
#' @importFrom magrittr %>%
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @details
#'
#' The calibration technique implemented here draws inspiration from Approximate Bayesian Computation and Inverse Transform Theorem,
#' allowing for recalibration either locally or globally. The global method employs a uniform kernel, while the local method employs an Epanechnikov kernel.
#'
#' It's important to note that the least squares method will only yield a probabilistic interpretation if the output to be modeled
#' follows a normal distribution, and this assumption was used to implement this function.
#'
#' The local recalibration method is expected to improve the predictive performance of the model, especially when the model is not able to capture the heteroscedasticity of the data.
#' However, there is a trade off between refinement of localization and the Monte Carlo error, which can be controlled by the number of neighbors.
#' That is, when more localized, the recalibration will grasp local changes better, but the Monte Carlo error will increase, because of the reduced number of neighbors.
#'
#' @details
#' When p_neighbours=1, recalibration is performed using the entire calibration dataset but with distance-weighted contributions.
#'
#'
#' @references
#' \insertRef{torres2024}{recalibratiNN}
#' \insertRef{musso2023}{recalibratiNN}
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


