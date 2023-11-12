#' Obtain recalibrated samples of the predictive distribution using Torres et al. (2024) method.
#'
#' @description
#' This function offers two approaches to obtain samples and the mean of a
#' recalibrated predictive distribution for any regression model, using Mean Squared Error (MSE) as the loss function.
#'
#' @param yhat_new Predicted values of the new (test) set.
#' @param yhat_cal Predicted values of the calibration/validation set. It is only used in the "kuleshov" method.
#' @param ycal True observations of the calibration set. This parameter will be required when used in the Kulheshov method.
#' @param space_cal Used in local recalibration. The covariates/features of the calibration/validation
#' set or any representation of those covariates, such as an intermediate layer or an output layer of a neural network.
#' @param space_new Used in local recalibration. A new set of covariates or other representation of those covariates, provided they are in the same space as the ones in space_cal.
#' @param pit_values Global Probability Integral Transform (PIT) values calculated on the calibration set..
#' @param mse Mean Squared Error of the calibration/validation set.
#' @param cum_prob A numeric vector of length 3 containing the cumulative probabilities to obtain the respective quantiles. Default is set to c(0.025, 0.5, 0.975),
#'  which extremes corresponds to the usual extremes for a 95% confidence interval and the central value corresponds to the median.
#' @param p_neighbours Double between (0,1] that represents the proportion of the x_cal is to be used as the number of neighboors for the KNN. If p_neighbours=1 calibration but weighted by distance. Default is set to 0.1.
#' @param epsilon Approximation for the K-nearest neighbors (KNN) method. Default is epsilon = 1, which returns the exact distance. This parameter is available when choosing local calibration.
#' @param method Choose one of the two available recalibration methods.
#' @param type Choose between local or global calibration.
#'
#' @return For the "torres" method, list containing the calibrated predicted mean/variance along with samples
#' from the recalibrated predictive distribution with its respective weights.
#' In the case of the Kuleshov method, the confidence intervals and median are provided instead of the mean/variance.
#'
#' @importFrom magrittr %>%
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
#' recalibrate(
#'   space_cal=x_cal,
#'   space_new=x_new,
#'   yhat_new=y_hat_new,
#'   pit_values=pit,
#'   mse= MSE_cal,
#'   method="torres",
#'   type="local")
#'
#' recalibrate(
#'   yhat_new=y_hat_new,
#'   yhat_cal = y_hat_cal,
#'   ycal = y_cal,
#'   space_cal=x_cal,
#'   space_new=x_new,
#'   pit_values=pit,
#'   mse= MSE_cal,
#'   method="torres",
#'   type="local")
#'

recalibrate <- function (
    yhat_new,
    yhat_cal=NULL,
    ycal = NULL,
    space_cal=NULL,
    space_new=NULL,
    pit_values,
    mse,
    cum_prob = c(0.025, 0.5, 0.975),
    method=c("torres","kuleshov1"),
    type=c("local", "global"),
    p_neighbours=0.1,
    epsilon = 1
) {


  if(type=="local"&(is.null(space_cal)|is.null(space_new))){stop("Local calibration needs a space both in the calibration and test/new set to find the nearst neighbors.")}
  if(method=="kuleshov1"&(is.null(ycal))){stop("Kuleshov calibration needs the ycal observations to calculate empirical distribution of pit-values.")}
  if(!method %in% c("torres","kuleshov1")){stop("Invalid method. Please choose between torres/kuleshov1.")}
  if(!type %in% c("global","local")){stop("Invalid type of recalibration. Please choose between global/local.")}
  if(!is.matrix(space_cal)){space_cal<-as.matrix(space_cal)}
  if(!is.matrix(space_new)){space_new<-as.matrix(space_new)}

  m <- length(yhat_new)
  epk_kernel <- function (x) {.75 * (1 - (x / max(x))^2)}
  n_neighbours <- trunc(p_neighbours*nrow(space_cal))

  if(method=="torres"){
    if(type=="local"){

      knn <- RANN::nn2(
        data = space_cal,
        query = space_new,
        k = n_neighbours,
        eps = epsilon)

    wts <- do.call(rbind, map(1:m, ~{epk_kernel(knn$nn.dists[.,])}))


    y_samples_raw <-  do.call(
        rbind,
        purrr::map(1:m, ~ {
          qnorm(
            pit_values[knn$nn.idx[.,]],
            mean = yhat_new[.],
            sd = sqrt(mse)
          )}))

      y_samples_wt <- do.call(rbind,
                              map(1:nrow(y_samples_raw), ~{
                                sample(y_samples_raw[.,],
                                       prob =knn$nn.dists[.,],
                                       replace=T)
      }))


      y_hat_cal <-  purrr::map_dbl(1:m,~{
         mean(
          x = y_samples_wt[.,],
          na.rm = T)})

      y_var_cal <-  purrr::map_dbl(1:m,~{
        var(
          x = y_samples_wt[.,],
          na.rm = T)})


      return(
        list(
          y_hat_calibrated = y_hat_cal,
          y_var_calibrated = y_var_cal,
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
  if(method=="kuleshov1"){
    if(is.null(yhat_cal)){stop("The Kuleshov method needs the yhat_cal argument.")}
    if(is.null(ycal)){stop("The Kuleshov method needs the y_cal argument.")}

    if(type=="local"){

      knn <- RANN::nn2(
        data = space_cal,
        query = space_new,
        k = n_neighbours,
        eps = epsilon)

   wts <- do.call(rbind, map(1:m, ~{epk_kernel(knn$nn.dists[.,])}))

   quantil_local <- do.call(rbind, map(1:length(yhat_new), ~{

    pit_local <- pit_values[knn$nn.idx[.,]]
    y_hat_cal_local <- yhat_cal[knn$nn.idx[.,]]
    y_cal_local <- y_cal[knn$nn.idx[.,]]

    df <- do.call(rbind, map(1:length(pit_local), ~{
        emp <- mean(ycal <= qnorm(p=pit_local[.] ,
                                   mean=yhat_cal,
                                   sd=sqrt(mse)))
        estim <- pit_local[.]
        c(estim, emp)
      } ))

    ir_local <- stats::isoreg(df[,1], df[,2])

    df_trat <- df %>%
      as.data.frame()%>%
      mutate(recal=as.stepfun(ir_local)(.[[1]])) %>%
      dplyr::mutate(dif1=abs(.[[3]]-cum_prob[1]),
                    dif2=abs(.[[3]]-cum_prob[2]),
                    dif3=abs(.[[3]]-cum_prob[3])) %>%
      dplyr::filter(.[[4]]==min(.[[4]])|
                      .[[5]]==min(.[[5]])|
                      .[[6]]==min(.[[6]])) %>%
      dplyr::distinct(.[[4]], .keep_all = T) %>%
      dplyr:: distinct(.[[5]], .keep_all = T) %>%
      dplyr:: distinct(.[[6]], .keep_all = T) %>%
      dplyr::select(1) %>%
      dplyr::arrange(.[[1]]) %>%
      dplyr::pull()


}))

    quantiles_cal <- do.call(rbind, purrr::map(1:length(yhat_new),
                                    ~qnorm(quantil_local[.,], yhat_new[.],
                                           sqrt(mse))))

    return(list(
      y_qtl_calibrated=quantiles_cal,
      other="Other outputs are being implemented."
    )

  )
    }

}

    if(type=="global"){

      df <- do.call(rbind, purrr::map(1:length(pit_values), ~{
        emp <- mean(ycal <= qnorm(p=pit_values[.] , mean=yhat_cal, sd=sqrt(mse)))
        estim <- pit_values[.]
        c(estim, emp)
      } ))

      # train isotonic regression

      ir <- stats::isoreg(df[,1], df[,2])


     # find correct probability

      y_quantis <- df %>%
        as.data.frame()%>%
        mutate(recal=as.stepfun(ir)(.[[1]])) %>%
        dplyr::mutate(dif1=abs(.[[3]]-cum_prob[1]),
                      dif2=abs(.[[3]]-cum_prob[2]),
                      dif3=abs(.[[3]]-cum_prob[3])) %>%
        dplyr::filter(.[[4]]==min(.[[4]])|
                        .[[5]]==min(.[[5]])|
                        .[[6]]==min(.[[6]])) %>%
        dplyr::distinct(.[[4]], .keep_all = T) %>%
        dplyr:: distinct(.[[5]], .keep_all = T) %>%
        dplyr:: distinct(.[[6]], .keep_all = T) %>%
        dplyr::select(1) %>%
        dplyr::arrange(.[[1]]) %>%
        dplyr::pull()


     quantiles <- do.call(rbind,
                          purrr::map(1:length(yhat_new),
                                     ~qnorm(y_quantis, yhat_new[.],
                                            sqrt(mse))))

      return(
        list(
          y_qtl_calibrated = quantiles,
         other="Other outputs are under construcution."
        )
      )

    }

  }


