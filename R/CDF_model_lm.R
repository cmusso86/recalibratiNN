#' Title
#'
#' @param x_cal vettor
#' @param model modelo
#'
#' @return
#' @export
#'
#' @examples
#'
#'
CDF_model_lm <- function(x_cal, model) {
  df <- data.frame(x_cal)
  names(df) <- names(model$model)[-1]
  pred <- stats::predict(model, newdata = df)
  Fx <- function(y_cal) stats::pnorm(y_cal, pred, summary(model)$sigma)
  return(Fx)
}
