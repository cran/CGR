#' @title CGR
#' @description Compound Growth Rate for Capturing the Growth Rate Over the Period
#'
#' @param variable Time series data taken for the study
#' @param data Name of the data taken for the study
#' @param verbose Logical. If TRUE, the function prints detailed information about its progress. Default is FALSE.
#'
#' @return Returns a list containing the Compound growth rate for capturing the growth rated over the period  and other model parameters.
#' The list includes:
#' \itemize{
#'   \item \code{CGR}: Growth rate calculated for the data.
#'   \item \code{AoS}: The value derived by taking anti log of the slope of exponential model.
#' }
#' @examples{
#' library(CGR)
#' years <- 1:50
#' value<-rnorm(length(years),100, 50)
#' data <- data.frame(Year = years, Sales = round(value))
#' CGR_results <- CGR(variable = data$Sales, data = data)
#' print(CGR_results)
#' }
#' @export
#'
#' @references
#' #' \itemize{
#'\item •	Shankar, S. V., Chandel, A., Gupta, R. K., Sharma, S., Chand, H., Kumar, R., ... & Gowsar, S. N. (2023). Corrigendum: Exploring the dynamics of arrivals and prices volatility in onion (Allium cepa) using advanced time series techniques. Frontiers in Sustainable Food Systems, 7, 1290515. DOI: 10.3389/fsufs.2023.1208898
#' }

CGR <- function(variable, data, verbose = FALSE) {
  y <- variable
  x <- seq_along(y)
  Model <- stats::lm(log(y) ~ x)
  Coefficient <- stats::coef(Model)
  Slope <- Coefficient[2]
  AoS <- base::exp(Slope)
  Growthrate <- (AoS - 1) * 100
  if(verbose) {
    s <- summary(Model)
    message("Model Summary:\n")
    print(s)
  }
  results <- data.frame("CGR in percentage" = Growthrate, "Antilog of Slope" = AoS)
  return(results)
}


