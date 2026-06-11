#' Normalization and standardization techniques
#'
#' @name scaling
#'
#' @description Normalization and standardization techniques
#'
#' @param x A Dataframe
#' @param method Standardization or normalization technique. So far "min-max" and "standardization" are available
#'
#' @return A data frame
#'
#' @examples
#' x <- data.frame(rnorm(20),rnorm(20),rnorm(20),rnorm(20))
#' scaling(x,method = "min-max")


scaling <- function(x, method = "min-max")

{
  d <- dim(x)[2]

  if(method == "min-max")
  {
    min_max <- function(v)
      {
        L <- min(v)
        H <- max(v)
        result <- (v - L) / (H - L)
        return(result)
      }
    x_new <- apply(x, 2, min_max)
  }
  else if(method=="standardization")
  {
    x_new <- as.data.frame(lapply(x, function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)))
  }
  #TODO: Add more normalization methods

return(as.data.frame(x_new))
}
