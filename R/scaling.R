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
    standardization <- function(v)
    {
      s_dev <- sd(v)
      avg <- mean(v)
      result <- (x - avg) / s_dev
      return(result)
    }
    x_new <- apply(x, 2, standardization)
  }
  #TODO: Add more normalization methods

return(as.data.frame(x_new))
}
