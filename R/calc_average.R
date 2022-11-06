#' Calculate averages
#'
#' @name calc_average
#'
#' @description Calculate different types of averages
#'
#' @param x A Dataframe
#' @param avg_type Choosing average type. So far "simple", "geometric" and "harmonic" average are availableç
#'
#' @return A data frame
#'
#' @examples
#' calc_average(x,avg_type = "simple")


calc_average <- function(x,avg_type = "simple")
{
  # calculating composite index (ci)
  if(avg_type == "simple")
  {
    y <- as.data.frame(rowMeans(x))
  }
  else if(avg_type == "geometric")
  {
    y <-  apply(x, 1, function(x) (prod(x[x!=0]))^(1/sum(x!=0)))
  }
  else if(avg_type == "harmonic")
  {
    y <- apply(x, 1, function(x) (1/mean(1/x)))
  }

  # tidying up
  row.names(y) <- NULL
  colnames(y) <- c("ci")

  return(y)
}
