#' Calculate Si using linear method by excluding Xi using VIF
#'
#' @name si_linear_exc_vif
#'
#' @description Calculate Si using linear method by excluding Xi using VIF
#'
#' @param x A Dataframe
#' @param avg_type Choosing average type. So far "simple", "geometric" and "harmonic" average are availableç
#' @param vif_threshold Threshold for VIF. Based on this threshold variables from input data (x) are excluded for the calculations.
#'
#' @return A data frame
#'
#' @examples
#' x <- data.frame(rnorm(20),rnorm(20),rnorm(20),rnorm(20))
#' si_linear_exc_vif(x,avg_type = "simple", vif_threshold = 4.5)


si_linear_exc_vif <- function(x,avg_type = "simple", vif_threshold = 4.5)
{

  # calculating composite index (ci)
  y<- calc_average(x,avg_type)


  # creating regression model using all the inputs and calculating VIF
  m <- lm(y$ci~.,data=x)
  suppressWarnings({ vif_calc <- vif(m) })

  #TODO: Add range for VIF

  vif_index <- which(as.matrix(vif_calc) > vif_threshold)
  vif_above_threshold <- vif_calc[vif_index]

  if(length(vif_index) != 0)
  {
    x_n <- x[-vif_index]
  }else
    {
      x_n <- x
    }
  #y_n<- calc_average(x_n,avg_type)
  d <- dim(x_n)[2]
  si <- NULL

    for (i in 1:d)
    {
      m <- lm(y$ci~as.matrix(x[,i]))
      m_s <- summary(m)
      r_2 <- m_s$r.squared
      si <- rbind(si,r_2)
    }
  si_normalized <- si/(sum(si))
  row.names(si) <- NULL
  row.names(si_normalized) <- NULL

  final_lst <- list(vif_calc,vif_index, si, si_normalized)
  names(final_lst) <- c("vif_calc", "vif_index","si", "si_normalized")
  return(final_lst)
}
