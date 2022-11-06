#' Calculate Si using linear method by excluding Xi using VIF
#'
#' @name si_linear_exc_vif
#'
#' @description Calculate Si using linear method by excluding Xi using VIF
#'
#' @param x A Dataframe
#' @param avg_type Choosing average type. So far "simple", "geometric" and "harmonic" average are availableç
#' @param avg_type Threshold for VIF. Based on this threshold variables from input data (x) are excluded for the calculations.
#'
#' @return A data frame
#'
#' @examples
#' si_linear_exc_vif(x,avg_type = "simple", vif_threshold = 4.5)


si_linear_exc_vif <- function(x,avg_type = "simple", vif_threshold = 4.5)
{

  # calculating composite index (ci)
  y<- calc_average(x,avg_type)

  # creating regression model using all the inputs and calculating VIF
  m <- lm(y$ci~.,data=x)
  suppressWarnings({ vif_calc <- vif(m) })

  #TODO: Add range for VIF

  vif_index <- which(as.matrix(vif_calc)>vif_threshold)

  if(length(vif_index)!=0)
  {
    x_new <- x[-vif_index]
  }else
    {
      x_new <- x
    }
  y_new<- calc_average(x_new,avg_type)
  d <- dim(x_new)[2]
  si_vif <- NULL
  we <- NULL

  for (i in 1:d)
  {
    xx <- x_new[,-i]
    #y<- calc_average(xx,avg_type)
    m_i <- lm(y_new$ci~as.matrix(xx))
    m_s <- summary(m_i)
    r_2 <- m_s$r.squared
    si_vif <- rbind(si_vif,r_2)
    w <- 1 - r_2
    we <- rbind(we,w)
  }
  si_normalized <- si_vif/(sum(si_vif))
  si_adj <- we/sum(we)
  row.names(si_vif) <- NULL
  row.names(si_adj) <- NULL
  row.names(si_standardized) <- NULL
  final_lst <- list(vif_calc,vif_index,si_vif, si_normalized,si_adj)
  names(final_lst) <- c("vif_calc", "vif_index","si", "si_normalized","si_adj")
  return(final_lst)
}
