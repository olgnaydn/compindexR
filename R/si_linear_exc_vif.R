si_linear_exc_vif <- function(x,avg_type = "simple",vif_threshold = 4.5)
{

  ## Dependencies

  # calculating composite index (ci)

  y<- calc_average(x,avg_type)

  # creating regression model using all the inputs and calculating VIF
  m <- lm(y$ci~.,data=x)
  suppressWarnings({ vif_calc <- vif(m) })

  #TODO: Add range for VIF

  vif_index <- which(as.matrix(vif_calc)>vif_threshold)

  x_new <- x[-vif_index]

  d <- dim(x_new)[2]
  s_i_exc_vif <- NULL
  we <- NULL

  for (i in 1:d)
  {
    xx <- x_new[,-i]
    y<- calc_average(x,avg_type)
    m_i <- lm(y$ci~as.matrix(xx))
    m_s <- summary(m_i)
    r_2 <- m_s$r.squared
    s_i_exc_vif <- rbind(s_i_exc_vif,r_2)
    w <- 1 - r_2
    we <- rbind(we,w)
  }

  initial_weights <- we/sum(we)
  row.names(initial_weights) <- NULL
  colnames(s_i_exc_vif) <- "s_i_exc_vif"
  row.names(s_i_exc_vif) <- NULL
  return(list(vif_calc,vif_index, s_i_exc_vif,initial_weights))
}
