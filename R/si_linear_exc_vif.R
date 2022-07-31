si_linear_exc_vif <- function(x,avg_type = "simple",vif_threshold = 4.5)
{
  
  ### Requirement is MASS package ###
  
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
  
  # creating regression model using all the inputs and calculating VIF
  m <- lm(y$ci~.,data=x)
  vif_calc <- vif(m)
  
  vif_index <- which(as.matrix(vif_calc)>vif_threshold)
  
  x_new <- x[-vif_index]
  
  d <- dim(x_new)[2]
  s_i_exc_vif <- NULL
  
  for (i in 1:d)
  {
    xx <- x_new[,-i]
    m_i <- lm(y$ci~as.matrix(xx))
    m_s <- summary(m_i)
    r_2 <- m_s$r.squared
    s_i_exc_vif <- rbind(s_i_exc_vif,r_2)
  }
  
  colnames(s_i_exc_vif) <- "s_i_exc_vif"
  row.names(s_i_exc_vif) <- NULL
  return(s_i_exc)
}