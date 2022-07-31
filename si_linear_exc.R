si_linear_exc <- function(x,avg_type = "simple")
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
  
  d <- dim(x)[2]
  s_i_exc <- NULL
  
  for (i in 1:d)
  {
    xx <- x[,-i]
    m <- lm(y$ci~as.matrix(xx))
    m_s <- summary(m)
    r_2 <- m_s$r.squared
    s_i_exc <- rbind(s_i_exc,r_2)
  }
  
  colnames(s_i_exc) <- "s_i_exc"
  row.names(s_i_exc) <- NULL
  return(s_i_exc)
}


