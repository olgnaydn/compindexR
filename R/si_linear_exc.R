si_linear_exc <- function(x,avg_type = "simple")
{
  # calculating composite index (ci)
  y<- calc_average(x,avg_type)

  # tidying up
  row.names(y) <- NULL
  colnames(y) <- c("ci")

  d <- dim(x)[2]
  s_i_exc <- NULL

  for (i in 1:d)
  {
    xx <- x[,-i]
    #y<- calc_average(xx,avg_type)
    m <- lm(y$ci~as.matrix(xx))
    m_s <- summary(m)
    r_2 <- m_s$r.squared
    s_i_exc <- rbind(s_i_exc,r_2)
  }

  colnames(s_i_exc) <- "s_i_exc"
  row.names(s_i_exc) <- NULL
  return(s_i_exc)
}
