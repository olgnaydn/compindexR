ci_optimizer <- function(x)
{

  objective_function <- function(w_w)
  {
       we <- as.matrix(w_w)
       x_w <- as.matrix(x)%*%we

       #Si calculation - linear
       d <- dim(x)[2]
       s_i <- NULL

       for (i in 1:d)
       {
         m <- lm(x_w~as.matrix(x[,i]))
         m_s <- summary(m)
         r_2 <- m_s$r.squared
         s_i <- rbind(s_i,r_2)
       }

       w_d <- 1/d
       s_d <- as.data.frame(rep(w_d,d))
       s_i_norm <- s_i/sum(s_i)

       s_d_mse <- sum((s_i - s_d)^2)

    return(s_d_mse)
  }

  heq1 <- function(w_w) sum(w_w) - 1
  x0 <- rep(1/d, d)
  x0 <- c(0.1666667, 0.1666667, 0.1666667,0.1666667,0.1666667,0.1666667)
  lb <- c(0.001,0.001,0.001,0.001,0.001,0.001)
  ub <- c(0.999,0.999,0.999,0.999,0.999,0.999)
  res <- fmincon(x0, objective_function, lb = lb, ub = ub, heq = Aeq1)
  return(res)
}
