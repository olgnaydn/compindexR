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
  lb <- rep(0.0001,d)
  ub <- rep(0.9999,d)
  res <- fmincon(x0, objective_function, lb = lb, ub = ub, heq = Aeq1,tol = 1e-04)
  return(res)
}
