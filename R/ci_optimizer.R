ci_optimizer <- function(x)
{
  # WORK IN PROGRESS !!! ###


  d <- dim(x)
  c_count <- d[2]
  w <- matrix(NA, nrow = c_count,1)

  objective_function <- function(ww)
  {
       we <- as.matrix(ww)
       x_w <- as.matrix(x)%*%we

       #Si calculation - linear
       d <- dim(x)[2]
       s_i <- NULL

       for (i in 1:d)
       {
         m <- lm(y$ci~as.matrix(x[,i]))
         m_s <- summary(m)
         r_2 <- m_s$r.squared
         s_i <- rbind(s_i,r_2)
       }

    return(x_w)
  }

  #heq1 <- function(ww) sum(ww) - 1

  x0 <- c(0.1666667, 0.1666667, 0.1666667,0.1666667,0.1666667,0.1666667)
  #x0 <- c(0, 0, 0, 0, 0)
  lb <- c(0.01,0.01,0.01,0.01,0.01,0.01)
  ub <- c(1,1,1,1,1,1)

  neldermead(x0, objective_function,lower=lb,upper=ub)
  res <- fmincon(x0, objective_function, lb = lb, ub = ub,heq = heq1)

  fminsearch(objective_function, x0, lower = lb, upper = ub,method = "Hooke-Jeeves")
  optim(x0,objective_function,method = "Nelder-Mead")

  #TODO: Test the code!

  # weights sum up to 1, positive.
    # lb = 0.001
    # ub = 1
  # no threshold will be added for Si
  # run optimization till normalized Si will be equal (or defined by the user - future functionality)
    # Remove variables with highest Si till the moment that Sis are equal.

}
