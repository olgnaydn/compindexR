ci_optimizer <- function(x)
{

  d <- dim(x)
  c_count <- d[2]
  w <- matrix(NA, nrow = c_count,1)

  objective_function <- function(ww)
  {
       wx <- c(ww[1],ww[2],ww[3],ww[4],ww[5],ww[6])
       x_w <- x%*%wx
    return(x_w)
  }


  x0 <- c(0.1666667, 0.1666667, 0.1666667,0.1666667,0.1666667,0.1666667)
  #lb <- c(0,0,0,0,0)
  #ub <- c(1,1,1,1,1)
  #opt_w<-
  #nelder_mead(objective_function, x0, rep(-0.5, 3), rep(0.5, 3))
  neldermead(x0, objective_function)
  #optim(x0, objective_function, method = "Nelder-Mead")

  #TODO: Test the code!

  # weights sum up to 1, positive.
    # lb = 0.001
    # ub = 1
  # no threshold will be added for Si
  # run optimization till normalized Si will be equal (or defined by the user - future functionality)
    # Remove variables with highest Si till the moment that Sis are equal.

}
