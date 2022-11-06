#' Calculate Si using linear method by excluding Xi
#'
#' @name si_linear_exc
#'
#' @description Calculate Si using linear method by excluding Xi at each iteration while calculating Si
#'
#' @param x A Dataframe
#' @param avg_type Choosing average type. So far "simple", "geometric" and "harmonic" average are availableç
#'
#' @return A data frame
#'
#' @examples
#' si_linear_exc(x,avg_type = "simple")

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

  colnames(s_i_exc) <- NULL
  row.names(s_i_exc) <- NULL
  si_normalized <-s_i_exc/sum(s_i_exc)
  final_lst <- list(s_i_exc,si_normalized)
  names(final_lst) <- c("si","si_normalized")
  return(final_lst)
}
