#' Calculate Si using linear method
#'
#' @name si_linear
#'
#' @description Calculate Si using linear method
#'
#' @param x A Dataframe
#' @param avg_type Choosing average type. So far "simple", "geometric" and "harmonic" average are availableç
#'
#' @return A data frame
#'
#' @examples
#' x <- data.frame(rnorm(20),rnorm(20),rnorm(20),rnorm(20))
#' si_linear(x,avg_type = "simple")


si_linear <- function(x,avg_type = "simple")
{

# calculating composite index (ci)
y<- calc_average(x,avg_type)

# tidying up
row.names(y) <- NULL
colnames(y) <- c("ci")

d <- dim(x)[2]
s_i <- NULL

for (i in 1:d)
{
  m <- lm(y$ci~as.matrix(x[,i]))
  m_s <- summary(m)
  r_2 <- m_s$r.squared
  s_i <- rbind(s_i,r_2)
}

row.names(s_i) <- NULL
si_normalized <-s_i/sum(s_i)
colnames(s_i) <- "si"

return(list(s_i,si_normalized))
}


