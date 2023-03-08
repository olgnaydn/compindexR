#' Calculating composite indicator automatically step by step
#'
#' @name calc_compindex
#'
#' @description Calculates composite indicator by excluding the least significant variable at each step.
#'
#' @param x A Dataframe
#' @param avg_type Choosing average type. So far "simple", "geometric" and "harmonic" average are available
#' @param scaling_method Standardization or normalization technique. So far "min-max" and "standardization" are available
#' @param vif_threshold Threshold for VIF. Based on this threshold variables from input data (x) are excluded for the calculations.
#' @param si_diff Tolerance for normalized Si calculation. Can be between 0 and 1
#'
#' @return A list of lists
#'
#' @examples
#' x <- data.frame(rnorm(20),rnorm(20),rnorm(20),rnorm(20))
#' calc_compindex(x, avg_type = "simple",
#' scaling_method = "min-max",
#'  vif_threshold = NULL,
#'   si_diff = 0.1)

calc_compindex <- function(x, avg_type = "simple", scaling_method = "min-max", vif_threshold = NULL, si_diff = 0.1)
{

  iteration <- dim(x)[2]-2

  x_scaled <- scaling(x, method = scaling_method)

  if(is.null(vif_threshold))
  {
    x_new_ini <- x_scaled
    si_ini <- si_linear(x_new_ini,avg_type = "simple")
    we_opt_ini <-ci_optimizer(x_new_ini)

  }else
  {
    si_ini <- si_linear_exc_vif(x_scaled,avg_type = avg_type,vif_threshold = vif_threshold)
    x_new_ini <- x_scaled[,-c(si_ini$vif_index)]
    we_opt_ini <-ci_optimizer(x_new_ini)
    #TODO: Add x_excluded here to make sure that variables excluded
    # because of VIF are visible in the output
  }

  weight_all <- NULL
  x_all <- NULL

  x_new_mat_ini <- as.matrix(x_new_ini)
  weight_mat_ini <- as.matrix(we_opt_ini$par)

  x_all <- append(x_all,list("x0" = data.frame(x_new_ini)))
  weight_all <- append(weight_all,list("w0" = data.frame(we_opt_ini$par)))

  y_new_ini <- x_new_mat_ini  %*% weight_mat_ini

  d <- dim(x_new_ini)[2]
  si <- NULL
  si_all <- NULL

  for (t in 1:d)
  {
    xx <- x_new_ini
    m <- lm(y_new_ini~as.matrix(xx[,t]))
    m_s <- summary(m)
    r_2 <- round(m_s$r.squared,3)
    si <- rbind(si,r_2)
  }

  row.names(si) <- NULL
  si_normalized <- round(si/sum(si),3)

  si_all <- append(si_all,list("si_initial" = data.frame(si_ini)))
  si_all <- append(si_all,list("si0" = data.frame(si,si_normalized)))

  upper_threshold <- (1/dim(x_new_ini)[2])+si_diff
  lower_threshold <- (1/dim(x_new_ini)[2])-si_diff

  if(all(between(si_normalized,lower_threshold,upper_threshold))==TRUE)
  {
    we_opt_new <-  we_opt_ini
    iter <- 1
  }

  x_new <- x_new_ini
  x_excluded <- NULL

  for(i in 1: iteration)
  {
    upper_threshold <- (1/dim(x_new)[2])+si_diff
    lower_threshold <- (1/dim(x_new)[2])-si_diff

    if(all(between(si_normalized,lower_threshold,upper_threshold))==TRUE) break

    ind_exclude <- which(si_normalized==min(si_normalized))[1]
    col_excluded <- colnames(x_new[ind_exclude])
    x_new <- x_new[-c(ind_exclude)]

    # appending all x which are not thrown
    x_all <- append(x_all,list(data.frame(x_new)))
    name_of_xi_list <- paste("x",i,sep="")
    names(x_all)[i+1] <- name_of_xi_list

    x_new_mat <- as.matrix(x_new)
    we_opt_new <- ci_optimizer(x_new_mat)
    weight_mat <- as.matrix(we_opt_new$par)

    # appending all weights
    weight_all <- append(weight_all,list(data.frame(we_opt_new$par)))
    name_of_wi_list <- paste("w",i,sep="")
    names(weight_all)[i+1] <- name_of_wi_list

    y_new <- x_new_mat  %*% weight_mat

    d <- dim(x_new)[2]
    si_calc <- NULL

    for (j in 1:d)
    {
      xx <- x_new
      m <- lm(y_new~as.matrix(xx[,j]))
      m_s <- summary(m)
      r_2 <- round(m_s$r.squared,3)
      si_calc <- rbind(si_calc,r_2)
    }
    row.names(si_calc) <- NULL
    si<- si_calc
    si_normalized <- round(si/sum(si),3)
    x_excluded <- rbind(x_excluded,col_excluded)
    iter <- i
    name_of_si_list <- paste("si",i,sep="")

    # creating lists for si
    si_all <- append(si_all,list(data.frame(si,si_normalized)))
    names(si_all)[i+2] <- name_of_si_list
  }
  row.names(x_excluded) <- NULL
  weight_mat <- as.matrix(we_opt_new$par)

  #calculating final ci
  ci <- as.matrix(x[,colnames(x_new)]) %*% weight_mat
  ci_sorted <- sort(ci,decreasing = TRUE)

  final_lst <- list(iter,si_all, x_excluded, weight_all,x_all,ci_sorted)
  names(final_lst) <- c("no_of_iteration","si_all","x_excluded_all","weights_all","x_all","ci")

  return(final_lst)
}
