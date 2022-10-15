calc_compindex <- function(x, avg_type = "simple", scaling_method = "min-max", vif_threshold = NULL, si_diff = 0.1, iteration =10)
{
  # Controlling Si values for the optimization loop
  upper_threshold <- 1 + si_diff
  lower_threshold <- 1 - si_diff

  x_scaled <- scaling(x, method = scaling_method)

  if(is.null(vif_threshold))
  {
    x_new_ini <- x_scaled
    we_opt_ini <-ci_optimizer(x_new_ini)

  }else
    {
    si_ini <- si_linear_exc_vif(x_scaled,avg_type = avg_type,vif_threshold = vif_threshold)
    x_new_ini <- x_scaled[,-c(si_ini$vif_index)]
    we_opt_ini <-ci_optimizer(x_new_ini)
    #TODO: Add x_excluded here to make sure that variables excluded
    # because of VIF are visible in the output
    }

  x_new_mat_ini <- as.matrix(x_new_ini)
  weight_mat_ini <- as.matrix(we_opt_ini$par )
  y_new_ini <- x_new_mat_ini  %*% weight_mat_ini

  d <- dim(x_new_ini)[2]
  si <- NULL

  for (i in 1:d)
  {
    xx <- x_new_ini[,-i]
    #y<- calc_average(xx,avg_type)
    m <- lm(y_new_ini[,1]~as.matrix(xx))
    m_s <- summary(m)
    r_2 <- m_s$r.squared
    si <- rbind(si,r_2)
  }

  row.names(si) <- NULL
  x_new <- x_new_ini
  x_excluded <- NULL

    for(i in 1: iteration)
      {

      ind_exclude <- which(si==max(si))
      col_excluded <- colnames(x_new[ind_exclude])
      x_new <- x_new[-c(ind_exclude)]

      x_new_mat <- as.matrix(x_new)
      we_opt_new <-ci_optimizer(x_new)
      weight_mat <- as.matrix(we_opt_new$par)
      y_new <- x_new_mat  %*% weight_mat

      d <- dim(x_new)[2]
      si_calc <- NULL

      for (j in 1:d)
      {
        xx <- x_new[,-j]
        #y<- calc_average(xx,avg_type)
        m <- lm(y_new~as.matrix(xx))
        m_s <- summary(m)
        r_2 <- m_s$r.squared
        si_calc <- rbind(si_calc,r_2)
      }
      row.names(si_calc) <- NULL
      si<- si_calc
      x_excluded <- rbind(x_excluded,col_excluded)
      #print(paste("Calculating weights, iteartion:",i,sep=""))
      if(all(si >= mean(si)*lower_threshold) == TRUE) break
    }

  row.names(x_excluded) <- NULL
  iteration <- i
  #weight_mat <- as.matrix(we_opt_new$par)

  #calculating final ci
  ci <- as.matrix(x[,colnames(x_new)]) %*% weight_mat
  ci_sorted <- sort(ci,decreasing = TRUE)

  final_lst <- list(iteration, x_excluded, weight_mat,si,colnames(x_new),ci_sorted)
  names(final_lst) <- c("no_of_iteration","x_excluded","final_weights","final_si","final_x","ci")

  return(final_lst)
  }


