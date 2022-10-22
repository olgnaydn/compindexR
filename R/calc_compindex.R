calc_compindex <- function(x, avg_type = "simple", scaling_method = "min-max", vif_threshold = NULL, si_diff = 0.1)
{

  upper_threshold <- (1/dim(x)[2])+si_diff
  lower_threshold <- (1/dim(x)[2])-si_diff
  print(paste("lower_threshold: ",lower_threshold, sep=""))
  print(paste("upper_threshold: ",upper_threshold, sep=""))

  iteration <- dim(x)[2]-2

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
  print("weight_start")
  print(weight_mat_ini)
  y_new_ini <- x_new_mat_ini  %*% weight_mat_ini

  d <- dim(x_new_ini)[2]
  si <- NULL

  for (t in 1:d)
  {
    #xx <- x_new[,-j]
    xx <- x_new_ini
    #y<- calc_average(xx,avg_type)
    #m <- lm(y_new_ini[,1]~as.matrix(xx))
    m <- lm(y_new_ini~as.matrix(xx[,t]))
    m_s <- summary(m)
    r_2 <- round(m_s$r.squared,3)
    si <- rbind(si,r_2)
  }

  row.names(si) <- NULL
  si_normalized <- round(si/sum(si),3)
  print("si_start")
  print(si)
  print("si_normalized_start")
  print(si_normalized)


  if(all(between(si_normalized,lower_threshold,upper_threshold))==TRUE)
  {
    we_opt_new <-  we_opt_ini
    print("we_opt_new = we_opt_ini: ")
    print(we_opt_new$par)
  }

  x_new <- x_new_ini
  x_excluded <- NULL

  for(i in 1: iteration)
  #while(all(between(si,lower_threshold,upper_threshold))==FALSE)
      {
      upper_threshold <- (1/dim(x_new)[2])+si_diff
      lower_threshold <- (1/dim(x_new)[2])-si_diff
      #if(all(si_normalized >= lower_threshold) && all(si_normalized <= upper_threshold)== TRUE) break

      if(all(between(si_normalized,lower_threshold,upper_threshold))==TRUE) break

      ind_exclude <- which(si_normalized==min(si_normalized))[1]
      col_excluded <- colnames(x_new[ind_exclude])
      print(paste("Beginning of iteration: ",i,sep=""))
      print(paste("Variable excluded: ",col_excluded,sep=""))
      x_new <- x_new[-c(ind_exclude)]

      x_new_mat <- as.matrix(x_new)
      we_opt_new <- ci_optimizer(x_new_mat)
      weight_mat <- as.matrix(we_opt_new$par)
      print("weight_opt")
      print(weight_mat)
      y_new <- x_new_mat  %*% weight_mat

      d <- dim(x_new)[2]
      si_calc <- NULL

      for (j in 1:d)
      {
        #xx <- x_new[,-j]
        xx <- x_new
        #y<- calc_average(xx,avg_type)
        #m <- lm(y_new~as.matrix(xx))
        m <- lm(y_new~as.matrix(xx[,j]))
        m_s <- summary(m)
        r_2 <- round(m_s$r.squared,3)
        si_calc <- rbind(si_calc,r_2)
      }
      row.names(si_calc) <- NULL
      si<- si_calc
      print("si_opt")
      print(si)
      si_normalized <- round(si/sum(si),3)
      print("si_opt_normalized")
      print(si_normalized)
      x_excluded <- rbind(x_excluded,col_excluded)
      print(paste("Calculated weights, end of iteration: ",i,sep=""))
      }

  row.names(x_excluded) <- NULL
  iteration <- i
  weight_mat <- as.matrix(we_opt_new$par)

  #calculating final ci
  ci <- as.matrix(x[,colnames(x_new)]) %*% weight_mat
  ci_sorted <- sort(ci,decreasing = TRUE)

  final_lst <- list(iteration, x_excluded, weight_mat,si,si_normalized,colnames(x_new),ci_sorted)
  names(final_lst) <- c("no_of_iteration","x_excluded","final_weights","final_si","final_si_normalized","final_x","ci")

  return(final_lst)
  }


