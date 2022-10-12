calc_compindex <- function(x,avg_type = "simple",vif_threshold = NULL, si_diff = 0.1)
{

  if(is.null(vif_threshold))
  {
    initial_si <- si_linear_exc(x,avg_type = avg_type)
    we_opt_first_it <-ci_optimizer(x)
  }else
    {
    initial_si <- si_linear_exc_vif(cel4_adjusted,avg_type = avg_type,vif_threshold = vif_threshold)
    we_opt_first_it <-ci_optimizer(x[,-c(initial_si$vif_index)])
  }

  # Controlling Si values for the optimization loop
  upper_threshold <- 1 + si_diff
  lower_threshold <- 1 - si_diff
  any(si >= (mean(si))*lower_threshold || si <= (mean(si))*upper_threshold) == TRUE


}
