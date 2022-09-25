calc_compindex <- function(x,avg_type = "simple",vif_threshold = 4.5)
{

  initial_si <- si_linear_exc_vif(x,avg_type = "simple",vif_threshold = 4.5)
  weight_opt <-ci_optimizer(x[,-c(initial_si$vif_index)])

  #TODO: Add Si calculation after optimization and also add while loop to run optimization
  #gives equal Sis


}
