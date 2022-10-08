calc_compindex <- function(x,avg_type = "simple",vif_threshold = 4.5)
{

  initial_si <- si_linear_exc_vif(cel4_adjusted,avg_type = "simple",vif_threshold = 4.5)
  weight_opt <-ci_optimizer(x[,-c(initial_si$vif_index)])

  #TODO: Add Si calculation after optimization and also add while loop to run optimization
  #gives equal Sis
  #TODO: Round Si, they dont have sharp equal. Even still rounding will not work add some thresholding
  # e.g. 2% different than avg Si
  # Add possibility to choose standardization function for x.
  # Calculat y after obtaining optimum weights then order(desc)
  # Run optimization without taking VIF into account, user should be able to decide on this.


}
