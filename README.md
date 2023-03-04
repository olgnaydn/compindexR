# compindexR: An R package for calculating composite indicators

The package uses the first-order sensitivity index to measure whether the weights assigned by the creator of the composite indicator match the actual importance of the variables. Moreover, the variance inflation factor (VIF) is used to reduce the set of correlated variables. 

In the case of a discrepancy between the importance and the assigned weight, the script determines weights that allow adjustment of the weights to the variables’ intended impact. If the optimised weights are unable to reflect the desired importance, the highly correlated variables are reduced, taking into account VIF. The final outcome of the script is the calculated value of the composite indicator based on optimal weights and a reduced set of variables, and the linear ordering of the analysed objects.

## Installing compindexR


```
library(devtools)
install_github("olgnaydn/compindexR")

```

### After completing steps above

- Every time on Github Desktop, please click fetch and check whether there are new commits
- If there are new commits please click PULL to get the latest updates about the package
- Afterwards, please open the project on RStudio and run the following command to compile the package on your local.


## Example usage of the package.

- Please make sure that you have `readxl` package installed. If its not installed, please install it using following commands

```
install.packages(readxl)
library(readxl)
```
- From data folder, import the excel file which is called `cel4_adjusted.xlsx`.
```
x <- read_xlsx("data/cel 10.xlsx")
```
- Run the following command to calculate composite indicator. Following command will allow you to choose average type as a arithmetic average, tolerance for Si calculation as 0.05, number of iterations to get Si as close to each other as possible, set VIF threshold. 

```
calc_compindex(x, 
              avg_type = "simple", 
              scaling_method = "min-max", 
              vif_threshold = NULL, 
              si_diff = 0.005
              )
```

Command above will return a result as shown below

```
$no_of_iteration
[1] 8

$x_excluded
     [,1]                                                                                                                    
[1,] "Income share of the bottom 40 % of the population [SDG_10_50]"                                                         
[2,] "Income distribution [SDG_10_41] (Ratio)"                                                                               
[3,] "Young people neither in employment nor in education and training (NEET) [SDG_08_20A] (From 15 to 29 years, Percentage)"
[4,] "Purchasing power adjusted GDP per capita [SDG_10_10]"                                                                  
[5,] "Relative median at-risk-of-poverty gap [SDG_10_30]"                                                                    
[6,] "People at risk of income poverty after social transfers [SDG_01_20A] (Percentage, 18 years or over)"                   
[7,] "Early leavers from education and training [SDG_04_10A] (Percentage, From 18 to 24 years)"                              
[8,] "People at risk of poverty or social exclusion  [SDG_01_10A] (percentage)"                                              

$final_weights
          [,1]
[1,] 0.5251892
[2,] 0.4748108

$final_si
          [,1]
[1,] 0.4437858
[2,] 0.4437853

$final_x
[1] "Asylum applications [SDG_10_60] (per million population)"      
[2] "Employment rate [SDG_08_30A] (Percentage, From 20 to 64 years)"

$ci
 [1] 0.8257575 0.6257100 0.5613797 0.5168642 0.4739217 0.4433114 0.4137330 0.3963608
 [9] 0.3902223 0.3828151 0.3616450 0.3543244 0.3322812 0.3252506 0.3193145 0.2940342
[17] 0.2926783 0.2895374 0.2892485 0.2790355 0.2612437 0.2400609 0.2359284 0.2343469
[25] 0.2205606 0.1401317 0.0493719
```

First published in uRos 2022. If you use compindexR, please cite it.

Pietrzak, M. B., Kuc-Czarnecka, M., & Aydin, O. (2022). compindexR: An R package for calculating composite indicators. uRos Conference 2022. https://r-project.ro/conference2022/book-of-abstracts-uros2022.pdf


