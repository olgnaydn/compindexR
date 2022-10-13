# compindexR: An R package for calculating composite indicators


- Please make sure that devtools package is installed.
- To install the package directly from Github, use following command. 

```
devtools::install_github("olgnaydn/ciaor",
                          ref="main",
                          auth_token = "your_access_token"
                          )
```

To get personal access token, please follow the setups shown here. visit this url: https://github.com/settings/tokens

## Having the new version of the package using Github Desktop

- Please install Github Desktop
- Clone the repository
- Open the project
- Run the following command

```
devtools::load_all()
```
- Now, its ready to use.

## Example usage of the package.

- Please make sure that you have `readxl` package installed. If its not installed, please install it using following command

```
install.packages(readxl)
library(readxl)
```
- From data folder import the excel file which is called `cel4_adjusted.xlsx`.
```
x <- read_xlsx("data/cel4_adjusted.xlsx")
```
- Run the following command to calculate composite indicator. Following command will allow you to choose average type as a arithmetic average, tolerance for Si calculation as 0.05, number of iterations to get Si as close to each other as possible, set VIF threshold. 

```
calc_compindex(x,avg_type = "simple",vif_threshold = NULL, si_diff = 0.05, iteration = 10)
```




