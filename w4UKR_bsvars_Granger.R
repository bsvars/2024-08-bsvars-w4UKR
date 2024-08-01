

library(bsvars)

# upload data
data(us_fiscal_lsuw)    # upload dependent variables

set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar$new() |>
  estimate(S = 1000) |> 
  estimate(S = 2000) -> post

post$last_draw$starting_values$A # study the structure of the A matrix

# verify no Granger causality from ttr to gdp
H0      = matrix(NA, 3, 4)
H0[,]    = 0 # modify this line to set appropriate restriction
post |> verify_autoregression(hypothesis = H0) |> summary()
