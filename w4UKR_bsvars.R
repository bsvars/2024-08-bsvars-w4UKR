
library(bsvars)

# upload data
data(us_fiscal_lsuw)    # upload dependent variables

set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar_t$new(p = 4) |>
  estimate(S = 1000) |> 
  estimate(S = 2000) -> post

post |> compute_impulse_responses(horizon = 8) |> plot(probability = 0.68)
post |> compute_variance_decompositions(horizon = 8) |> plot()
post |> compute_historical_decompositions() |> plot()
post |> compute_structural_shocks() |> plot()
post |> compute_conditional_sd() |> plot()
post |> compute_fitted_values() |> plot()

T       = nrow(us_fiscal_lsuw)
h       = 4
cf = cbind(rep(NA, h),tail(us_fiscal_lsuw[,2], 1) + mean(diff(us_fiscal_lsuw[,2])) * 1:h,rep(NA, h))
post |> 
  forecast(horizon = h) |> 
  plot(data_in_plot = 0.1)
post |> 
  forecast(horizon = h, conditional_forecast = cf) |> 
  plot(data_in_plot = 0.2)

post |> verify_identification() |> summary()

# check if 3 lags would be enough
H0          = matrix(NA, 3, 13)
H0[,10:12]  = 0       
post |> verify_autoregression(hypothesis = H0) |> summary()
