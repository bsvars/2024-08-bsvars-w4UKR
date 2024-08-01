############################################################
# Workshops for Ukraine by Daria Mykailyshyna
# Structural and Predictive Macro Analyses Using the R Package bsvars
# by Tomasz Woźniak 
# 1 August 2024
############################################################

# download and transform the data
############################################################
create_interpolated_series <- function(data, start_date, end_date) {
  # function to interpolate quarterly to monthly data
  # start_date and end_date need to be given as a character with format "YYYY-MM-DD"
  
  # data object to merge by data all time series
  df <- data.frame(date = seq(as.Date(start_date), as.Date(end_date), by = "months"))
  df <- dplyr::left_join(df, data, by = "date")  
  ## interpolate GDP series
  # first shift GDP series such that Q1 is assigned to March etc
  df$X   = c(rep(NA,2), df$value[1:(length(df$value) - 2)])
  # now interpolate, taking into account leading and trailing NA values
  df <- dplyr::mutate(df, value_out = c(rep(NA, min(which(!is.na(df$X))) - 1),
                                        zoo::na.approx(df$X),
                                        rep(NA, length(df$date) - max(which(!is.na(df$X))))
  )
  )
  df <- dplyr::select(df, c(date, value_out))
  df
}

start_date = "1950-01-01"
end_date   = "2024-07-27"

# US data
############################################################
# QUARTERLY
# real gdp
gdp       = fredr::fredr("GDPC1")
df_gdp    = create_interpolated_series(gdp, start_date, end_date)
gdp       = xts::xts(df_gdp$value_out, df_gdp$date, tclass = 'yearmon')
rgdp       = xts::to.monthly(gdp, OHLC = FALSE, drop.time = TRUE)
rm(df_gdp)

# Consumer Price Index: All Items for the United States
cpi       = fredr::fredr("USACPIALLMINMEI")
cpi       = xts::xts(cpi$value, cpi$date, tclass = 'yearmon')
cpi       = 100 * log(cpi)

# Federal Funds Effective Rate
FFR       = fredr::fredr("FEDFUNDS")
FFR       = xts::xts(FFR$value, FFR$date, tclass = 'yearmon')

# AUS data
############################################################

# QUARTERLY
# Real Gross Domestic Product for Australia (Domestic Currency, Seasonally Adjusted)
aud_gdp   = fredr::fredr("NGDPRSAXDCAUQ")
df_au_gdp = create_interpolated_series(aud_gdp, start_date, end_date)
aud_gdp   = na.omit(xts::xts(df_au_gdp$value / 1000, df_au_gdp$date, tclass = 'yearmon'))
aud_gdp   = xts::to.monthly(aud_gdp, OHLC = FALSE, drop.time = TRUE)
aud_gdp   = 100 * log(aud_gdp)

# QUARTERLY and MONTHLY
# Quarterly CPI from FRED
aud_cpi   = fredr::fredr("AUSCPIALLQINMEI")
df_au_cpi = create_interpolated_series(aud_cpi, start_date, end_date)
aud_cpi_fr   = xts::xts(df_au_cpi$value_out, df_au_cpi$date, tclass = 'yearmon')
colnames(aud_cpi_fr) = "aud_cpi"
aud_cpi_fr = na.omit(aud_cpi_fr$aud_cpi  / as.numeric(aud_cpi_fr$aud_cpi["2017-09"]))

# Monthly CPI from ABS
aud_cpi_mont = readabs::read_abs(series_id = "A128478317T")
aud_cpi_mont = xts::xts(aud_cpi_mont$value, aud_cpi_mont$date, tclass = 'yearmon')
aud_cpi_mont = aud_cpi_mont/as.numeric(aud_cpi_mont["2017-10"])

# merge
aud_cpi   = rbind(aud_cpi_fr["/2017-08"], aud_cpi_mont)
aud_cpi   = xts::to.monthly(aud_cpi, OHLC = FALSE, drop.time = TRUE)
aud_cpi   = 100 * log(aud_cpi)

# cash rate
aud_CR    = readrba::read_rba(series_id = "FIRMMCRT")   # Cash Rate Target
aud_CR    = xts::xts(aud_CR$value, aud_CR$date, tclass = 'yearmon')
aud_CR    = xts::to.monthly(aud_CR, OHLC = FALSE, drop.time = TRUE)

# exchange rate
aud_USD   = readrba::read_rba(series_id = "FXRUSD")
aud_USD   = xts::xts(aud_USD$value, aud_USD$date, tclass = 'yearmon')
aud_USD   = xts::to.monthly(aud_USD, OHLC = FALSE, drop.time = TRUE)

aord_yahoo        = "https://query1.finance.yahoo.com/v7/finance/download/^AORD?period1=345513600&period2=1722211200&interval=1d&events=history&includeAdjustedClose=true"
aord_download     = read.csv(aord_yahoo, na.strings = "null")
aord_tmp          = xts::xts(log(aord_download[,6]), as.Date(aord_download[,1]))
aord_tmp          = na.omit(aord_tmp)
aud_aord          = xts::to.monthly(aord_tmp, OHLC = FALSE, drop.time = TRUE)

aus = na.omit(merge(aud_gdp, aud_cpi, aud_CR, aud_USD, aud_aord))
us  = na.omit(merge(rgdp, cpi, FFR))
soe = na.omit(merge(aus, us))

save(soe, file = "soe.rda")

# analyses using SVAR-SV model 
############################################################
library(bsvars)
load("soe.rda")
soe           = as.matrix(soe)

TT            = nrow(soe)
lag_order     = 8
lag_exogenous = 4
T             = TT - max(lag_order, lag_exogenous)

exogenous     = matrix(NA, TT - lag_exogenous, 0)
for (i in 0:lag_exogenous) {
  exogenous   = cbind(exogenous, as.matrix(soe[(lag_exogenous - i + 1):(TT - i), 6:8]))
}

set.seed(1234)
spec          = specify_bsvar_sv$new(
  data        = tail(soe[,1:5], T),
  p           = lag_order,
  exogenous   = tail(exogenous, T)
)

burn          = estimate(spec, 1e4)
post          = estimate(burn, 1e4)

post |> compute_impulse_responses(horizon = 60) |> plot(probability = 0.68, col = bsvars_grad)
post |> compute_variance_decompositions(horizon = 60) |> plot(col = bsvars_grad)
post |> compute_structural_shocks() |> plot(col = "#0056B9")
post |> compute_conditional_sd() |> plot(col = "#0056B9")
post |> compute_fitted_values() |> plot(col = "#0056B9")

post |> verify_identification() |> summary()

A0 = matrix(NA, 5, 56)
A0[,45:56] = 0
post |> verify_autoregression(hypothesis = A0) |> summary()

# analyses using SVAR-t model 
############################################################

set.seed(1234)
spec          = specify_bsvar_t$new(
  data        = tail(soe[,1:5], T),
  p           = lag_order,
  exogenous   = tail(exogenous, T)
)

burn          = estimate(spec, 1e4)
post          = estimate(burn, 1e4)

post |> compute_impulse_responses(horizon = 60) |> plot(probability = 0.68, col = bsvars_grad)
post |> compute_variance_decompositions(horizon = 60) |> plot(col = bsvars_grad)
post |> compute_structural_shocks() |> plot(col = "#0056B9")
post |> compute_conditional_sd() |> plot(col = "#0056B9")
post |> compute_fitted_values() |> plot(col = "#0056B9")

post |> verify_identification() |> summary()

A0 = matrix(NA, 5, 56)
A0[,45:56] = 0
post |> verify_autoregression(hypothesis = A0) |> summary()
