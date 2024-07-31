
# download the small-open economy data from the FRED database
# based on Groshenny, Javed (2023, WP)
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

############################################################
# to set
start_date = "1950-01-01"
end_date   = "2024-07-27"



# US data
############################################################
# QUARTERLY
# real gdp
gdp       = fredr::fredr("GDPC1")
df_gdp    = create_interpolated_series(gdp, start_date, end_date)
gdp       = xts::xts(df_gdp$value_out / 1000, df_gdp$date, tclass = 'yearmon')
gdp       = xts::to.monthly(gdp, OHLC = FALSE, drop.time = TRUE)
gdp       = 100 * log(gdp)
# dgdp      = na.omit(12 * diff(gdp))
rm(df_gdp)
plot(gdp)

# Consumer Price Index: All Items for the United States
cpi       = fredr::fredr("USACPIALLMINMEI")
cpi       = xts::xts(cpi$value, cpi$date, tclass = 'yearmon')
cpi       = 100 * log(cpi)
# pi        = na.omit(12 * diff(cpi))
plot(cpi)

# Federal Funds Effective Rate
FFR       = fredr::fredr("FEDFUNDS")
FFR       = xts::xts(FFR$value, FFR$date, tclass = 'yearmon')
plot(FFR)


# Australian data
############################################################

aud_mb = readrba::read_rba(series_id = "DMABMN")
aud_mb    = xts::xts(log(aud_mb$value), aud_mb$date, tclass = 'yearmon')
aud_mb    = xts::to.monthly(aud_mb, OHLC = FALSE, drop.time = TRUE)
plot(aud_mb)

aud_hw = readrba::read_rba(series_id = "GLFMHW")
aud_hw    = xts::xts(log(aud_hw$value), aud_hw$date, tclass = 'yearmon')
aud_hw    = xts::to.monthly(aud_hw, OHLC = FALSE, drop.time = TRUE)
plot(aud_hw)


# QUARTERLY
# Real Gross Domestic Product for Australia (Domestic Currency, Seasonally Adjusted)
aud_gdp   = fredr::fredr("NGDPRSAXDCAUQ")
df_au_gdp = create_interpolated_series(aud_gdp, start_date, end_date)
aud_gdp   = na.omit(xts::xts(df_au_gdp$value / 1000, df_au_gdp$date, tclass = 'yearmon'))
aud_gdp   = xts::to.monthly(aud_gdp, OHLC = FALSE, drop.time = TRUE)
aud_gdp   = 100 * log(aud_gdp)
rm(df_au_gdp)
plot(aud_gdp)

# QUARTERLY and MONTHLY
# Quarterly CPI from FRED
aud_cpi   = fredr::fredr("AUSCPIALLQINMEI")
df_au_cpi = create_interpolated_series(aud_cpi, start_date, end_date)
aud_cpi_fr   = xts::xts(df_au_cpi$value_out, df_au_cpi$date, tclass = 'yearmon')
colnames(aud_cpi_fr) = "aud_cpi"
aud_cpi_fr = na.omit(aud_cpi_fr$aud_cpi /as.numeric(aud_cpi_fr$aud_cpi["2017-09"]))

# Monthly CPI from ABS
aud_cpi_mont = readabs::read_abs(series_id = "A128478317T")
aud_cpi_mont = xts::xts(aud_cpi_mont$value, aud_cpi_mont$date, tclass = 'yearmon')
aud_cpi_mont = aud_cpi_mont/as.numeric(aud_cpi_mont["2017-10"])

# merge
aud_cpi   = rbind(aud_cpi_fr["/2017-08"], aud_cpi_mont)
aud_cpi   = xts::to.monthly(aud_cpi, OHLC = FALSE, drop.time = TRUE)
aud_cpi   = 100 * log(aud_cpi)
plot(aud_cpi)

# 3-Month or 90-day Rates and Yields: Interbank Rates for Australia (Percent, Not Seasonally Adjusted) 
aud_IR    = fredr::fredr("IR3TIB01AUM156N")
aud_IR    = xts::xts(aud_IR$value, aud_IR$date, tclass = 'yearmon')
plot(aud_IR)

# cash rate
aud_CR    = readrba::read_rba(series_id = "FIRMMCRT")   # Cash Rate Target
aud_CR    = xts::xts(aud_CR$value, aud_CR$date, tclass = 'yearmon')
aud_CR    = xts::to.monthly(aud_CR, OHLC = FALSE, drop.time = TRUE)
plot(aud_CR)

# TS
aud_LTR   = readrba::read_rba(series_id = "FCMYGBAG10")
aud_LTR   = xts::xts(aud_LTR$value, aud_LTR$date, tclass = 'yearmon')
aud_LTR   = xts::to.monthly(aud_LTR, OHLC = FALSE, drop.time = TRUE)
aud_TS    = na.omit(aud_LTR - aud_IR)
colnames(aud_TS) = "aud_TS"

# exchange rate
aud_USD   = readrba::read_rba(series_id = "FXRUSD")
aud_USD   = xts::xts(aud_USD$value, aud_USD$date, tclass = 'yearmon')
aud_USD   = xts::to.monthly(aud_USD, OHLC = FALSE, drop.time = TRUE)
plot(aud_USD)

aord_yahoo        = "https://query1.finance.yahoo.com/v7/finance/download/^AORD?period1=345513600&period2=1722211200&interval=1d&events=history&includeAdjustedClose=true"
aord_download     = read.csv(aord_yahoo, na.strings = "null")
aord_tmp          = xts::xts(log(aord_download[,6]), as.Date(aord_download[,1]))
aord_tmp          = na.omit(aord_tmp)
aud_aord          = xts::to.monthly(aord_tmp, OHLC = FALSE, drop.time = TRUE)
plot(aud_aord)

us = na.omit(merge(gdp, cpi, FFR))
aud = na.omit(merge(aud_gdp, aud_cpi, aud_CR, aud_USD, aud_aord))
soe = na.omit(merge(aud, us))
save(soe, file = "empiRical/data/soe.rda")
