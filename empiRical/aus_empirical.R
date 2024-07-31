
# soe00: idUR-SVAR-SV
#################################################

model = "00"

library(bsvars)
load("empiRical/data/soe.rda")

TT            = nrow(soe)
lag_order     = 8
lag_exogenous = 4
T             = TT - max(lag_order, lag_exogenous)

# exogenous     = matrix(NA, TT - lag_exogenous, 0)
# for (i in 0:lag_exogenous) {
#   exogenous   = cbind(exogenous, as.matrix(soe[(lag_exogenous - i + 1):(TT - i), 6:8]))
# }

set.seed(1234)
spec          = specify_bsvar$new(
  # data        = as.matrix(tail(soe[,1:5], T)),
  data        = as.matrix(soe[,1:8]),
  p           = lag_order,
  # exogenous   = tail(exogenous, T)
)

burn          = estimate(spec, 1e3)
post          = estimate(burn, 5e3)

irfs          = compute_impulse_responses(post, horizon = 60)
plot(irfs)
# fevd          = compute_variance_decompositions(post, horizon = 60)

# save(spec, post, file = paste0("results/soe", model, ".rda"))