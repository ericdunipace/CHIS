library(prism)

sapply(c("tmean","tmax","ppt","vpdmax"), function(x) prism::get_prism_annual(type = x, years = 2000:2023,keepZip = FALSE))

sapply(c("tmean","tmax","ppt","vpdmax"), function(var) prism::get_prism_dailys(
  type = var,
  minDate = "2020-01-01",
  maxDate = "2023-12-31",
  keepZip = FALSE)
)

prism::get_prism_dailys(
  type = "tmax",
  minDate = "2000-01-01",
  maxDate = "2019-12-31",
  keepZip = FALSE
)