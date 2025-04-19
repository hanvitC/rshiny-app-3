# deploy.R

library(rsconnect)

rsconnect::setAccountInfo(
  name = "columbia5243-team9",
  token = "032478A762ED83074185F80A2E24E57C",
  secret = "F28EWe4CNYBEvsvMK0U48bNCR5v876zrginZB5xj"
)

setwd("~/Desktop/CapytoolDeploy")

rsconnect::deployApp(
  appPrimaryDoc = "capytool_app.R",
  appName = "capytool_final",
  forceUpdate = TRUE  
)
