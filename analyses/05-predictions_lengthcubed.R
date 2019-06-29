library(JPPSiterweight)
library(tidyverse)
library(cowplot)
source("R/01-plotting.R")

models <- readRDS(file = "data/model_results_lengthcubed.RDS")


#...................................
# Ordinary Nonlinear Least Square
#...................................

ONLSmodels <- models %>%
  dplyr::select(c("model", "ONLSmodels")) %>%
  dplyr::mutate(
    # CI values
    lowerCIparams = purrr::map(ONLSmodels, function(x){apply(x, 2, quantile, prob = 0.025)}),
    medianCIparams = purrr::map(ONLSmodels, function(x){apply(x, 2, quantile, prob = 0.5)}),
    upperCIparams = purrr::map(ONLSmodels, function(x){apply(x, 2, quantile, prob = 0.975)}),

    # CI predictions first order
    lowerCIpredsFO = purrr::map(lowerCIparams, function(y){ y <- makeJPPS.firstorder_obj(y); return( predict(y, x = seq(1,50, 0.01)) )} ),
    medianCIpredsFO = purrr::map(medianCIparams, function(y){ y <- makeJPPS.firstorder_obj(y); return( predict(y, x = seq(1,50, 0.01)) )} ),
    upperCIpredsFO = purrr::map(upperCIparams, function(y){ y <- makeJPPS.firstorder_obj(y); return( predict(y, x = seq(1,50, 0.01)) )} ),

    # CI predictions second order
    lowerCIpredsSO = purrr::map(lowerCIparams, function(y){ y <- makeJPPS.secondorder_obj(y); return( predict(y, x = seq(1,50, 0.01)) )} ),
    medianCIpredsSO = purrr::map(medianCIparams, function(y){ y <- makeJPPS.secondorder_obj(y); return( predict(y, x = seq(1,50, 0.01)) )} ),
    upperCIpredsSO = purrr::map(upperCIparams, function(y){ y <- makeJPPS.secondorder_obj(y); return( predict(y, x = seq(1,50, 0.01)) )} )
  )


ONLSmodels$FOplots <- purrr::pmap(ONLSmodels[,c("lowerCIpredsFO", "medianCIpredsFO", "upperCIpredsFO")], .f = make_FO_plot)
ONLSmodels$SOplots <- purrr::pmap(ONLSmodels[,c("lowerCIpredsSO", "medianCIpredsSO", "upperCIpredsSO")], .f = make_SO_plot)
ONLSmodels$fullplots <- purrr::pmap(ONLSmodels[,c("FOplots", "SOplots")], make_fullplot)


#...................................................
# Iterative Reweighting Nonlinear Least Square
#....................................................
IRNLSmodels <- models %>%
  dplyr::select(c("model", "IRNLSmodels")) %>%
  dplyr::mutate(
    # CI values
    lowerCIparams = purrr::map(IRNLSmodels, function(x){apply(x, 2, quantile, prob = 0.025)}),
    medianCIparams = purrr::map(IRNLSmodels, function(x){apply(x, 2, quantile, prob = 0.5)}),
    upperCIparams = purrr::map(IRNLSmodels, function(x){apply(x, 2, quantile, prob = 0.975)}),

    # CI predictions first order
    lowerCIpredsFO = purrr::map(lowerCIparams, function(y){ y <- makeJPPS.firstorder_obj(y); return( predict(y, x = seq(1,50, 0.01)) )} ),
    medianCIpredsFO = purrr::map(medianCIparams, function(y){ y <- makeJPPS.firstorder_obj(y); return( predict(y, x = seq(1,50, 0.01)) )} ),
    upperCIpredsFO = purrr::map(upperCIparams, function(y){ y <- makeJPPS.firstorder_obj(y); return( predict(y, x = seq(1,50, 0.01)) )} ),

    # CI predictions second order
    lowerCIpredsSO = purrr::map(lowerCIparams, function(y){ y <- makeJPPS.secondorder_obj(y); return( predict(y, x = seq(1,50, 0.01)) )} ),
    medianCIpredsSO = purrr::map(medianCIparams, function(y){ y <- makeJPPS.secondorder_obj(y); return( predict(y, x = seq(1,50, 0.01)) )} ),
    upperCIpredsSO = purrr::map(upperCIparams, function(y){ y <- makeJPPS.secondorder_obj(y); return( predict(y, x = seq(1,50, 0.01)) )} )
  )


IRNLSmodels$FOplots <- purrr::pmap(IRNLSmodels[,c("lowerCIpredsFO", "medianCIpredsFO", "upperCIpredsFO")], .f = make_FO_plot)
IRNLSmodels$SOplots <- purrr::pmap(IRNLSmodels[,c("lowerCIpredsSO", "medianCIpredsSO", "upperCIpredsSO")], .f = make_SO_plot)
IRNLSmodels$fullplots <- purrr::pmap(IRNLSmodels[,c("FOplots", "SOplots")], make_fullplot)


#...................................................
# Write out Final Figures
#....................................................
pathout <- "~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/final/"
svglite::svglite(file = paste0(pathout, "lengthcubed/kanyawara-combined.svg"), width = 11, height = 8)
IRNLSmodels$fullplots[[1]]
graphics.off()


svglite::svglite(file = paste0(pathout, "lengthcubed/kanyawara-males.svg"), width = 11, height = 8)
IRNLSmodels$fullplots[[2]]
graphics.off()


svglite::svglite(file = paste0(pathout, "lengthcubed/kanyawara-females.svg"), width = 11, height = 8)
IRNLSmodels$fullplots[[3]]
graphics.off()

svglite::svglite(file = paste0(pathout, "lengthcubed/uganda-captive.svg"), width = 11, height = 8)
IRNLSmodels$fullplots[[4]]
graphics.off()


svglite::svglite(file = paste0(pathout, "lengthcubed/sanwa-combined.svg"), width = 11, height = 8)
IRNLSmodels$fullplots[[5]]
graphics.off()


svglite::svglite(file = paste0(pathout, "lengthcubed/sanwa-male.svg"), width = 11, height = 8)
IRNLSmodels$fullplots[[6]]
graphics.off()


svglite::svglite(file = paste0(pathout, "lengthcubed/sanwa-female.svg"), width = 11, height = 8)
IRNLSmodels$fullplots[[7]]
graphics.off()

#...................................................
# Write out data that underlies figures
#....................................................
growthest_FO <- IRNLSmodels %>%
  dplyr::select(c("model", "medianCIpredsFO")) %>%
  tidyr::unnest()

growthveloc_SO <- IRNLSmodels %>%
  dplyr::select(c("model", "medianCIpredsSO")) %>%
  tidyr::unnest()

readr::write_csv(x = growthest_FO, path = paste0(pathout, "lengthcubed/FO_growth_est_predictions_forcurves.csv"))
readr::write_csv(x = growthveloc_SO, path = paste0(pathout, "lengthcubed/SO_growth_veloc_predictions_forcurves.csv"))

#...................................................
# Write out params from models
#....................................................
growthest_params_ONLS <- ONLSmodels %>%
  dplyr::select(c("model", "lowerCIparams", "medianCIparams", "upperCIparams")) %>%
  tidyr::unnest()


growthest_params_IRNLS <- IRNLSmodels %>%
  dplyr::select(c("model", "lowerCIparams", "medianCIparams", "upperCIparams")) %>%
  tidyr::unnest()


readr::write_csv(x = growthest_params_ONLS, path = paste0(pathout, "lengthcubed/ONLS_params_ests.csv"))
readr::write_csv(x = growthest_params_IRNLS, path = paste0(pathout, "lengthcubed/IRNLS_params_ests.csv"))




