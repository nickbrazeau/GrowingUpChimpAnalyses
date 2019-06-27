library(JPPSiterweight)
library(tidyverse)
library(cowplot)

models <- readRDS(file = "data/model_results.RDS")



ONLSmodels <- models %>%
  dplyr::select(c("model", "ONLSmodels")) %>%
  dplyr::mutate(
    # CI values
    lowerCIparams = purrr::map(ONLSmodels, function(x){apply(x, 2, quantile, prob = 0.025)}),
    medianCIparams = purrr::map(ONLSmodels, function(x){apply(x, 2, quantile, prob = 0.55)}),
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

