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


IRNLSmodels$FOplots <- purrr::pmap(ONLSmodels[,c("lowerCIpredsFO", "medianCIpredsFO", "upperCIpredsFO")], .f = make_FO_plot)
IRNLSmodels$SOplots <- purrr::pmap(ONLSmodels[,c("lowerCIpredsSO", "medianCIpredsSO", "upperCIpredsSO")], .f = make_SO_plot)
IRNLSmodels$fullplots <- purrr::pmap(ONLSmodels[,c("FOplots", "SOplots")], make_fullplot)


#...................................................
# Write out Final Figures
#....................................................
figpathout <- "~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/finalfigures/lengthcubed/"
svglite::svglite(file = paste0(figpathout, "kanyawara-combined.svg"), width = 11, height = 8)
IRNLSmodels$fullplots[[1]]
graphics.off()


figpathout <- "~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/finalfigures/"
svglite::svglite(file = paste0(figpathout, "kanyawara-males.svg"), width = 11, height = 8)
IRNLSmodels$fullplots[[2]]
graphics.off()


figpathout <- "~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/finalfigures/"
svglite::svglite(file = paste0(figpathout, "kanyawara-females.svg"), width = 11, height = 8)
IRNLSmodels$fullplots[[3]]
graphics.off()

figpathout <- "~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/finalfigures/"
svglite::svglite(file = paste0(figpathout, "uganda-captive.svg"), width = 11, height = 8)
IRNLSmodels$fullplots[[4]]
graphics.off()


figpathout <- "~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/finalfigures/"
svglite::svglite(file = paste0(figpathout, "sanwa-combined.svg"), width = 11, height = 8)
IRNLSmodels$fullplots[[5]]
graphics.off()


figpathout <- "~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/finalfigures/length/"
svglite::svglite(file = paste0(figpathout, "sanwa-male.svg"), width = 11, height = 8)
IRNLSmodels$fullplots[[6]]
graphics.off()


figpathout <- "~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/finalfigures/"
svglite::svglite(file = paste0(figpathout, "sanwa-female.svg"), width = 11, height = 8)
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

readr::write_csv(x = growthest_FO, path = "~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/finalfigures/length/figure_datapoints/FO_growth_est_predictions_forcurves.csv")
readr::write_csv(x = growthveloc_SO, path = "~/Google_Drive/Kanyawara_Work/KCP_Body_Size_2012data_VisitJun2019/finalfigures/length/figure_datapoints/FO_growth_veloc_predictions_forcurves.csv")




