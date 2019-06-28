dat <- readxl::read_excel(path = "~/Downloads/2013 Final Measures_ZPM_6_28_19.xlsx",
                          sheet = 5) %>%
  magrittr::set_colnames(tolower(colnames(.))) %>%
  dplyr::rename(age = age,
                length = avgoftruelength) %>%
  dplyr::mutate(site = "Kanyawara")

# look at individuals over time
ggplot(data = dat) +
  geom_point(aes(x = date, y = length, group = chimpid_final)) +
  facet_wrap(~chimpid_final)




models <- tibble(

  model = c("Kanyawara-All", "Kanyawara-Males", "Kanyawara-Females"
  )
)


# init empty dataframes
initdf <- data.frame(age = NA, length = NA)
models$data <- lapply(1:nrow(initdf), function(x){return(initdf)}) # gross hack

# fill in the dataframes
models$data[[1]] <- dat %>%
  dplyr::filter(site == "Kanyawara")

models$data[[2]] <- dat %>%
  dplyr::filter(site == "Kanyawara" & sex == "M")

models$data[[3]] <- dat %>%
  dplyr::filter(site == "Kanyawara" & sex == "F")


#............................................
# Now pull out stuff we need for models
#.............................................
models <- models %>%
  dplyr::mutate(
    ind.obs = purrr::map(data, "age"),
    dep.obs = purrr::map(data, "length")
  )

#............................................
# Now add some initial conditoins we need
#.............................................
models$B <- purrr::map(models$dep.obs, mean) %>%
  unlist()

models$initparams <- lapply(1:nrow(models), function(x){return(c(""))})

models$initparams[[1]] <- c(B = models$B[[1]],
                            D1 = 8.835 , D2 = 9.450, D3 = 28.465, C1 = 3.355, C2 = 12.990, C3 = 1.005) # combined

models$initparams[[2]] <- c(B = models$B[[2]],
                            D1 = 9.19 , D2 = 9.97, D3 = 12.12, C1 = 4.79, C2 = 19.03, C3 = 1.22) # males

models$initparams[[3]] <- c(B = models$B[[3]],
                            D1 = 8.48 , D2 = 8.93, D3 = 44.81, C1 = 1.92, C2 = 6.95, C3 = 0.79) # females



# now drop B since we don't need it
# add reps as a parameter we are going to use
models <- models %>%
  dplyr::select(-c("B")) %>%
  dplyr::mutate(reps = 1e2)



models$ONLSmodels <- purrr::pmap(models[,c("ind.obs", "dep.obs", "initparams", "reps")],
                                 .f = function(ind.obs, dep.obs, initparams, reps){

                                   ret.ord <- parallel::mclapply(1:reps, FUN = function(x){
                                     JPPSiterweight::optimize.jpps.ord(
                                       ind.obs = ind.obs,
                                       dep.obs = dep.obs,
                                       params = initparams,
                                       optim.method = "SANN")
                                   }) # end inner function

                                   ret.ord.params <- ret.ord %>%
                                     purrr::map(., "params")
                                   ret.ord.params <- as.data.frame(do.call(rbind, ret.ord.params))
                                   return(ret.ord.params)
                                 }) # end purrr function




# models$IRNLSmodels <- purrr::pmap(models[,c("ind.obs", "dep.obs", "initparams", "reps")],
#                                   .f = function(ind.obs, dep.obs, initparams, reps){
#
#                                     ret.irnls <- parallel::mclapply(1:reps, function(x){
#                                       JPPSiterweight::optimize.jpps.iteratively_reweight(par = initparams,
#                                                                                          optim.method = "SANN",
#                                                                                          iter.method = "Nelder",
#                                                                                          tol = 1e-9, # tolerance
#                                                                                          maxit = 500, # number of iterative weight steps to perform
#                                                                                          ind.obs = ind.obs,
#                                                                                          dep.obs = dep.obs)
#                                     }) # end inner function
#
#                                     ret.irnls.params <- ret.irnls %>%
#                                       purrr::map(., "params")
#                                     ret.irnls.params <- as.data.frame(do.call(rbind, ret.irnls.params))
#                                     return(ret.irnls.params)
#                                   }) # end purrr function
#
#
#


#...................................
# Ordinary Nonlinear Least Square
#...................................

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






