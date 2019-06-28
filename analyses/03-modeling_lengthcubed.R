library(JPPSiterweight)
library(tidyverse)
library(remotes)

dat <- readRDS("data/chimpanzee_lengthage_averages.rds") %>%
  dplyr::mutate(lengthcubed = length^3)
reps_to_run <- 1e2


#................................................
# Make Modeling Object for Comparisons
#...............................................
models <- tibble(

  model = c("Kanyawara-All", "Kanyawara-Males", "Kanyawara-Females",
            "Uganda-Captive-All", # don't have enough to split males and females
            "Sanwa-All", "Sanwa-Males", "Sanwa-Females"
            )
)


# init empty dataframes
initdf <- data.frame(age = NA, lengthcubed = NA)
models$data <- lapply(1:nrow(initdf), function(x){return(initdf)}) # gross hack

# fill in the dataframes
models$data[[1]] <- dat %>%
  dplyr::filter(site == "Kanyawara")

models$data[[2]] <- dat %>%
  dplyr::filter(site == "Kanyawara" & sex == "M")

models$data[[3]] <- dat %>%
  dplyr::filter(site == "Kanyawara" & sex == "F")


models$data[[4]] <- dat %>%
  dplyr::filter(site == "Uganda-Captive")

models$data[[5]] <- dat %>%
  dplyr::filter(site == "Sanwa")

models$data[[6]] <- dat %>%
  dplyr::filter(site == "Sanwa" & sex == "M")

models$data[[7]] <- dat %>%
  dplyr::filter(site == "Sanwa" & sex == "F")


#............................................
# Now pull out stuff we need for models
#.............................................
models <- models %>%
  dplyr::mutate(
    ind.obs = purrr::map(data, "age"),
    dep.obs = purrr::map(data, "lengthcubed")
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

models$initparams[[4]] <- c(B = models$B[[4]],
                            D1 = 8.835 , D2 = 9.450, D3 = 28.465, C1 = 3.355, C2 = 12.990, C3 = 1.005) # combined

models$initparams[[5]] <- c(B = models$B[[5]],
                            D1 = 8.835 , D2 = 9.450, D3 = 28.465, C1 = 3.355, C2 = 12.990, C3 = 1.005) # combined

models$initparams[[6]] <- c(B = models$B[[6]],
                            D1 = 9.19 , D2 = 9.97, D3 = 12.12, C1 = 4.79, C2 = 19.03, C3 = 1.22) # males

models$initparams[[7]] <- c(B = models$B[[7]],
                            D1 = 8.48 , D2 = 8.93, D3 = 44.81, C1 = 1.92, C2 = 6.95, C3 = 0.79) # females

# now drop B since we don't need it
# add reps as a parameter we are going to use
models <- models %>%
  dplyr::select(-c("B")) %>%
  dplyr::mutate(reps = reps_to_run)



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




models$IRNLSmodels <- purrr::pmap(models[,c("ind.obs", "dep.obs", "initparams", "reps")],
                                 .f = function(ind.obs, dep.obs, initparams, reps){

                                   ret.irnls <- parallel::mclapply(1:reps, function(x){
                                     JPPSiterweight::optimize.jpps.iteratively_reweight(par = initparams,
                                                                                        optim.method = "SANN",
                                                                                        iter.method = "Nelder",
                                                                                        tol = 1e-9, # tolerance
                                                                                        maxit = 500, # number of iterative weight steps to perform
                                                                                        ind.obs = ind.obs,
                                                                                        dep.obs = dep.obs)
                                   }) # end inner function

                                   ret.irnls.params <- ret.irnls %>%
                                     purrr::map(., "params")
                                   ret.irnls.params <- as.data.frame(do.call(rbind, ret.irnls.params))
                                   return(ret.irnls.params)
                                   }) # end purrr function



# save out for predictions
saveRDS(models, file = "data/model_results_lengthcubed.RDS")


