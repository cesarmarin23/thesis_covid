#Comment desde editJesus
#Comentario editado en RStudio 6 Abri
#install.packages(c("reticulate", "greta", "greta.gp"))
library(reticulate)
use_condaenv('r-reticulate', required = TRUE)
library(greta)
library(greta.gp)
library(curl)
library(here)
library(dplyr)
library(data.table)
library(tidyr)
library(padr)
library(countrycode)
# if tensorflow is not installed to a virtual
# environment/conda environment, run this command
# to get the right versions of tensorflow installed

#greta::install_tensorflow(method = "conda",
                          #version = "1.14.0",
                          #extra_packages = "tensorflow-probability==0.7")

# Temporal variation in reporting - bayesian model framework
# Fit gaussian process model using greta.gp to under-reporting estimates over time

# Set paths
setwd(here::here())

#source data processing and plotting scripts
source('/Users/cesarmarin/Documents/scripts_tesis/regiones/movilidad_chile_covid/R/chile_data_import.R')
source('/Users/cesarmarin/Documents/scripts_tesis/regiones/movilidad_chile_covid/R/chile_scale_cfr.R')
source('/Users/cesarmarin/Documents/scripts_tesis/regiones/movilidad_chile_covid/R/chile_case_convolution.R')
source('/Users/cesarmarin/Documents/scripts_tesis/regiones/movilidad_chile_covid/R/run_bayesian_model.R')

# Set parameters
mean <- 13
median <- 9.1

mu_cfr <- log(median)
sigma_cfr <- sqrt(2*(log(mean) - mu_cfr))

# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x) {
  plnorm(x + 1, mu_cfr, sigma_cfr) - plnorm(x, mu_cfr, sigma_cfr)
}

#--- Load and clean data using separate function
#jhu_data <- case_death_timeseries_function()
chile_data <- timeseries_cases_death() 


#--- inference is compute and memory heavy
#--- a HPC is used to run the inference for many countries/regions
#--- therefore we pick a single country here to run 
#--- we also only run it for the timeseries from September 2020
#--- as there are memory allocation issues when running it with a longer
#--- timeseries on standard computers

#--- choosing which region to run the full inference for
iso_region <- "Metropolitana de Santiago"
 
regions_age_stratified_cfr <- read.csv(file = '/Users/cesarmarin/Documents/scripts_tesis/regiones/movilidad_chile_covid/R/regions_age_stratified_cfr.csv')

cfr_baseline_region <- regions_age_stratified_cfr %>%
  dplyr::filter(nombre_region == iso_region) %>%
  pull(cfr_mid)
  
cfr_range_high <- regions_age_stratified_cfr %>%
  dplyr::filter(nombre_region == iso_region) %>%
  pull(cfr_high)

cfr_range_low <- regions_age_stratified_cfr %>%
  dplyr::filter(nombre_region == iso_region) %>%
  pull(cfr_low)

cfr_range <- c(cfr_range_low,cfr_range_high)

inference_region = 'Metropolitana'
inference_data_chile <- cases_known_convolution(inference_region, chile_data, cfr_baseline_region)

prediction <- run_bayesian_model(inference_data_chile,
                                 n_inducing = 5,
                                 cfr_baseline = cfr_baseline_region,
                                 cfr_range = cfr_range,
                                 verbose = TRUE)

ci_poly <- tibble::tibble(x = c(plot_data$date, rev(plot_data$date)),
                          y = c(prediction$upper, rev(prediction$lower)))

library(reticulate)
use_condaenv('r-reticulate', required = TRUE)
install.packages(c("reticulate", "greta", "greta.gp"))
library(greta)
library(greta.gp)
  
