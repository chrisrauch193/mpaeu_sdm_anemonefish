library(obissdm)
library(furrr)
library(progressr)
library(storr)
library(polars)
library(arrow)
library(dplyr)

library(terra)
library(sp)
library(tidyverse)
library(raster)
library(stars)

source("functions/model_species.R")
source("functions/components_model_species.R")
source("functions/auxiliary_modelfit.R")
set.seed(2023)
handlers("cli")
options("progressr.enable" = TRUE)
if (interactive()) { cat("\014") } else { system("clear") }
start_time <- Sys.time()



# Define settings ----
# This file can also be run as "source". In that case, the default objects
# are used. To enable also a more flexible run, for testing purposes, three 
# arguments can be override by creating the objects on the environment before. 
# `Those are outfolder`, `outacro`, and `sel_species`. The latter have as 
# default the value "all", what means that all species in the species list will 
# be modeled. Supplying a vector of AphiaID will filter the species based on the
# supplied values.
#
# We create a small function to perform those checks
check_exists <- function(object_name, if_not) {
  if (exists(object_name)) {
    return(eval(parse(text = object_name)))
  } else {
    return(if_not)
  }
}


sel_species <- list.files("data/species/")
sel_species <- gsub("key=", "", sel_species)
sel_species <- gsub("\\.parquet", "", sel_species)
sel_species <- as.numeric(sel_species)
sel_species <- sel_species[(sel_species %in% c(212781, 212783, 278394, 278400, 278403, 278405, 289169, 289898, 290089, 291133, 291136))]




# General
# The output folder
outfolder <- "results"

# An unique code for identifying this model run
outacro <- "mpaasia"

# What will be modelled?
# 'all' will model all species. Supply a vector of AphiaIDs to filter
sel_species <- check_exists("sel_species", "all")
# The path for the species data dataset
species_dataset <- "data/species/"
# The path for the species list
species_list <- recent_file("data", "all_splist")


# Modelling
# Algorithms to be used
algos <- c("maxent", "rf", "xgboost")
# Personalized options
algo_opts <- obissdm::sdm_options()[algos]
algo_opts$xgboost$gamma <- c(0, 4)
algo_opts$xgboost$shrinkage <- c(0.1, 0.3)
# Should areas be masked by the species depth?
limit_by_depth <- TRUE
# A buffer to be applied to the depth limitation
depth_buffer <- 100
# Assess spatial bias?
assess_bias <- TRUE
# Quadrature size
quad_samp <- 0.01 # 1% of the total number of points
# Target metric
tg_metric <- "cbi"
# Metric threshold
tg_threshold <- 0.3

# Create storr to hold results
st <- storr_rds(paste0(outacro, "_storr"))
# If does not exist, add start date
if (!st$exists("startdate")) {
  st$set("startdate", format(Sys.Date(), "%Y%m%d"))
}
# Should the storr object be destructed at the end if the model succeeded?
destroy_storr <- FALSE


# Define species ----
species_list <- read.csv(species_list)
species_list <- get_listbygroup(species_list)

if (length(sel_species) > 1 | !any("all" %in% sel_species)) {
  species_list <- species_list %>%
    filter(taxonID %in% sel_species)
}


for (i in 1:nrow(species_list)) {
  print("RUN FOR")
  print(species_list$taxonID[i])
  print(species_list$sdm_group[i])
  
  model_species(
    species = species_list$taxonID[i],
    group = species_list$sdm_group[i],
    species_dataset = species_dataset,
    outfolder = outfolder,
    outacro = outacro,
    algorithms = algos,
    algo_opts = algo_opts,
    limit_by_depth = limit_by_depth,
    depth_buffer = depth_buffer,
    assess_bias = assess_bias,
    post_eval = c("sst", "niche", "hyper"),
    tg_metric = tg_metric,
    tg_threshold = tg_threshold,
    quad_samp = quad_samp,
    verbose = TRUE
  )
  print("RUN FINISHED")
}

# species <- 291133
# group <- "anemonefish"

