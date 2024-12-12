############################## MPA Europe project ##############################
########### WP3 - Species and biogenic habitat distributions (UNESCO) ##########
# March of 2024
# Authors: Silas C. Principe, Pieter Provoost
# Contact: s.principe@unesco.org
#
############################ Check for colinearity #############################

# Load packages
library(obissdm)
library(robis)
library(arrow)
library(terra)
library(dplyr)

# Settings
set.seed(2023)


# All croping and masking will be done for each layer
# The depth of cut is registered in the model log file

# Here we assess the VIF of the chosen layers
# We do this for each depth/group combination
# The final decision on which remain is, however, done by hand



# Retrieve data from GitHub
# download.file("https://github.com/iobis/mpaeu_sdm/raw/66c89b61eef69b29378075b923133e7c66d23a70/snapshot/std_records_20231027.parquet",
#               destfile = "records.parquet")
# 
# download.file("https://github.com/iobis/mpaeu_sdm/raw/66c89b61eef69b29378075b923133e7c66d23a70/snapshot/std_splist_20231027.parquet",
#               destfile = "splist.parquet")

# Load species data
occ <- open_dataset("records.parquet")
occ

# The species list we can load in its full
splist <- read_parquet("splist.parquet")
head(splist)


# Chose species
# tg_sp <- 290090
tg_sp <- 100803
conf_path <- "sdm_conf.yml"


# Get the path to the configuration file
# download.file("https://raw.githubusercontent.com/iobis/mpaeu_sdm/main/sdm_conf.yml", "sdm_conf.yml")
conf_path <- "sdm_conf.yml"
# If you want to see the content, just run the following two lines:
conf_content <- yaml::read_yaml(conf_path)
conf_content


# Include list of species groups in the list
splist_groups <- get_listbygroup(splist, conf_path)

head(splist_groups)



# Select one hypothesis
chos_hypothesis <- "basevars"

# Get the group of the species
chos_group <- splist_groups$sdm_group[splist_groups$taxonID == tg_sp]

# Get the variables list
chos_vars <- get_conf("sdm_conf.yml", what = "variables")
chos_vars <- chos_vars$variables[[chos_group]][[chos_hypothesis]]


env <- rast("data/env/current/thetao_baseline_depthsurf_mean.tif")
plot(env)




# Get env data
env_sdm <- get_envofgroup(chos_group)

env_sdm

# Get data for only the chosen species
pts <- occ %>%
  filter(taxonID == tg_sp) %>%
  collect() # We use collect here to get the data from the Parquet dataset

head(pts)







# Prepare layers
clip_area <- ext(-41, 47, 20, 89)

mask_area <- vect("data/shapefiles/mpa_europe_starea_v2.shp")

groups <- names(get_conf(what = "groups")$groups)
# depth_env <- c("depthsurf", "depthmean")
depth_env <- c("depthsurf")

test_grid <- expand.grid(groups, depth_env, stringsAsFactors = F)


# Create a list to hold the results of vif step
vif_step_list <- list()

print("HERE 1")

# for (i in 1:nrow(test_grid)) {
#   # env_layers <- get_envofgroup(group = test_grid[i, 1],
#   #                              depth = test_grid[i, 2],
#   #                              load_all = T,
#   #                              verbose = TRUE)
#   env_layers <- get_envofgroup(chos_group)
# 
#   # nams <- unique(unlist(env_layers$hypothesis))
#   # nams <- nams[!grepl("wavefetch", nams)]
#   # 
#   # env_layers <- terra::subset(env_layers$layers, nams)
# 
#   vif_step_list[[i]] <- usdm::vifstep(env_layers, th = 5)
# }

env_layers <- get_envofgroup(chos_group)
vif_step_list[[i]] <- usdm::vifstep(env_layers, th = 5)

print("HERE 2")

vif_step_list[[1]] <- vif_step_list[[2]]

vif_step_list

# See which ones have collinearity problems
which_excluded <- lapply(1:length(vif_step_list), function(x) {
  exc <- vif_step_list[[x]]@excluded
  if (length(exc) > 0) {
    cat(x, ">>>", test_grid[x, 1], "|", test_grid[x, 2], "\n")
    cat(exc, "\n\n")
    return(x)
  } else {
    return(invisible(NULL))
  }
})

print("HERE 22")

to_check <- unlist(which_excluded)

to_check

print("HERE 3")


# For those we will get the correlation matrix
vif_list <- list()

for (i in to_check) {
  env_layers <- get_envofgroup(group = test_grid[i, 1],
                               depth = test_grid[i, 2],
                               load_all = T,
                               verbose = FALSE)

  nams <- unique(unlist(env_layers$hypothesis))
  nams <- nams[!grepl("wavefetch", nams)]

  env_layers <- terra::subset(env_layers$layers, nams)

  vif_list[[i]] <- usdm::vif(env_layers)
}

print("HERE 4")

# Print the results
for (i in to_check) {
  cat(i, ">>", test_grid[i, 1], test_grid[i, 2], "\n")
  print(vif_list[[i]])
  cat("\n")
}


print("HERE 5")

# The problem is, in general, between thetao-* and some other variable
# The high correlation between SST and air temperature was expected.
# However, because patterns change specially close to the poles, we will keep both

# We will now try to resolve the problem between rugosity and slope

# To open the doc:
# rstudioapi::documentOpen("sdm_conf.yml")


# After edited, check again
for (i in to_check) {
  env_layers <- get_envofgroup(group = test_grid[i, 1],
                               depth = test_grid[i, 2],
                               load_all = T,
                               verbose = FALSE)

  nams <- unique(unlist(env_layers$hypothesis))
  nams <- nams[!grepl("wavefetch", nams)]

  env_layers <- terra::subset(env_layers$layers, nams)

  vif_list[[i]] <- usdm::vif(env_layers)
}

# Print the results
for (i in to_check) {
  cat(i, ">>", test_grid[i, 1], test_grid[i, 2], "\n")
  print(vif_list[[i]])
  cat("\n")
}

# Temperature still have a high VIF. This is expected, temperature is usually
# strongly correlated with other variables.
env_layers <- get_envofgroup(group = test_grid[1, 1],
                             depth = test_grid[1, 2],
                             load_all = T,
                             verbose = T)

nams <- unique(unlist(env_layers$hypothesis))
nams <- nams[!grepl("wavefetch", nams)]
nams <- nams[!grepl("tas_mean", nams)] # Remove just for testing

env_layers <- terra::subset(env_layers$layers, nams)

usdm::vif(env_layers)
check_res <- usdm::vifcor(env_layers)
check_res@corMatrix

# However, SST is an essential variable. We then keep this variable and the remaining for this first run.
# We will also keep O2 and PAR, although they are causing some problems with the VIF

# We save the results
names(vif_step_list) <- paste0(test_grid[,1], "_", test_grid[,2])
names(vif_list) <- names(vif_step_list)[min(to_check):max(to_check)]
all_results <- list(
  vif_before = vif_step_list,
  vif_after = vif_list[-which(sapply(vif_list, is.null))]
)

fs::dir_create("data/log")
saveRDS(all_results, file = paste0("data/log/vif_list_", format(Sys.Date(), "%Y%m%d"), ".rds"))
