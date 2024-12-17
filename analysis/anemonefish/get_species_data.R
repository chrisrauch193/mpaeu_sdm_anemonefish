# --- Prepare Anemone and Anemonefish Species List ---
library(dplyr)
library(obissdm)
library(terra)

# Load the anemone/anemonefish species list
anemone_list <- readxl::read_excel("Species List.xlsx")
colnames(anemone_list) <- c(
  "vernacularName",
  "scientificNameWWF",
  "rank",
  "listedIn",
  "annex",
  "note"
)

head(anemone_list)

# --- Match with WoRMS ---

# Match the scientific names in your list with WoRMS using obistools
new_names <- obistools::match_taxa(anemone_list$scientificNameWWF)

# Bind the WoRMS matched names to your original list
new_anemone_list <- bind_cols(new_names, anemone_list)

# Add AphiaID (from WoRMS) and rearrange columns
new_anemone_list <- new_anemone_list %>%
  mutate(AphiaID = acceptedNameUsageID) %>%
  relocate(AphiaID, scientificName, scientificNameWWF, vernacularName)

# --- Add GBIF Information ---

# Get GBIF backbone information for each species (using the corrected scientific names)
gbif_names <- lapply(new_anemone_list$scientificName, rgbif::name_backbone)
gbif_names <- bind_rows(gbif_names)
gbif_names <- gbif_names %>%
  select(gbif_speciesKey = usageKey, gbif_scientificName = scientificName, gbif_matchType = matchType)

# Add GBIF information to your list
new_anemone_list <- new_anemone_list %>% bind_cols(gbif_names)

# --- Remove Duplicates ---
# If there are any duplicate AphiaIDs (after WoRMS matching), keep only one entry
new_anemone_list_final <- new_anemone_list %>%
  distinct(AphiaID, .keep_all = TRUE)

# --- Add Taxon ID and Taxonomic Details ---

# Use AphiaID as the taxonID
new_anemone_list_final$taxonID <- new_anemone_list_final$AphiaID

# Check if there are any missing AphiaIDs
missing_aphia_ids <- as.numeric(new_anemone_list_final$AphiaID[is.na(new_anemone_list_final$phylum)])

if (length(missing_aphia_ids) == 0) {
  cat("All species have phylum information. Skipping taxonomic details retrieval.\n")
  
  # --- Save the Processed Species List ---
  write.csv(new_anemone_list_final, "analysis/anemonefish/anemone_list.csv", row.names = FALSE)
  
} else {
  # Fetch taxonomic records using the new wm_record (handles multiple IDs)
  missing_det_list <- worrms::wm_record(id = missing_aphia_ids)
  
  # Convert the list of records to a data frame, ensuring order is maintained
  missing_det <- dplyr::bind_rows(missing_det_list, .id = "AphiaID") # Add AphiaID from list names
  
  # Convert AphiaID column in missing_det to numeric for merging
  missing_det$AphiaID <- as.numeric(missing_det$AphiaID)
  
  # Select relevant columns
  missing_det <- missing_det[, c("AphiaID", "kingdom", "phylum", "class", "order", "family")]
  
  # Merge the taxonomic details back into the main data frame using AphiaID
  new_anemone_list_final <- dplyr::left_join(new_anemone_list_final, missing_det, by = "AphiaID")
  
  # --- Save the Processed Species List ---
  write.csv(new_anemone_list_final, "analysis/anemonefish/anemone_list.csv", row.names = FALSE)
}




# Download data not available on our previous download
all_sp <- read.csv("data/all_splist_20241213.csv")

not_available <- new_anemone_list_final[!new_anemone_list_final$AphiaID %in% all_sp$taxonID,]


setwd("../mpaeu_shared/")
obissdm::mp_get_gbif(sci_names = not_available$gbif_speciesKey)
setwd("../mpaeu_sdm_anemonefish/")

# And then do the standardization
for (i in 1:length(unique(not_available$AphiaID))) {
  if (!file.exists(paste0("data/species/", "key=", unique(not_available$AphiaID)[i], ".parquet"))) {
    print("here")
    print(unique(not_available$AphiaID)[i])
    print("okay printed")
    
    cat(unique(not_available$AphiaID)[i], "\n")
    # mp_standardize(species = unique(not_available$AphiaID)[i],
    #                species_list = "analysis/anemonefish/anemone_list.csv",
    #                sdm_base = rast("data/env/current/thetao_baseline_depthsurf_mean.tif"),
    #                species_folder = "../mpaeu_shared/")
    mp_standardize(species = unique(not_available$AphiaID)[i],
                   species_list = "data/all_splist_20241213.csv",
                   sdm_base = rast("data/env/current/thetao_baseline_depthsurf_mean.tif"),
                   species_folder = "../mpaeu_shared/")
  } else {
    print("Not here")
    print(unique(not_available$AphiaID)[i])
    print("okay printedddd")
  
    cat(unique(not_available$AphiaID)[i], "done \n")
  }
}

# END