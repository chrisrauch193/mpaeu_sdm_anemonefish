
sel_species <- list.files("data/species/")
sel_species <- gsub("key=", "", sel_species)
sel_species <- gsub("\\.parquet", "", sel_species)
sel_species <- as.numeric(sel_species)
#sel_species <- sel_species[sel_species != 137205]
sel_species <- sel_species[!(sel_species %in% c(212781, 212783, 278394, 278400, 278403, 278405, 289169, 289898, 290089, 291133, 291136))]

source("codes/model_fit.R")