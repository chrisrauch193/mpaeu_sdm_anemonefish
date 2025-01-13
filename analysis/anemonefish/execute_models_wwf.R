
sel_species <- list.files("data/species/")
sel_species <- gsub("key=", "", sel_species)
sel_species <- gsub("\\.parquet", "", sel_species)
sel_species <- as.numeric(sel_species)
# sel_species <- sel_species[sel_species != 137205]
# sel_species <- sel_species[sel_species == 278400]
sel_species <- sel_species[(sel_species %in% c(212781, 212783, 278394, 278400, 278403, 278405, 289169, 289898, 290089, 291133, 291136))]
# sel_species <- sel_species[!(sel_species %in% c(1042871, 1043465, 1043469, 1043479, 1043480, 1043484, 1043490, 1043607, 126901, 137125, 1374498, 1567463, 1576133, 1576136, 1577351, 157869, 158530, 158667, 158668, 158669, 158670, 217430))]

# sel_species <- sel_species[(sel_species %in% c(212781, 212783, 278394))]

source("codes/model_fit.R")