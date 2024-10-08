#Script to calculate indexes for historical and current period

  # load libraries
  library(dplyr)
  library(sf)
  library(mapview)
  library(RColorBrewer)
  library(viridis)
  library(ggspatial) 
  library(ggrepel)
  library(units)
  
  # # # # # # # # # # # # # # #
  #Data manipulation for HP   
  # # # # # # # # # # # # # # #
  
  # clean environment
  rm(list = ls())
  # load Spanish UTM grid
  malla <- st_read("Spatial_Data/Malla_UTM.shp")
  malla <- st_transform(malla, crs = "EPSG:25830") 
  
  # load atlas data (1998-2002)
  AtlasHP <- read.csv("Data/Atlas_HP.csv", stringsAsFactors = F)
  
  #Changing Sylvia conspicillata to Curruca conspicillata
  AtlasHP$NOMLATIN <- gsub("Miliaria calandra", "Emberiza calandra", AtlasHP$NOMLATIN)
  AtlasHP$NOMLATIN <- gsub("Sylvia conspicillata", "Curruca conspicillata", AtlasHP$NOMLATIN)
  AtlasHP$NOMLATIN <- gsub("Calandrella rufescens", "Alaudala rufescens", AtlasHP$NOMLATIN)
  
  names (AtlasHP)[4] = "Species"
  
  AtlasHP_1 = select(AtlasHP, UTMCODE, Species)
  #Merging with spatial data of 10 x 10 km cellgrid to remove cells from Canarias
  AtlasHP_2 <- left_join(malla[, c("UTMCODE", "X_COORD", "Y_COORD", "RICHNESS_D", "RICHNESS_O")], 
                           AtlasHP_1, by = "UTMCODE") 
  
  AtlasHP_3 <- select(AtlasHP_2,Species, UTMCODE) 
  
  AtlasHP_3$geometry <- NULL
  AtlasHP_3 <- subset(AtlasHP_3, !is.na(Species))
  
  # species per cell
  Spp_percellHP <- AtlasHP_3 %>% 
    group_by(UTMCODE) %>% 
    summarize(Species = n())
  
  names (Spp_percellHP)[2] = "N_Species_HP"
  
  # # # # # # # # # # # # # # #
  #Data manipulation for CP   
  # # # # # # # # # # # # # # #
  
  # Loading atlas data (2014-2018)
  
  atlas16_0 <- read.csv("Data/Atlas_CP.csv", stringsAsFactors = F)
  #Changing Sylvia conspicillata to Curruca conspicillata
  atlas16_0$ESP_LAT <- gsub("Sylvia conspicillata", "Curruca conspicillata", atlas16_0$ESP_LAT)
  
  names(atlas16_0)[3] = "UTMCODE"
  names(atlas16_0)[6] = "Species"
  
  atlas16_0 = select(atlas16_0, UTMCODE, Species)
  #Merging with spatial data of 10 x 10 km cellgrid to remove cells from Canarias
  atlas16_01 <- left_join(malla[, "UTMCODE"], 
                       atlas16_0, by = "UTMCODE") 
  atlas16_01$geometry <- NULL
  
  #Removing NAs 
  atlas16 <- subset(atlas16_01, !is.na(Species))
  
  # Loading Ebird data 2019-2023 (26 csv files)
  
  file_list <- list.files(path="Data_Ebird/", pattern = "\\.csv$", full.names = TRUE)
  # Loading each file in a separate dataframe
  data_list <- lapply(file_list, read.csv)
  # There are some Ebird records reported as X, which are presence observations, therefore, I am assigning a 1 to those rows 
  data_list <- lapply(data_list, function(df) {
    df$max_indivs <- ifelse(df$max_indivs == "X", 1, df$max_indivs)
    return(df)
  })
  # Some values in "max_indivs" are characters, therefore transforming into numeric. 
  data_list <- lapply(data_list, function(df) {
    df$max_indivs <- as.numeric(df$max_indivs)
    return(df)
  })
  #Merging all 26 datasets 
  Steppebirds_Ebird <- bind_rows(data_list)
  
  # We are only using breeding season data, therefore selecting only records from february to August 
  Steppebirds_FebAug <- filter(Steppebirds_Ebird, month %in% c("2", "3", "4", "5", "6", "7", "8")) 
  
  # I only need to know if species were present or not in each UTM, therefore, assigning a 0 for absence and a 1 for presence. 
  Steppebirds_FebAug$presence<- ifelse(Steppebirds_FebAug$max_indivs > 0, 1, 0)
  
  #Keeping only cells with presence reports
  Steppebirds_present <- Steppebirds_FebAug[Steppebirds_FebAug$presence == 1, ]
  
  #Merging with spatial data of 10 x 10 km cellgrid 
  Steppebirds_present1 <- left_join(malla[, c("UTM10X10", "UTMCODE", "X_COORD", "Y_COORD")], 
                                    Steppebirds_present, by = c("UTM10X10" = "UTM")) #(Steppebirds_present includes records from Canarias, 
  #that is why when merging with malla, Steppebirds_present1 has less records)
  
  Steppebirds_toJ <- select(Steppebirds_present1,species, UTMCODE) 
  names (Steppebirds_toJ)[1] = "Species"
  
  Steppebirds_toJ1 <- Steppebirds_toJ
  Steppebirds_toJ1$geometry <- NULL
  Steppebirds_toJ1 <- subset(Steppebirds_toJ1, !is.na(Species))
  
  #Removing duplicates based on UTM and species
  Steppebirds_toJ2 <- distinct(Steppebirds_toJ1, UTMCODE, Species)

  #Merging Atlas 2014-2018 with Ebird 2019-2023
  JoinedDBs <-rbind(atlas16, Steppebirds_toJ2)
  #Removing duplicates based on UTM and species
  JoinedDBs1 <- distinct(JoinedDBs, UTMCODE, Species)

  # species per cell
  Spp_percellCP <- JoinedDBs1 %>% 
    group_by(UTMCODE) %>% 
    summarize(Species = n())
  
  names (Spp_percellCP)[2] = "N_Species_CP"
  
  #Identifying cells with data for both periods. 4823 out of 4823 cells have data for both periods.
  Spp_percellHPCP <- merge(Spp_percellHP, Spp_percellCP, by="UTMCODE")

  #Now just keeping rows with data for both periods
  FilteredHP <- merge(AtlasHP_3, Spp_percellHPCP, by="UTMCODE")
  FilteredCP <- merge(JoinedDBs1, Spp_percellHPCP, by="UTMCODE")
  FilteredHP$N_Species_HP <- NULL
  FilteredHP$N_Species_CP <- NULL
  
  FilteredCP$N_Species_HP <- NULL
  FilteredCP$N_Species_CP <- NULL
  
  #############################################################
  #Summarizing data per species, calculating indexes, and identifying hotspots
  #############################################################

  Species_cellHP <- FilteredHP %>% 
  group_by(Species) %>% 
  summarize(UTMCODE = n())
  
  names (Species_cellHP)[2] = "N_cellsHP"
  
  # Percentage of cells occupied by each species from the total 4823 cells
  Species_cellHP$Perc_cellsHP  <- (Species_cellHP$N_cellsHP*1/4823)

  # "rarity_classes" is assigned as 0 for common species: > 50% of cells, 5 for rare species: 25 and 50% of cells, and 10 for 
  #  very rare species: <25% of cells.
  Species_cellHP$rarityHP <- ifelse(Species_cellHP$Perc_cellsHP > 0.5, 0,
                                        ifelse(Species_cellHP$Perc_cellsHP > 0.25, 5, 10))
  
  #Including conservation status according to the Spanish Red list, European vulnerability, and SPEC categories 
  #Sources: Burfield et al 2023 (Birds in Europe 4: the 4th assessment of Species of European Conservation Concern)
  Threat_cats <-read.csv("Data/Threat_categories.csv", stringsAsFactors = F)
  
  Species_cellHP_threats <- merge(Species_cellHP, Threat_cats, by="Species")
  Species_cellHP_threats$RLS.2021  <- NULL
  Species_cellHP_threats$SCPTS.2023  <- NULL
  Species_cellHP_threats$SCPTS.2023.value  <- NULL
  
  #Using the same scoring system from Traba et al 2007 for the categories of the Spanish Red list (RLS)
  Species_cellHP_threats <- Species_cellHP_threats %>%
    mutate(RLS_VAL_2004 = case_when(
      RLS.2004 == "EN" ~ 10,
      RLS.2004 == "VU" ~ 7,
      RLS.2004 == "NT" ~ 5,
      RLS.2004 == "LC" ~ 3,
      RLS.2004 == "DD" ~ 1,
      RLS.2004 == "NE" ~ 0
    ))
  
  #Using the same scoring system from Traba et al 2007 for the European Population Status (EPS) categories
  Species_cellHP_threats <- Species_cellHP_threats %>%
    mutate(EPS_VAL_2004 = case_when(
      EPS.2004 == "EN"        ~ 10,
      EPS.2004 == "V"         ~  7,
      EPS.2004 == "DEC"       ~  5,
      EPS.2004 == "DEP"       ~  2,
      EPS.2004 == "S"         ~  0,
    ))
  
  #Using the same values from Traba et al 2007 to score species according to their SPECS category 
  Species_cellHP_threats <- Species_cellHP_threats %>%
    mutate(SPEC_VAL_2004 = case_when(
      SPEC.2004 == "1"    ~ 10,
      SPEC.2004 == "2"    ~  7,
      SPEC.2004 == "3"    ~  5,
      SPEC.2004 == "4"    ~  1,
      SPEC.2004 == "0"    ~  0,
    ))

Species_cellHP_threats1 <- select(Species_cellHP_threats, Species, rarityHP, RLS_VAL_2004, EPS_VAL_2004, SPEC_VAL_2004) 

DB_mergedHP <- merge(FilteredHP, Species_cellHP_threats1, by = "Species")

DB_mergedHP$richHP <- 1

# Aggregating indexes values per UTM 
Indexes_pcellHP <- aggregate(cbind(richHP, rarityHP, RLS_VAL_2004, EPS_VAL_2004, SPEC_VAL_2004) ~ UTMCODE, data = DB_mergedHP, FUN = sum)

#min max linear rescaling
basic_function <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Apply the rescaling function to each relevant column and store the results in new columns
Indexes_pcellHP$richHP_sc <- basic_function(Indexes_pcellHP$richHP)
Indexes_pcellHP$rarityHP_sc <- basic_function(Indexes_pcellHP$rarityHP)
Indexes_pcellHP$RLS2004_sc <- basic_function(Indexes_pcellHP$RLS_VAL_2004)
Indexes_pcellHP$EPS2004_sc <- basic_function(Indexes_pcellHP$EPS_VAL_2004)
Indexes_pcellHP$SPEC2004_sc <- basic_function(Indexes_pcellHP$SPEC_VAL_2004)

# Combine the scaled values to create a new combined index
Indexes_pcellHP$comb_indHP <- rowSums(Indexes_pcellHP[, c("richHP_sc", "rarityHP_sc", "RLS2004_sc", "EPS2004_sc", "SPEC2004_sc")], na.rm = TRUE)

#Joining with spatial data of 10 x 10 km cellgrid 
Indexes_pcellHP0 <- left_join(malla[, c("UTMCODE")], Indexes_pcellHP,
                             by = "UTMCODE")

#Sorting cells by combined_index
Indexes_pcellHP0 <- Indexes_pcellHP0[order(Indexes_pcellHP0$comb_indHP, decreasing = TRUE),]

#Removing NAs 
Indexes_pcellHP1 <- subset(Indexes_pcellHP0, !is.na(richHP))

#Selecting the 254 cells with the highest values in combined index, which represents the 5% used by Traba to determine hotspots
hotspots_HP <- Indexes_pcellHP1[1:254,]

#########
#########Loading Natural Protected areas to identify cells with/without PA, and to intersect with hotspots##########
#########

PAs2007 <- st_read("Spatial_Data/espacios protegidos.shp")
PAs2007 <- st_set_crs(PAs2007, st_crs("EPSG:25830"))

#Selecting the same categories used used by Traba 2007: "The coverage afforded by the Natural Protected areaHPs (National, Natural and Regional Parks)".
cat_selectedHP <- c("PARQUE NACIONAL", "PARQUE NATURAL", "PARQUE REGIONAL")

PAs2007 <- PAs2007[PAs2007$FIGURAPROT %in% cat_selectedHP, ]

#Assigning 1 for cells with Protected areaHPs and 0 for cells without
interseccionesHP <- st_intersection(Indexes_pcellHP1, PAs2007) 
interseccionesHP <- select(interseccionesHP, 1:12)
interseccionesHP <- distinct(interseccionesHP, UTMCODE, .keep_all = TRUE)
interseccionesHP$geometry  <- NULL
interseccionesHP$PAs <- 1
Indxs_pcellHP_PA <- merge(Indexes_pcellHP1,interseccionesHP[,c(1,13)],by="UTMCODE", all.x=TRUE)

#Assigning 0 for cells without PA
Indxs_pcellHP_PA[,13][is.na(Indxs_pcellHP_PA[,13])] <- 0

mapview(PAs2007) + mapview(hotspots_HP, zcol = "comb_indHP")

#########
# Intersecting hotspots wth PAs to calculate the coverage of PAs over hotspots
#########

# Calculating hotpots area from each polygon
areaHP <- st_area(hotspots_HP)

# Aggregating all areaHPs to have a total areaHP
areaHP_total <- sum(areaHP)

# I want the units to be removed
areaHP_total <- drop_units(areaHP_total)

# Printing the result
print(paste("Total area of hotspots is:", areaHP_total, "m2")) #total area is 25369975260.7962 m2

# Perform the intersection between hotspots HP and PAs 2007
intersectHP0 <- st_intersection(hotspots_HP, PAs2007)

# 7 unique cells intersect with protected areas. 2.76% of cells intersect with Protected areas
length(unique(intersectHP0$UTMCODE))

intersectHP0$areaHPintsct_m2 <- st_area(intersectHP0)
#Removing units from rows
intersectHP0 <- drop_units(intersectHP0)
totalintsctareaHP <- sum(intersectHP0$areaHPintsct_m2)
print(paste("Total intersect area is:", totalintsctareaHP, "m2")) #total area is 238978138.503468 m2, or 238.98 km2

(totalintsctareaHP/1000000)*100/(areaHP_total/1000000) #0.94% of hotspots area overlaps with Protected areas

#########Loading SPAs (Zonas de Especial Protección para Aves - ZEPAS in spanish) to identify cells with/without SPA, 
#########and to consolidate one single shp that merges PA and SPA presence per cell

SPAs2007 <- st_read("Spatial_Data/zepas_peninsula.shp")

#Obtaining the crs from hotspots_HP and assigning it to SPAs2007
SPAs2007 <- st_set_crs(SPAs2007, st_crs("EPSG:25830"))

mapview(SPAs2007) + mapview(hotspots_HP, zcol = "comb_indHP")

#Assigning 1 for cells with SPAs and 0 for cells without
interseccionesHP1 <- st_intersection(Indexes_pcellHP1, SPAs2007) 
interseccionesHP1 <- select(interseccionesHP1, 1:12)
interseccionesHP1 <- distinct(interseccionesHP1, UTMCODE, .keep_all = TRUE)
interseccionesHP1$geometry  <- NULL
interseccionesHP1$SPAs <- 1
Indxs_pcellHP_SPA <- merge(Indexes_pcellHP1,interseccionesHP1[,c(1,13)],by="UTMCODE", all.x=TRUE)
Indxs_pcellHP_SPA[,13][is.na(Indxs_pcellHP_SPA[,13])] <- 0
Indxs_pcellHP_SPA$geometry <- NULL

#Merging indexes, presence of PA, and SPA per cell 
Indx_PASPA_HP <- merge(Indxs_pcellHP_PA, Indxs_pcellHP_SPA [,c(1,13)], by="UTMCODE")

#if a cell contains only PAs, then "PAs" is assigned; when only SPAs, "SPAS" is assigned; if PAs and SPAs are present a 2 "PA_SPAs" is assigned, otherwise "no PA_SPAs".
Indx_PASPA_HP  <- Indx_PASPA_HP %>%
  mutate(
    PA_SPAs = case_when(
      PAs == 1 & SPAs == 0 ~ "PAs",
      PAs == 1 & SPAs == 1 ~ "PA_SPAs",
      PAs == 0 & SPAs == 1 ~ "SPAS",
      TRUE ~ "no PA_SPAs"
    )
  )

Indx_PASPA_HP$PA_SPAs <- as.factor(Indx_PASPA_HP$PA_SPAs)

#########
# Intersecting hotspots wth SPAs to calculate the coverage of SPAs over hotspots
#########

intersectHP <- st_intersection(hotspots_HP, SPAs2007)

# 123 unique cells intersect with SPAs. 48.43% of cells intersect with SPAs
length(unique(intersectHP$UTMCODE))

intersectHP$areaHPintsct_m2 <- st_area(intersectHP)
#Removing units from rows
intersectHP <- drop_units(intersectHP)
totalintersectHP <- sum(intersectHP$areaHPintsct_m2)
print(paste("Total intersect area is:", totalintersectHP, "m2")) #total area is 5352854567.42834 m2, or 5352.85 km2

(totalintersectHP/1000000)*100/(areaHP_total/1000000) #21.1% of hotspots area overlap with SPAs

#########Now calculating the percentage of overlap of hotspots inside PASPAs (considering PAs and SPAs as a single object)##########
###################################################################################################
#Disolving all attributes into a single one 
SPAs2007 <- st_union(SPAs2007) 
PAs2007 <- st_union(PAs2007) 

PASPAs2007 <- st_union(PAs2007, SPAs2007) 

# Perform the intersection
intsctPASPAHP <- st_intersection(hotspots_HP, PASPAs2007)

# 127 unique cells intersectHP with protected areaHPs. 
length(unique(intsctPASPAHP$UTMCODE))

intsctPASPAHP$areaHPintsct_m2 <- st_area(intsctPASPAHP)
#Removing units from rows
intsctPASPAHP <- drop_units(intsctPASPAHP)
totalintersectHP <- sum(intsctPASPAHP$areaHPintsct_m2)
print(paste("Total intersect area is:", totalintersectHP, "m2")) #total area is 5534394660.27794 m2, or 5534.39 km2

(totalintersectHP/1000000)*100/(areaHP_total/1000000)# 21.81% of hotspots areaHP overlap with PASPAs (PAs and SPAs as a single object)

#This shp contains the combined index for all 4823 cells in HP.
write_sf(Indx_PASPA_HP, "Spatial_Data/combindexHP_26June.shp") 

# These dbs (only considering cells with data for both periods) will be used in the script 3 when calculating the number of spp for each AC in HP and CP
write.csv(FilteredHP, "Data/FilteredHP.csv", row.names = FALSE)
write.csv(FilteredCP, "Data/FilteredCP.csv", row.names = FALSE) #This one will be used as well in the next script for CP analysis.
