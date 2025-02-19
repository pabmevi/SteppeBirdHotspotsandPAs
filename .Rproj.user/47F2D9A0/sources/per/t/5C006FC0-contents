#Script to identify taxonomic priorities For protection
library(sf)
library(dplyr)
library(cowplot)
library(scales)
library(car)
library(data.table)
library(writexl)

rm(list = ls())
setwd("~/GitHub/SteppeBirdHotspotsandPAs")

####CALCULATING DATA FOR SPECIES IN HISTORICAL PERIOD (HP)########################################
########################################################################################################

db3 <- st_read("Spatial_Data/HPCP_PASPA_27June.shp")
Threat_cats <-read.csv("Data/Threat_categories.csv", stringsAsFactors = F)

FilteredHP <- read.csv("Data/FilteredHP.csv", stringsAsFactors = F)
FilteredCP <- read.csv("Data/FilteredCP.csv", stringsAsFactors = F)
malla <- st_read("Spatial_Data/Malla_UTM.shp")

# hotspots maps selecting the 5% of the highest scores
hotspotsfinal_HP <- db3[order(db3$cmbn_HP, decreasing = TRUE),]
hotspotsfinal_CP <- db3[order(db3$cmbn_CP, decreasing = TRUE),]

#Selecting the 254 cells with the highest values in combined index, which represents the 5% used by Traba to determine hotspots
hotspotsfinal_HP <- hotspotsfinal_HP[1:254,]
hotspotsfinal_CP <- hotspotsfinal_CP[1:254,]

#Spotting the species present in the hotspots area from historical period. 
Hotspots_species1 <- merge(FilteredHP, hotspotsfinal_HP[,c(1,8)], by="UTMCODE", all.y=TRUE)
unique(Hotspots_species1$Species) #25 species present in hotspots are in HP

# Number of hotspot cells that each species occupy
Species_hots1 <- Hotspots_species1 %>% 
  group_by(Species) %>% 
  summarize(UTMCODE = n())

# summarise data per species and calculate number of cells where each species is present
Species_cell <- FilteredHP %>% 
  group_by(Species) %>% 
  summarize(UTMCODE = n())

names (Species_cell)[2] = "N_cells_HP"    

# Percentage of cells occupied by each species from the total 4823 cells
Species_cell$Perc_cells_HP  <- round(Species_cell$N_cells_HP*100/4823, digits=2)

Species_cell_threats <- merge(Species_cell, Threat_cats, by="Species")
Species_data_P1 <- merge(Species_cell_threats, Species_hots1, by="Species", all.x = T)

names (Species_data_P1)[10] = "N_hotspot_cells_HP"
Species_data_P1$RLS.2021<- NULL
Species_data_P1$SCPTS.2023<- NULL
Species_data_P1$SCPTS.2023.value<- NULL

####CALCULATING DATA FOR SPECIES IN CURRENT PERIOD (CP)########################################
###############################################################################################

#Spotting the species present in the hotspots area from historical period.
Hotspots_species2 <- merge(FilteredCP, hotspotsfinal_CP[,c(1,8)], by="UTMCODE", all.y=TRUE)
unique(Hotspots_species2$Species)

# Number of hotspot cells that each species occupy
Species_hots2 <- Hotspots_species2 %>% 
  group_by(Species) %>% 
  summarize(UTMCODE = n())

# summarise data per species and calculate number of cells where each species is present
Species_cell2 <- FilteredCP %>% 
  group_by(Species) %>% 
  summarize(UTMCODE = n())

names (Species_cell2)[2] = "N_cells_CP"

# Percentage of cells occupied by each species from the total 5070 cells
Species_cell2$Perc_cells_CP  <- round(Species_cell2$N_cells_CP*100/4823, digits=2)

# "rarity" is assigned as 0 for common species: >= 50% of cells, 5 for rare species: 25 and 50% of cells, and 10 for 
#  very rare species: <25% of cells.
Species_cell2$Rarity_CP <- ifelse(Species_cell2$Perc_cells_CP > 50, 0,
                                  ifelse(Species_cell2$Perc_cells_CP > 25, 5, 10))

#Including conservation status according to the Spanish Redlist, European vulnerability, and SPEC categories 
#Sources: Burfield et al 2023 (Birds in Europe 4: the 4th assessment of Species of European Conservation Concern)
SPECS2023 <- read.csv("Data/SPECS_2023.csv", stringsAsFactors = F) 

Species_cell_threats1 <- merge(Species_cell2, Threat_cats, by="Species")
Species_cell_threats1 <- merge(Species_cell_threats1, SPECS2023[, c(1,3,4)], by.x ="Species", by.y = "Scientific.name")

names(Species_cell_threats1)[11]= "EPS.2023"
names(Species_cell_threats1)[12]= "SPEC.2023"

#Using the same values from Traba et al 2007 to score species according to the Spanish Red list 
Species_cell_threats1 <- Species_cell_threats1 %>%
  mutate(RLS_2021_value = case_when(
    RLS.2021 == "EN" ~ 10,
    RLS.2021 == "VU" ~ 7,
    RLS.2021 == "NT" ~ 5,
    RLS.2021 == "LC" ~ 3,
    RLS.2021 == "DD" ~ 1,
    RLS.2021 == "NE" ~ 0
  ))

#Using the same values from Traba et al 2007 to score species according to their EU status category included in BirdLife International (2017)
Species_cell_threats1 <- Species_cell_threats1 %>%
  mutate(EPS_2023_value = case_when(
    EPS.2023 == "EN"         ~  10,
    EPS.2023 == "VU"         ~  7,
    EPS.2023 == "NT"         ~  6,
    EPS.2023 == "Declining"  ~  5,
    EPS.2023 == "Rare"       ~  3,
    EPS.2023 == "Depleted"   ~  2,
    EPS.2023 == "SecureF"    ~  1, #in Traba et al 2007 this value was assigned to "localised" category (inexistent now)
    EPS.2023 == "Secure"     ~  0,
  ))

#Using the same values from Traba et al 2007 to score species according to their SPECS category included in BirdLife International (2017)
Species_cell_threats1 <- Species_cell_threats1 %>%
  mutate(SPEC_2023_value = case_when(
    SPEC.2023 == "SPEC 1"    ~ 10,
    SPEC.2023 == "SPEC 2"    ~  7,
    SPEC.2023 == "SPEC 3"    ~  5,
    SPEC.2023 == "Non-SPEC"  ~  1,
    SPEC.2023 == "Non-SPECe" ~  0,
  ))

Species_cell_threats1$RLS.2004  <- NULL
Species_cell_threats1$EPS.2004  <- NULL
Species_cell_threats1$SPEC.2004  <- NULL

Species_data_P2 <- merge(Species_cell_threats1, Species_hots2, by="Species")

names (Species_data_P2)[13] = "N_hotspot_cells_CP"

Species_data_P1_P2 <- merge(Species_data_P1, Species_data_P2, by="Species")

# Replacing the NA of C. cursor in N hotspots P1 for a 0
Species_data_P1_P2$N_hotspot_cells_HP[is.na(Species_data_P1_P2$N_hotspot_cells_HP)] <- 0

#Percentage changes in the number of cells occupied by each species, as percentage as well, and change in the number of hotspot cells   
Species_data_P1_P2$pch_cells  <- round((Species_data_P1_P2$N_cells_CP - Species_data_P1_P2$N_cells_HP)*100/ Species_data_P1_P2$N_cells_HP, 2)
Species_data_P1_P2$ch_cells  <- round((Species_data_P1_P2$Perc_cells_CP - Species_data_P1_P2$Perc_cells_HP), 2)

TaxPrior <- Species_data_P1_P2

TaxPrior$RLS.2004  <- NULL
TaxPrior$EPS.2004  <- NULL
TaxPrior$SPEC.2004  <- NULL
TaxPrior$N_hotspot_cells_HP  <- NULL
TaxPrior$RLS.2021  <- NULL
TaxPrior$SCPTS.2023  <- NULL
TaxPrior$EPS.2023  <- NULL
TaxPrior$SPEC.2023  <- NULL
TaxPrior$N_hotspot_cells_CP  <- NULL

#Calculating correlations among indices
corrmatrix0 <- TaxPrior[, c(6:11)]
corrmatrix0 <- corrmatrix0[, c(6, 1, 3, 4, 5, 2)]
corrmatrix0  <- as.data.frame(cor(corrmatrix0))
write_xlsx(corrmatrix0, 'Data/CIs_indices_correlation.xlsx') 

#min max linear rescaling
basic_function <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# inverted rescaling for cells ocuppancy
inverted_function <- function(x) {
  1 - basic_function(x)
}

# Apply the rescaling function to each relevant column and store the results in new columns
TaxPrior$pch_cells_sc <- round(inverted_function(TaxPrior$pch_cells), 2)
TaxPrior$ch_cells_sc <- round(inverted_function(TaxPrior$ch_cells), 2)
TaxPrior$rarity <- round(basic_function(TaxPrior$Rarity_CP), 2)
TaxPrior$SCPTS <- round(basic_function(TaxPrior$SCPTS.2023.value),2)
TaxPrior$RLS <- round(basic_function(TaxPrior$RLS_2021_value),2)
TaxPrior$EPS <- round(basic_function(TaxPrior$EPS_2023_value),2)
TaxPrior$SPEC <- round(basic_function(TaxPrior$EPS_2023_value),2)

#Aggregating values per species to have a combined index using SCPTS and another one that uses RLS instead of SCPTS 
combindex_SCPTS <- rowSums(TaxPrior[, c("pch_cells_sc", "rarity", "SCPTS", "EPS", "SPEC")]) # WITH PERCENTAGE CHANGE
combindex_RLS <- rowSums(TaxPrior[, c("pch_cells_sc", "rarity", "RLS", "EPS", "SPEC")]) # WITH PERCENTAGE CHANGE

combindex_SCPTS1 <- rowSums(TaxPrior[, c("ch_cells_sc", "rarity", "SCPTS", "EPS", "SPEC")]) # WITH ABSOLUTE CHANGE - JUST SUBSTRACTING VALUES FROM CP AND HP
combindex_RLS1 <- rowSums(TaxPrior[, c("ch_cells_sc", "rarity", "RLS", "EPS", "SPEC")]) # WITH ABSOLUTE CHANGE - JUST SUBSTRACTING VALUES FROM CP AND HP

# Adding the combined indexes to TaxPrior
TaxPrior$combindex_SCPTS <- round(combindex_SCPTS, digits=2)
TaxPrior$combindex_RLS <- round(combindex_RLS, digits=2)
TaxPrior$combindex_SCPTS1 <- round(combindex_SCPTS1, digits=2)
TaxPrior$combindex_RLS1 <- round(combindex_RLS1, digits=2)

# Calculating quantiles to divide values in high, medium and low values
quantiles_SCPTS <- quantile(TaxPrior$combindex_SCPTS, probs = c(0.333, 0.667))
quantiles_RLS <- quantile(TaxPrior$combindex_RLS, probs = c(0.333, 0.667))
quantiles_SCPTS1 <- quantile(TaxPrior$combindex_SCPTS1, probs = c(0.333, 0.667))
quantiles_RLS1 <- quantile(TaxPrior$combindex_RLS1, probs = c(0.333, 0.667))

# Assigning high, medium and low categories
TaxPrior$Priority_SCPTS <- cut(TaxPrior$combindex_SCPTS, breaks = c(-Inf, quantiles_SCPTS, Inf), labels = c("low", "medium", "high"), include.lowest = TRUE)
TaxPrior$Priority_RLS <- cut(TaxPrior$combindex_RLS, breaks = c(-Inf, quantiles_RLS, Inf), labels = c("low", "medium", "high"), include.lowest = TRUE)
TaxPrior$Priority_SCPTS1 <- cut(TaxPrior$combindex_SCPTS1, breaks = c(-Inf, quantiles_SCPTS1, Inf), labels = c("low", "medium", "high"), include.lowest = TRUE)
TaxPrior$Priority_RLS1 <- cut(TaxPrior$combindex_RLS1, breaks = c(-Inf, quantiles_RLS1, Inf), labels = c("low", "medium", "high"), include.lowest = TRUE)

# Now plotting the frequencies of CI values

basic_function <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
TaxPrior$combindex_SCPTS <- basic_function(TaxPrior$combindex_SCPTS)
TaxPrior$combindex_RLS <- basic_function(TaxPrior$combindex_RLS)

threshold_SCPTS <- quantile(TaxPrior$combindex_SCPTS, 0.667, na.rm = TRUE)
threshold_RLS <- quantile(TaxPrior$combindex_RLS, 0.667, na.rm = TRUE)

# Crear histograma para cmbindx_HP
SCPTSfreq <- ggplot(TaxPrior, aes(x = combindex_SCPTS)) +
  geom_histogram(bins = 30, fill = "gray", color = "black", alpha = 0.7) +
  geom_vline(xintercept = threshold_SCPTS, color = "red", linetype = "dashed", linewidth = 1) +
  ggtitle("") +
  xlab("Species-based combined index (SCPTS)") +
  ylab("Frequency") +
  theme_minimal()

# Crear histograma para cmbindx_CP
RLSfreq <- ggplot(TaxPrior, aes(x = combindex_RLS)) +
  geom_histogram(bins = 30, fill = "gray", color = "black", alpha = 0.7) +
  geom_vline(xintercept = threshold_RLS, color = "red", linetype = "dashed", linewidth = 1) +
  ggtitle("") +
  xlab("Species-based combined index (RLS)") +
  ylab("Frequency") +
  theme_minimal()

# Mostrar los histogramas
print(SCPTSfreq)
print(RLSfreq)

# Saving as CSV
write.csv(TaxPrior, "Data/taxpriorities11February.csv", row.names = FALSE)
