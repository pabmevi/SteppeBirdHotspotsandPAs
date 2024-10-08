#Script to calculate indexes for current period

# load libraries
library(dplyr)
library(sf)
library(mapview)
library(RColorBrewer)
library(viridis)
library(ggspatial) 
library(ggrepel)
library(units)
library(mapSpain)
library(gridExtra)

# clean environment
rm(list = ls())

# load Spanish UTM grid
malla <- st_read("Spatial_Data/Malla_UTM.shp")
malla <- st_transform(malla, crs = "EPSG:25830") 

# loading the merged data from atlas (2014-2018) and ebird (2019-2023) considering only cells with data for HP and CP.
FilteredCP <- read.csv("Data/FilteredCP.csv", stringsAsFactors = F)

# summarise data per species and calculate number of cells where each species is present
Species_cell <- FilteredCP %>% 
  group_by(Species) %>% 
  summarize(UTMCODE = n())

names (Species_cell)[2] = "N_cells"

# Percentage of cells occupied by each species from the total 4823 cells
Species_cell$Perc_cells  <- (Species_cell$N_cells*1/4823)

# "rarity" is assigned as 0 for common species: >= 50% of cells, 5 for rare species: 25 and 50% of cells, and 10 for 
#  very rare species: <25% of cells.
Species_cell$rarityCP <- ifelse(Species_cell$Perc_cells > 0.5, 0,
                                ifelse(Species_cell$Perc_cells > 0.25, 5, 10))

#Including conservation status according to the Spanish Redlist, European vulnerability, and SPEC categories 
#Sources: Burfield et al 2023 (Birds in Europe 4: the 4th assessment of Species of European Conservation Concern)
Threat_cats <-read.csv("Data/Threat_categories.csv", stringsAsFactors = F)
SPECS2023 <- read.csv("Data/SPECS_2023.csv", stringsAsFactors = F) 

Species_cell_threats <- merge(Species_cell, Threat_cats, by="Species")
Species_cell_threats1 <- merge(Species_cell_threats, SPECS2023[, c(1,3,4)], by.x ="Species", by.y = "Scientific.name")
names(Species_cell_threats1)[11]= "EPS_2023" 
names(Species_cell_threats1)[12]= "SPEC_2023"

#Using the same values from Traba et al 2007 to score species according to the Spanish Red list 
Species_cell_threats1 <- Species_cell_threats1 %>%
  mutate(RLS_VAL_2021 = case_when(
    RLS.2021 == "EN" ~ 10,
    RLS.2021 == "VU" ~ 7,
    RLS.2021 == "NT" ~ 5,
    RLS.2021 == "LC" ~ 3,
    RLS.2021 == "DD" ~ 1,
    RLS.2021 == "NE" ~ 0
  ))

#Using the same values from Traba et al 2007 to score species according to their SPECS category included in BirdLife International (2017)
Species_cell_threats1 <- Species_cell_threats1 %>%
  mutate(SPEC_VAL_2023 = case_when(
    SPEC_2023 == "SPEC 1"    ~ 10,
    SPEC_2023 == "SPEC 2"    ~  7,
    SPEC_2023 == "SPEC 3"    ~  5,
    SPEC_2023 == "Non-SPEC"  ~  1,
    SPEC_2023 == "Non-SPECe" ~  0,
  ))

#Using the same values from Traba et al 2007 to score species according to their EU status category included in BirdLife International (2017)
Species_cell_threats1 <- Species_cell_threats1 %>%
  mutate(EPS_VAL_2023 = case_when(
    EPS_2023 == "EN"         ~  10,
    EPS_2023 == "VU"         ~  7,
    EPS_2023 == "NT"         ~  6,
    EPS_2023 == "Declining"  ~  5,
    EPS_2023 == "Rare"       ~  3,
    EPS_2023 == "Depleted"   ~  2,
    EPS_2023 == "SecureF"    ~  1, #in Traba et al 2007 this value was assigned to "localised" category (inexistent now)
    EPS_2023 == "Secure"     ~  0,
  ))

DB_merged <- merge(FilteredCP, Species_cell_threats1, by = "Species")

DB_merged$richCP <- 1

# Aggregating indexes values per UTM 
Indexes_pcells <- aggregate(cbind(richCP, rarityCP, RLS_VAL_2021, SPEC_VAL_2023, EPS_VAL_2023) ~ UTMCODE, data = DB_merged, FUN = sum)

#min max linear rescaling
basic_function <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Apply the rescaling function to each relevant column and store the results in new columns
Indexes_pcells$richCP_sc <- basic_function(Indexes_pcells$richCP)
Indexes_pcells$rarityCP_sc <- basic_function(Indexes_pcells$rarityCP)
Indexes_pcells$RLS2021_sc <- basic_function(Indexes_pcells$RLS_VAL_2021)
Indexes_pcells$EPS2023_sc <- basic_function(Indexes_pcells$EPS_VAL_2023)
Indexes_pcells$SPEC2023_sc <- basic_function(Indexes_pcells$SPEC_VAL_2023)

# Combine the scaled values to create a new combined index
Indexes_pcells$comb_indCP <- rowSums(Indexes_pcells[, c("richCP_sc", "rarityCP_sc", "RLS2021_sc", "EPS2023_sc", "SPEC2023_sc")], na.rm = TRUE)

#Joining with spatial data of 10 x 10 km cellgrid 
Indexes_pcells1 <- left_join(malla[, "UTMCODE"], Indexes_pcells,
                             by = "UTMCODE")

#Sorting cells by combined_index
Indexes_pcells2 <- Indexes_pcells1[order(Indexes_pcells1$comb_indCP, decreasing = TRUE),]

#Removing NAs 
Indexes_pcells2 <- subset(Indexes_pcells2, !is.na(richCP))

#Selecting the 254 cells with the highest values in combined index, which represents the 5% used by Traba to determine hotspots
hotspots_CP <- Indexes_pcells2[1:254,]

#########Now loading Natural Protected Areas shp to intersect with hotspots##########

PAs2022 <- st_read("Spatial_Data/Enp2022_p.shp")
#Selecting from PAs 2022 the same categories used for Protected Areas in 2007
cat_selected <- c("Parque Nacional", "Parque Natural", "Parque Regional")#, "Reserva Integral", "Área Marina Protegida", 
                  #"Reserva Natural de Fauna Salvaje", "Reserva Natural", "Reserva Natural Integral", "Reserva Natural Parcial")

PAs2022 <- PAs2022[PAs2022$ODESIGNATE %in% cat_selected, ]

#Projection
PAs2022 <- st_transform(PAs2022, crs = "EPSG:25830") 

#Assigning 1 for cells with Protected Areas and 0 for cells without
intersecciones <- st_intersection(Indexes_pcells2, PAs2022) 
intersecciones <- select(intersecciones, 1:12)
intersecciones <- distinct(intersecciones, UTMCODE, .keep_all = TRUE)
intersecciones$geometry  <- NULL
intersecciones$PAs <- 1
Indexes_pcells2_PA <- merge(Indexes_pcells2,intersecciones[,c(1,13)],by="UTMCODE", all.x=TRUE)

#Assigning 0 for cells without PA
Indexes_pcells2_PA[,13][is.na(Indexes_pcells2_PA[,13])] <- 0

mapview(PAs2022) + mapview(hotspots_CP, zcol = "comb_indCP")

# Calculating the area from each polygon
area <- st_area(hotspots_CP)

# Aggregating all areas to have a total area
area_total <- sum(area)
area_total <- drop_units(area_total)

# Printing the result
print(paste("Total area of hotspots is::", area_total, "m2")) #total area is 25288028610.8469 m2

# Perform the intersection
intersect0 <- st_intersection(hotspots_CP, PAs2022) 

# 25 unique hotspot cells intersect with protected areas. 9.84% intersect with Protected areas
length(unique(intersect0$UTMCODE))

intersect0$areaintsct_m2 <- st_area(intersect0)
#Removing units from rows
intersect0 <- drop_units(intersect0)
totalintersectareaAP <- sum(intersect0$areaintsct_m2)
print(paste("Total intersect area is:", totalintersectareaAP, "m2"))#intersect area is 621824226.719042 m2 or 621.824 km2

(totalintersectareaAP/1000000)*100/(area_total/1000000) #2.46% of hotspots area overlaps with Protected Areas

# Adding Autonomous communities from Spain
comm <-esp_get_ccaa()
comm <- comm[!comm$iso2.ccaa.name.es %in% c("Canarias"),]

#########Now loading SPAs (ZEPAS-Zonas de Especial Protección para las Aves) shp to intersect with hotspots##########

SPAs2022 <- st_read("Spatial_Data/Es_Lic_SCI_Zepa_SPA_Medalpatl_202212.shp")

#Obtaining the crs from hotspots_CP and assigning it to SPAs2022
SPAs2022 <- st_transform(SPAs2022, crs = st_crs(hotspots_CP))

#Selecting only type A, which is only ZEPA (this was used in Traba 2007). B is ZEC/LIC, and C is ZEc/LIC and ZEPA
SPAs2022_TYPE_AC  <- subset(SPAs2022, TIPO %in% c('A', 'C'))

mapview(SPAs2022_TYPE_AC) + mapview(hotspots_CP, zcol = "comb_indCP")

SPAs2022_TYPE_AC   <- st_make_valid(SPAs2022_TYPE_AC)
#Assigning 1 for cells with ZEPAS and 0 for cells without
intersecciones1 <- st_intersection(Indexes_pcells2_PA, SPAs2022_TYPE_AC) 
intersecciones1 <- select(intersecciones1, 1:12)
intersecciones1 <- distinct(intersecciones1, UTMCODE, .keep_all = TRUE)
intersecciones1$geometry  <- NULL
intersecciones1$SPAs <- 1
Indexes_pcells2_PA_ZPAS <- merge(Indexes_pcells2_PA,intersecciones1[,c(1,13)],by="UTMCODE", all.x=TRUE)
Indexes_pcells2_PA_ZPAS[,13][is.na(Indexes_pcells2_PA_ZPAS[,13])] <- 0

#if a cell contains only PAs, then "PAs" is assigned; when only SPAs, "SPAS" is assigned; if PAs and SPAs are present a 2 "PA_SPAs" is assigned, otherwise "no PA_SPAs".
Indexes_pcells2_PA_ZPAS  <- Indexes_pcells2_PA_ZPAS %>%
  mutate(
    PA_SPAs = case_when(
      PAs == 1 & SPAs == 0 ~ "PAs",
      PAs == 1 & SPAs == 1 ~ "PA_SPAs",
      PAs == 0 & SPAs == 1 ~ "SPAS",
      TRUE ~ "no PA_SPAs"
    )
  )

# Perform the intersection
intersect <- st_intersection(SPAs2022_TYPE_AC, hotspots_CP)

# 194 unique cells intersect with SPAs. 76.38% of cells intersect with SPAs
length(unique(intersect$UTMCODE))

intersect$areaintsct_m2 <- st_area(intersect)
#Removing units from rows
intersect <- drop_units(intersect)
totalintersectareaZP <- sum(intersect$areaintsct_m2)
print(paste("Total intersect area is:", totalintersectareaZP, "m2")) #total area is 7249894360.8098 m2, or 7249.89 km2 

(totalintersectareaZP/1000000)*100/(area_total/1000000) #28.67% of hotspots area overlap with SPAS

#########Now calculating the percentage of overlap of hotspots inside PASPAs (union of PAs and SPAs)##########

#Selecting type A and C, which was used in Traba 2007 for ZEPAs. B is ZEC/LIC, and C is ZEc/LIC and ZEPA
SPAs2022  <- subset(SPAs2022, TIPO %in% c('A', 'C'))
#Disolving all attributes into a single one 
SPAs2022 <- st_union(SPAs2022) 
PAs2022 <- st_union(PAs2022) 

PASPAs2022 <- st_union(PAs2022, SPAs2022) 

# Perform the intersection
intersect1 <- st_intersection(hotspots_CP, PASPAs2022)

# 198 unique cells intersect with protected areas. 77.95% of cells intersect with Protected areas and SPAs
length(unique(intersect1$UTMCODE))

intersect1$areaintsct_m2 <- st_area(intersect1)
#Removing units from rows
intersect1 <- drop_units(intersect1)
totalintersectarea1 <- sum(intersect1$areaintsct_m2)
print(paste("Total intersect area is:", totalintersectarea1, "m2")) #total area is 7442382797.53457 m2, or 7442.38 km2

(totalintersectarea1/1000000)*100/(area_total/1000000) #29.43% of hotspots area overlap with PA_SPAs (PAs and SPAs as a single object)

write_sf(Indexes_pcells2_PA_ZPAS, "Spatial_Data/combindexCP_26June.shp")#This shp contains the combined index for all 5070 cells.

####NOW CALCULATING DIFFERENCES BETWEEN COMBINED INDEX IN 2007 AND 2023##########

#Reading the shapefiles of combined indexes from 2007 and 2023
combindexHP <- st_read("Spatial_Data/combindexHP_26June.shp")
combindexCP <- st_read("Spatial_Data/combindexCP_26June.shp")

combindexHP$geometry <- NULL
combindexCP$geometry <- NULL

#Merging both dataframes
combindexHP_CP <- merge(combindexHP, combindexCP, by="UTMCODE")
sum(is.na(combindexHP_CP$cmb_nHP))
# Calculating the percentage of change of the combined index
combindexHP_CP$ch_combindx <- combindexHP_CP$cmb_nCP - combindexHP_CP$cmb_nHP

#Merging with spatial data of 10 x 10 km cellgrid 
combindexHP_CP_1 <- left_join(malla[, c("UTMCODE")], combindexHP_CP,
                             by = "UTMCODE")

#Removing NAs 
combindexHP_CP_1 <- subset(combindexHP_CP_1, !is.na(richHP))

mapview(combindexHP_CP_1, zcol = "ch_combindx", col.regions = brewer.pal(34,"RdYlBu"))

combindexHP_CP_1$change_cat <- ifelse(combindexHP_CP_1$ch_combindx < 0, "decrease", 
                                      ifelse(combindexHP_CP_1$ch_combindx == 0, "no change", "increase"))

table(combindexHP_CP_1$change_cat) #2807 cells decrease, 2016 cells increase

# hotspots 2007 and 2023
mapcombindexHP <- ggplot() +
  geom_sf(data = combindexHP_CP_1, aes(fill = cmb_nHP)) +
  geom_sf(data = comm, fill = "transparent", color = "black", size=5.5) +
  scale_fill_viridis(option = "turbo", na.value = "grey95", direction = 1, limits = c(0, 5), breaks = seq(0, 16, by = 4))+
  labs(x = "Longitude", y = "Latitude", fill = "comb. index") +
  annotation_scale(location = "bl", width_hint = .3) +
  theme_bw() +
  theme(title = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.position = c(0.85, 0.15),
        axis.title = element_text(size = 15, face = "plain")) + 
  guides(colour = "none", size = "none", alpha = "none")

mapcombindexCP <- ggplot() +
  geom_sf(data = combindexHP_CP_1, aes(fill = cmb_nCP)) +
  geom_sf(data = comm, fill = "transparent", color = "black", size=5.5) +
  scale_fill_viridis(option = "turbo", na.value = "grey95", direction = 1,limits = c(0, 5), breaks = seq(0, 16, by = 4))+
  labs(x = "Longitude", y = "Latitude", fill = "comb. index") +
  annotation_scale(location = "bl", width_hint = .3) +
  theme_bw() +
  theme(title = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.position = c(0.85, 0.15),
        axis.title = element_text(size = 15, face = "plain")) + 
  guides(colour = "none", size = "none", alpha = "none")

combined_plot <- grid.arrange(mapcombindexHP, mapcombindexCP, ncol = 2)

ggsave("Figures/combindsHPCP_26June.png", combined_plot, wi = 50, he = 40, un = "cm", dpi = 300)

# hotspots change 2007 vs 2023
map_change <- ggplot() +
  geom_sf(data = combindexHP_CP_1, aes(fill = ch_combindx)) +
  geom_sf(data = comm, fill = "transparent", color = "black", size=5.5) +
  scale_fill_viridis(option = "turbo", na.value = "grey95", direction = 1)+
  labs(x = "Longitude", y = "Latitude", fill = "combindex change") +
  annotation_scale(location = "bl", width_hint = .3) +
  theme_bw() +
  theme(title = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.position = c(0.85, 0.15),
        axis.title = element_text(size = 15, face = "plain")) + 
  guides(colour = "none", size = "none", alpha = "none")

map_change

ggsave("Figures/hotspotschange26june.png", map_change, wi = 20, he = 20, un = "cm", dpi = 300)
write_sf(combindexHP_CP_1, "Spatial_Data/combindexHP_CP_26June.shp")#This shp contains the combined index only for cells with data for both periods. 
#i.e, if one of the two periods had no data, that row was removed. This was done to only compare surveyed cells for both periods.
