#Script to calculate the areas differences between Protected areas and SPAs in 2022 and 2004. 
#Also to calculate the changes in species richness and conservation status (LR, SPEC and EU) between both periods.

# load libraries
library(sf)
library(dplyr)
library(units)
library(viridis)
library(ggspatial) 
library(ggplot2)
library(mapSpain)
library(writexl)
library(mapview)
library(RColorBrewer)
library(ggrepel)
library(gridExtra)

# clean environment
rm(list = ls())

setwd("~/GitHub/SteppeBirdHotspotsandPAs")

combindex_HP_CP <- st_read("Spatial_Data/combindexHP_CP_26June.shp")
Protectedareas2004 <- st_read("Spatial_Data/espacios protegidos.shp")
Protectedareas2022 <- st_read("Spatial_Data/Protected areas_enp_2022/Enp2022_p.shp")
malla <- st_read("Spatial_Data/Malla_municipios/Malla_UTM.shp")
malla <- st_transform(malla, crs = "EPSG:25830") 

#Obtaining the crs from combindex_HP_CP and assigning it to Protectedareas2004
crs_existente <- st_crs(combindex_HP_CP)
Protectedareas2004 <- st_set_crs(Protectedareas2004, crs_existente)

#Removing raw values of indexes to keep just scaled values. I am only keeping raw richness
combindex_HP_CP$rartyHP <- NULL
combindex_HP_CP$RLS_VAL_x <- NULL
combindex_HP_CP$EPS_VAL_x <- NULL
combindex_HP_CP$SPEC_VA_x <- NULL
combindex_HP_CP$rartyCP <- NULL
combindex_HP_CP$RLS_VAL_y <- NULL
combindex_HP_CP$SPEC_VA_y <- NULL
combindex_HP_CP$EPS_VAL_y <- NULL

names (combindex_HP_CP)[2] = "rich_HP"
names (combindex_HP_CP)[3] = "rich_HP_sc"
names (combindex_HP_CP)[4] = "rarity_HP_sc"
names (combindex_HP_CP)[5] = "RLS_2004_sc"
names (combindex_HP_CP)[6] = "EPS_2004_sc"
names (combindex_HP_CP)[7] = "SPEC_2004_sc"
names (combindex_HP_CP)[8] = "cmbindx_HP"
names (combindex_HP_CP)[9] = "PAs_2004"
names (combindex_HP_CP)[10] = "SPAs_2004"
names (combindex_HP_CP)[11] = "PA_SPAs_2004"
names (combindex_HP_CP)[12] = "rich_CP"
names (combindex_HP_CP)[13] = "rich_CP_sc"
names (combindex_HP_CP)[14] = "rarity_CP_sc"
names (combindex_HP_CP)[15] = "RLS_2021_sc"
names (combindex_HP_CP)[16] = "EPS_2022_sc"
names (combindex_HP_CP)[17] = "SPEC_2022_sc"
names (combindex_HP_CP)[18] = "cmbindx_CP"
names (combindex_HP_CP)[19] = "PAs_2022"
names (combindex_HP_CP)[20] = "SPAs_2022"
names (combindex_HP_CP)[21] = "PA_SPAs_2022"
names (combindex_HP_CP)[22] = "ch_cmbindx"

#Selecting from the same categories used by Traba 2004
cat_selected0 <- c("PARQUE NACIONAL", "PARQUE NATURAL", "PARQUE REGIONAL")

cat_selected <- c("Parque Nacional", "Parque Natural", "Parque Regional")#, "Reserva Integral", "Área Marina Protegida", 
                  #"Reserva Natural de Fauna Salvaje", "Reserva Natural", "Reserva Natural Integral", "Reserva Natural Parcial")

Protectedareas2004 <- Protectedareas2004[Protectedareas2004$FIGURAPROT %in% cat_selected0, ]
Protectedareas2022 <- Protectedareas2022[Protectedareas2022$ODESIGNATE %in% cat_selected, ]

# Disolver todas las categorías de las áreas protegidas para que sean un solo objeto
Protectedareas2004 <- st_union(Protectedareas2004)
Protectedareas2022 <- st_union(Protectedareas2022)

# Calcular el área de las áreas protegidas en cada celda para 2004
interseccion_2004 <- st_intersection(combindex_HP_CP, Protectedareas2004)
interseccion_2004$areaPA_2004 <- st_area(interseccion_2004)

# Calcular el área de las áreas protegidas en cada celda para 2022
interseccion_2022 <- st_intersection(combindex_HP_CP, Protectedareas2022)
interseccion_2022$areaPA_2022 <- st_area(interseccion_2022)

combindex_HP_CPx <- combindex_HP_CP

interseccion_2004$geometry <- NULL
combindex_HP_CPx$geometry <- NULL

# Unir los dos conjuntos de datos
datos <- merge(combindex_HP_CPx, interseccion_2004[,c(1,24)], by = "UTMCODE", all.x = TRUE)
datos <- merge(datos, interseccion_2022[,c(1,25)], by = "UTMCODE", all.x = TRUE)

# New Protected areas may have arisen in 2022, so I assigned a 0 to all NA, so I can calculate the differences between 2004 and 2022.
datos0  <- datos
datos0$areaPA_2022[is.na(datos0$areaPA_2022)] <- 0
datos0$areaPA_2004[is.na(datos0$areaPA_2004)] <- 0

# Calculating areas difference
datos0$differencePAs <- datos0$areaPA_2022 - datos0$areaPA_2004

names(datos0)
Indexes_HP_CP00 <- datos0[, c(1:25,27)]
Indexes_HP_CP0 <- left_join(malla[, c("UTMCODE")], Indexes_HP_CP00,
                              by = "UTMCODE")

Indexes_HP_CP <- subset(Indexes_HP_CP0, !is.na(cmbindx_HP))

########### SPAs 2004 vs 2022 difference SPAs ###################

SPAs2004 <- st_read("Spatial_Data/zepas_peninsula.shp")
SPAs2022 <- st_read("Spatial_Data/Es_Lic_SCI_Zepa_SPA_Medalpatl_202212.shp")
crs_existente <- st_crs(Protectedareas2022)

#Obtaining the crs from crs_existente and assigning it to Protectedareas2004
SPAs2004 <- st_set_crs(SPAs2004, crs_existente)
#Obtaining the crs from hotspots2022 and assigning it to SPAs2022
SPAs2022 <- st_transform(SPAs2022, crs = st_crs(SPAs2004))

#Selecting type A and C, as this was used in Traba 2004. A is ZEPA, B is ZEC/LIC, and C is ZEc/LIC and ZEPA
SPAs2022_TYPE_AC  <- subset(SPAs2022, TIPO %in% c('A', 'C'))

#Correcting invalid geometries
SPAs2022_TYPE_AC <- st_make_valid(SPAs2022_TYPE_AC)

# Disolving all SPAs categories into one single object
SPAs2004 <- st_union(SPAs2004)
SPAs2022_TYPE_AC <- st_union(SPAs2022_TYPE_AC)

# Calculating SPAs area for each cell in 2004
interseccion_Z2004 <- st_intersection(Indexes_HP_CP, SPAs2004)
interseccion_Z2004$area_Z2004 <- st_area(interseccion_Z2004)

# Calculating SPAs area for each cell in 2022
interseccion_Z2022 <- st_intersection(Indexes_HP_CP, SPAs2022_TYPE_AC)
interseccion_Z2022$area_Z2022 <- st_area(interseccion_Z2022)

interseccion_Z2004$geometry  <- NULL
Indexes_HP_CP$geometry  <- NULL

# Unir los dos conjuntos de datos
dataZ <- merge(Indexes_HP_CP, interseccion_Z2004[,c(1,27)], by = "UTMCODE", all.x = TRUE)
dataZ <- merge(dataZ, interseccion_Z2022[,c(1,28)], by = "UTMCODE", all.x = TRUE)

# New SPAs may have arisen in 2022, so I assigned a 0 to all NA, so I can calculate the differences between 2004 and 2022.
dataZ0  <- dataZ
dataZ0$area_Z2004[is.na(dataZ0$area_Z2004)] <- 0
dataZ0$area_Z2022[is.na(dataZ0$area_Z2022)] <- 0

# Calculating areas difference
dataZ0$differenceSPAs <- dataZ0$area_Z2022 - dataZ0$area_Z2004
names(dataZ0)
Indexes_HP_CP_PA_SPAs <- dataZ0[, c(1:28,30)]

#Calculating the percentage of change of richness between HP and CP. Using the raw value, as this will allow to have a better idea of differences
# in richness change between both periods. This is not the case for the other indices 
Indexes_HP_CP_PA_SPAs$pch_rich <- (Indexes_HP_CP_PA_SPAs$rich_CP - Indexes_HP_CP_PA_SPAs$rich_HP) * 100 / Indexes_HP_CP_PA_SPAs$rich_HP

#Calculating the percentage of change of rarity between both periods
Indexes_HP_CP_PA_SPAs$pch_rarity <- (Indexes_HP_CP_PA_SPAs$rarity_CP_sc - Indexes_HP_CP_PA_SPAs$rarity_HP_sc) * 100 / Indexes_HP_CP_PA_SPAs$rarity_HP_sc

#Calculating the percentage of change of conservation status according to the RLS from 2004 and 2021
Indexes_HP_CP_PA_SPAs$pch_RLS <- (Indexes_HP_CP_PA_SPAs$RLS_2021_sc - Indexes_HP_CP_PA_SPAs$RLS_2004_sc) * 100 / Indexes_HP_CP_PA_SPAs$RLS_2004_sc

#Calculating the percentage of change of conservation status according to the SPEC between 2004 and 2022
Indexes_HP_CP_PA_SPAs$pch_SPEC <- (Indexes_HP_CP_PA_SPAs$SPEC_2022_sc - Indexes_HP_CP_PA_SPAs$SPEC_2004_sc) * 100 / Indexes_HP_CP_PA_SPAs$SPEC_2004_sc

#Calculating the percentage of change of conservation status according to the EPS between 2004 and 2022 
Indexes_HP_CP_PA_SPAs$pch_EPS <- (Indexes_HP_CP_PA_SPAs$EPS_2022_sc - Indexes_HP_CP_PA_SPAs$EPS_2004_sc) * 100 / Indexes_HP_CP_PA_SPAs$EPS_2004_sc
Indexes_HP_CP_PA_SPAs$chng_ct <- NULL

names (Indexes_HP_CP_PA_SPAs)[23] = "areaPA_04"
names (Indexes_HP_CP_PA_SPAs)[24] = "areaPA_22"
names (Indexes_HP_CP_PA_SPAs)[25] = "difPAs"
names (Indexes_HP_CP_PA_SPAs)[26] = "areaSPA_04"
names (Indexes_HP_CP_PA_SPAs)[27] = "areaSPA_22"
names (Indexes_HP_CP_PA_SPAs)[28] = "difSPAs"

#Some variables contain values in m2, so I am dropping units
Indexes_HP_CP_PA_SPAs <- drop_units(Indexes_HP_CP_PA_SPAs)

Indexes_HP_CP_PA_SPAs1 <- left_join(malla[, c("UTMCODE")], Indexes_HP_CP_PA_SPAs,
                 by = "UTMCODE")

Indexes_HP_CP_PA_SPAs1 <- subset(Indexes_HP_CP_PA_SPAs1, !is.na(cmbindx_HP))

# I also want to analyse PAs and SPAs as a single object. So, I am merging both shapes from 2004 and 2022, and disolving. 
# Previously I had calculated the areas of PAs and SPAs and just aggregated both values per cell to have the total areas of PAs and SPAs,
# However, as PAs and SPAs overlap, I was having inflated values. Therefore, first I had to dissolve PAs and SPAs to calculate their
# area as a whole for each cell

# Merging PAs 2004 and SPAs2004 as a single object
union_PAs2004 <- st_union(Protectedareas2004, SPAs2004)

# Calculating PAsSPAs (PA and SPA as a single object) area for each cell in 2004
intersc_PAsSPAs2004 <- st_intersection(Indexes_HP_CP_PA_SPAs1, union_PAs2004)
intersc_PAsSPAs2004$area_PAsSPAs2004 <- st_area(intersc_PAsSPAs2004)
intersc_PAsSPAs2004$area_PAsSPAs2004 <- intersc_PAsSPAs2004$area_PAsSPAs2004
intersc_PAsSPAs2004 <- drop_units(intersc_PAsSPAs2004) #Some variables contain values in m2, so I am dropping units
intersc_PAsSPAs2004$area_PAsSPAs2004 <- intersc_PAsSPAs2004$area_PAsSPAs2004/1000000

# PAs 2022 and SPAs 2022 (so PA and SPAs as a single object)
union_PAs2022 <- st_union(Protectedareas2022, SPAs2022_TYPE_AC)

# Calculating the area of PAsSPAs for each cell in 2022
intersc_PAsSPAs2022 <- st_intersection(Indexes_HP_CP_PA_SPAs1, union_PAs2022)
intersc_PAsSPAs2022$area_PAsSPAs2022 <- st_area(intersc_PAsSPAs2022)
intersc_PAsSPAs2022$area_PAsSPAs2022 <- intersc_PAsSPAs2022$area_PAsSPAs2022
intersc_PAsSPAs2022 <- drop_units(intersc_PAsSPAs2022) #Some variables contain values in m2, so I am dropping units
intersc_PAsSPAs2022$area_PAsSPAs2022 <- intersc_PAsSPAs2022$area_PAsSPAs2022/1000000

intersc_PAsSPAs2004$geometry  <- NULL
intersc_PAsSPAs2022$geometry  <- NULL

# Merging the 2 datasets
dataZ1 <- merge(Indexes_HP_CP_PA_SPAs1, intersc_PAsSPAs2004[,c(1,34)], by = "UTMCODE", all.x = TRUE)
dataZ1 <- merge(dataZ1, intersc_PAsSPAs2022[,c(1,34)], by = "UTMCODE", all.x = TRUE)

# New SPAs may have arisen in 2022, so I assignated a 0 to all NA, so I can calculate the differences between 2004 and 2022.
dataZ01  <- dataZ1
dataZ01$area_PAsSPAs2004[is.na(dataZ01$area_PAsSPAs2004)] <- 0
dataZ01$area_PAsSPAs2022[is.na(dataZ01$area_PAsSPAs2022)] <- 0

# Calculating the difference of areas
dataZ01$df_PAsSPAs <- dataZ01$area_PAsSPAs2022 - dataZ01$area_PAsSPAs2004
names(dataZ01)
Indexes_HP_CP_complete <- dataZ01[, c(1:35,37)]

#Calculating maintaining, gain, or no PAs/SPAs at cells level. I do not consider increases or decreases of areas,
# If there was a PA in HP, and a SPA in CP, then, this is a maintainance no matter the area. The primary idea was to 
#compare loss, none, maintenance, and gain, however, we just had 38 loss cells, which would not be comparable with the
#other categories: none (1969), maintenance (1693), gain (1123). Therefore, loss cells were assigned to none cells.
Indexes_HP_CP_complete <- Indexes_HP_CP_complete %>%
  mutate(PASPAS_gaincell = case_when(
    PA_SPAs_2004 == "PAs" & PA_SPAs_2022 == "PAs" ~  "maintenance", 
    PA_SPAs_2004 == "PAs" & PA_SPAs_2022 == "PA_SPAs" ~ "maintenance", 
    PA_SPAs_2004 == "PAs" & PA_SPAs_2022 == "SPAS" ~  "maintenance", 
    PA_SPAs_2004 == "SPAS" & PA_SPAs_2022 == "SPAS" ~  "maintenance", 
    PA_SPAs_2004 == "SPAS" & PA_SPAs_2022 == "PAS" ~  "maintenance", 
    PA_SPAs_2004 == "SPAS" & PA_SPAs_2022 == "PA_SPAs" ~  "maintenance", 
    PA_SPAs_2004 == "PA_SPAs" & PA_SPAs_2022 == "PA_SPAs" ~  "maintenance",
    PA_SPAs_2004 == "PA_SPAs" & PA_SPAs_2022 == "SPAS" ~  "maintenance", 
    PA_SPAs_2004 == "PA_SPAs" & PA_SPAs_2022 == "PAs" ~  "maintenance", 
    PA_SPAs_2004 == "no PA_SPAs" & PA_SPAs_2022 == "PAs" ~  "gain", 
    PA_SPAs_2004 == "no PA_SPAs" & PA_SPAs_2022 == "SPAS" ~  "gain", 
    PA_SPAs_2004 == "no PA_SPAs" & PA_SPAs_2022 == "PA_SPAs" ~  "gain", 
    PA_SPAs_2004 == "PAs" & PA_SPAs_2022 == "no PA_SPAs" ~  "none", 
    PA_SPAs_2004 == "SPAS" & PA_SPAs_2022 == "no PA_SPAs" ~  "none", 
    PA_SPAs_2004 == "PA_SPAs" & PA_SPAs_2022 == "no PA_SPAs" ~  "none",
    PA_SPAs_2004 == "no PA_SPAs" & PA_SPAs_2022 == "no PA_SPAs" ~  "none", 
    
  ))
sum(is.na(Indexes_HP_CP_complete))

#Now we calculate loss, maintaining, gain, or no PAs/SPAs considering areas. So any increase 
#or decrease in area comparing HP and CP is considered
Indexes_HP_CP_complete$PASPAS_gainarea <- ifelse(Indexes_HP_CP_complete$area_PAsSPAs2004 == 0 & Indexes_HP_CP_complete$area_PAsSPAs2022 == 0, "none",
                    ifelse(Indexes_HP_CP_complete$area_PAsSPAs2022 > Indexes_HP_CP_complete$area_PAsSPAs2004, "gain",
                           ifelse(Indexes_HP_CP_complete$area_PAsSPAs2022 < Indexes_HP_CP_complete$area_PAsSPAs2004, "loss", "maintenance")))

# map difference PAsSPAs 2004 vs 2022
mapDifPAs_SPAs <- ggplot() +
  geom_sf(data = Indexes_HP_CP_complete, aes(fill = df_PAsSPAs)) +
  scale_fill_viridis(option = "turbo", na.value = "grey95", direction = -1 )+
  labs(x = "Longitude", y = "Latitude", fill = "PA_SPAS g/l" ) +
  annotation_scale(location = "bl", width_hint = .3) +
  theme_bw() +
  theme(title = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.position = c(0.85, 0.15),
        axis.title = element_text(size = 15, face = "plain")) + 
  guides(colour = "none", size = "none", alpha = "none")

Indexes_HP_CP_complete$PAs_2004 <- NULL
Indexes_HP_CP_complete$SPAs_2004 <- NULL
Indexes_HP_CP_complete$PAs_2022 <- NULL
Indexes_HP_CP_complete$SPAs_2022 <- NULL

names(Indexes_HP_CP_complete)[34] <- "PASPAS_cell"
names(Indexes_HP_CP_complete)[35] <- "PASPAS_area"

write_sf(Indexes_HP_CP_complete, "Spatial_Data/HPCP_PASPA_27June.shp") #This shp has 4823 cells because those cells without richness data were removed.

################################################################################
#Stats per AC
################################################################################

#Loading ACs
comm <-esp_get_ccaa()
comm <- comm[!comm$iso2.ccaa.name.es %in% c("Canarias"),]
comm <- st_transform(comm, st_crs(Indexes_HP_CP_complete))

############## Overlap with autonomous communities to calculate number of spp per AC in HP and CP

#Loading the files with spp names and UTMs generated in Script 1 for HP and CP
FilteredHP <- read.csv("Data/FilteredHP.csv", stringsAsFactors = F)
FilteredCP <- read.csv("Data/FilteredCP.csv", stringsAsFactors = F)

FilteredHP_shp <- left_join(malla[, c("UTMCODE")], FilteredHP,
                            by = "UTMCODE")

FilteredHP_shp <- subset(FilteredHP_shp, !is.na(Species))

#Stats per AC in HP
ACstatsHP <- st_intersection(FilteredHP_shp, comm)

ACstatsHP1 <- ACstatsHP %>%
  distinct(Species, ccaa.shortname.en, .keep_all = TRUE)

ACstatsHP2 <- subset(ACstatsHP1, !is.na(Species))

ACstatsHP2 <- ACstatsHP2 %>% 
  group_by(ccaa.shortname.en) %>% 
  summarize(Species = n()) 

names(ACstatsHP2)[2] = "N_spp_HP"

FilteredCP_shp <- left_join(malla[, c("UTMCODE")], FilteredCP,
                            by = "UTMCODE")

FilteredCP_shp <- subset(FilteredCP_shp, !is.na(Species))

#Stats per AC in CP
ACstatsCP <- st_intersection(FilteredCP_shp, comm)

ACstatsCP1 <- ACstatsCP %>%
  distinct(Species, ccaa.shortname.en, .keep_all = TRUE)

ACstatsCP2 <- subset(ACstatsCP1, !is.na(Species))

ACstatsCP2 <- ACstatsCP2 %>% 
  group_by(ccaa.shortname.en) %>% 
  summarize(Species = n()) 

names(ACstatsCP2)[2] = "N_spp_CP"

ACstatsHP2$geometry <- NULL
ACstatsCP2$geometry <- NULL

ACstatsHPCP <- merge(ACstatsHP2, ACstatsCP2, by="ccaa.shortname.en")

# Spatial intersection between ACs and Indexes_HP_CP_complete to have statistics per AC

Indexes_HP_CP_complete1 <- subset(Indexes_HP_CP_complete, !is.na(rich_HP))

celdas_por_comunidad <- st_intersection(Indexes_HP_CP_complete1, comm)
# Stats per AC
resultados <- celdas_por_comunidad %>%
  group_by(ccaa.shortname.en) %>%
  summarise(
    mean_richHP = mean(rich_HP),
    sd_richHP = sd(rich_HP),
    mean_rich_CP = mean(rich_CP),
    sd_rich_CP = sd(rich_CP),
    rich_ch = round((mean_rich_CP - mean_richHP) * 100 / mean_richHP, 2),
    mean_rarity_HP = mean(rarity_HP_sc),
    sd_rarity_HP = sd(rarity_HP_sc),
    mean_rarity_CP = mean(rarity_CP_sc),
    sd_rarity_CP = sd(rarity_CP_sc),
    rarity_ch = round((mean_rarity_CP - mean_rarity_HP) * 100 / mean_rarity_HP, 2),
    mean_RLS_2004 = mean(RLS_2004_sc),
    sd_RLS_2004 = sd(RLS_2004_sc),
    mean_RLS_2021 = mean(RLS_2021_sc),
    sd_RLS_2021 = sd(RLS_2021_sc),
    RLS_ch = round((mean_RLS_2021 - mean_RLS_2004) * 100 / mean_RLS_2004, 2),
    mean_EPS_2004 = mean(EPS_2004_sc),
    sd_EPS_2004 = sd(EPS_2004_sc),
    mean_EPS_2022 = mean(EPS_2022_sc),
    sd_EPS_2022 = sd(EPS_2022_sc),
    EPS_ch = round((mean_EPS_2022 - mean_EPS_2004) * 100 / mean_EPS_2004, 2),
    mean_SPEC_2004 = mean(SPEC_2004_sc),
    sd_SPEC_2004 = sd(SPEC_2004_sc),
    mean_SPEC_2022 = mean(SPEC_2022_sc),
    sd_SPEC_2022 = sd(SPEC_2022_sc),
    SPEC_ch = round((mean_SPEC_2022 - mean_SPEC_2004) * 100 / mean_SPEC_2004, 2),
    n_cells = n()
  )
# Adjusting format for mean and SD columns. I want that each mean value has its SD value in brackets in the same column.
resultados1 <- resultados %>%
  mutate(
    mean_richHP = paste(round(mean_richHP, 2), " (", round(sd_richHP, 2), ")", sep = ""),
    mean_rich_CP = paste(round(mean_rich_CP, 2), " (", round(sd_rich_CP, 2), ")", sep = ""),
    mean_rarity_HP = paste(round(mean_rarity_HP, 2), " (", round(sd_rarity_HP, 2), ")", sep = ""),
    mean_rarity_CP = paste(round(mean_rarity_CP, 2), " (", round(sd_rarity_CP, 2), ")", sep = ""),
    mean_RLS_2004 = paste(round(mean_RLS_2004, 2), " (", round(sd_RLS_2004, 2), ")", sep = ""),
    mean_RLS_2021 = paste(round(mean_RLS_2021, 2), " (", round(sd_RLS_2021, 2), ")", sep = ""),
    mean_EPS_2004 = paste(round(mean_EPS_2004, 2), " (", round(sd_EPS_2004, 2), ")", sep = ""),
    mean_EPS_2022 = paste(round(mean_EPS_2022, 2), " (", round(sd_EPS_2022, 2), ")", sep = ""),
    mean_SPEC_2004 = paste(round(mean_SPEC_2004, 2), " (", round(sd_SPEC_2004, 2), ")", sep = ""),
    mean_SPEC_2022 = paste(round(mean_SPEC_2022, 2), " (", round(sd_SPEC_2022, 2), ")", sep = ""))

# Removing SD columns
resultados1 <- resultados1 %>% select(-contains("sd_"))

resultados_Comun <- as.data.frame(resultados1)

# hotspots maps selecting the 5% of the highest scores
hotspfinal_HP <- Indexes_HP_CP_complete1[order(Indexes_HP_CP_complete1$cmbindx_HP, decreasing = TRUE),]
hotspfinal_CP <- Indexes_HP_CP_complete1[order(Indexes_HP_CP_complete1$cmbindx_CP, decreasing = TRUE),]

#Selecting the 254 cells with the highest values in combined index, which represents the 5% used by Traba to determine hotspots
hotspfinal_HP <- hotspfinal_HP[1:254,]
hotspfinal_CP <- hotspfinal_CP[1:254,]

# Spatial intersect between ACs and cells
hotspots_perAC_HP <- st_intersection(hotspfinal_HP, comm)
hotspots_perAC_CP <- st_intersection(hotspfinal_CP, comm)

# Stats per AC. Although there are 254 cells, when adding all cells per community we obtained 277 cells
# because there are some cells that fall into two autonomous communities.

hotspotspcellHP <- hotspots_perAC_HP %>% 
  group_by(ccaa.shortname.en) %>% 
  summarize(UTMCODE = n())

hotspotspcellCP <- hotspots_perAC_CP %>% 
  group_by(ccaa.shortname.en) %>% 
  summarize(UTMCODE = n())

names (hotspotspcellHP)[2] = "N hotspots cells HP" 
names (hotspotspcellCP)[2] = "N hotspots cells CP" 

unique(hotspots_perAC_HP$ccaa.shortname.en)
unique(hotspots_perAC_CP$ccaa.shortname.en)

resultados_Comun1  <- merge(resultados_Comun, hotspotspcellHP, by="ccaa.shortname.en", all.x=TRUE)
resultados_Comun2  <- merge(resultados_Comun1, hotspotspcellCP, by="ccaa.shortname.en", all.x=TRUE)

#% difference of %Perc_hots_HP and Perc_hots_CP
resultados_Comun2$change_perhots <- round((resultados_Comun2$`N hotspots cells CP`-resultados_Comun2$`N hotspots cells HP`)*100/resultados_Comun2$`N hotspots cells HP`, digits=1)

#Calculating percentage of AC cells that are hotspots
resultados_Comun2$PercAC_hots_HP <- round(resultados_Comun2$`N hotspots cells HP`*100/resultados_Comun2$n_cells, digits=1)
resultados_Comun2$PercAC_hots_CP <- round(resultados_Comun2$`N hotspots cells CP`*100/resultados_Comun2$n_cells, digits=1)

resultados_Comun2$geometry.x<-NULL
resultados_Comun2$geometry.y<-NULL
resultados_Comun2$geometry<-NULL
resultados_Comun2$n_cells<-NULL 

ACs_stats_30June <- merge(ACstatsHPCP, resultados_Comun2, by="ccaa.shortname.en")

write_xlsx(ACs_stats_30June, 'Data/ACs_stats_1july.xlsx') 
write_sf(hotspfinal_HP, "Spatial_Data/hotspfinal_HP.shp")
write_sf(hotspfinal_CP, "Spatial_Data/hotspfinal_CP.shp")


hotspfinal_HP
# comparisons between periods for each index

######Boxplots for each index in a single panel
# Function to calculate confidence interval
conf_int <- function(x) {
  ci <- qt(0.975, df=length(x)-1) * sd(x) / sqrt(length(x))
  return(mean(x) + c(-ci, ci))
}

# Function to add error bars
add_error_bars <- function(plot, data1, data2) {
  ci1 <- conf_int(data1)
  ci2 <- conf_int(data2)
  arrows(1, ci1[1], 1, ci1[2], length=0.05, angle=90, code=3, col="darkgrey")
  arrows(2, ci2[1], 2, ci2[2], length=0.05, angle=90, code=3, col="darkgrey")
}
 
plot1 <- boxplot(Indexes_HP_CP_complete1$rich_HP, Indexes_HP_CP_complete1$rich_CP, 
                 names = c("Historical", "Current"), 
                 main = "Species richness", ylab = "Average richness",
                 col = c("white", "gray93"))

mean07 <- mean(Indexes_HP_CP_complete1$rich_HP, na.rm = TRUE)
mean23 <- mean(Indexes_HP_CP_complete1$rich_CP, na.rm = TRUE)

#T test to check if differences are significant
t_test_rich <- t.test(Indexes_HP_CP_complete1$rich_CP, Indexes_HP_CP_complete1$rich_HP, paired = TRUE)

text(1, mean07, labels = round(mean07, 2), pos = 3, col = "black")
text(2, mean23, labels = round(mean23, 2), pos = 3, col = "black")

add_error_bars(plot1, Indexes_HP_CP_complete1$rich_HP, Indexes_HP_CP_complete1$rich_CP)

points(1, mean07, col = "red", pch = 18)
points(2, mean23, col = "red", pch = 18)

# Rarity
plot2 <- boxplot(Indexes_HP_CP_complete1$rarity_HP_sc, Indexes_HP_CP_complete1$rarity_CP_sc, 
                 names = c("Historical", "Current"), 
                 main = "Rarity", ylab = "Average rarity (scaled 0-1)",
                 col = c("white", "gray93"))
mean07 <- mean(Indexes_HP_CP_complete1$rarity_HP_sc)
mean23 <- mean(Indexes_HP_CP_complete1$rarity_CP_sc)
text(1, mean07, labels = round(mean07, 2), pos = 3, col = "black")
text(2, mean23, labels = round(mean23, 2), pos = 3, col = "black")
add_error_bars(plot2, Indexes_HP_CP_complete1$rarity_HP_sc, Indexes_HP_CP_complete1$rarity_CP_sc)
points(1, mean07, col = "red", pch = 18)
points(2, mean23, col = "red", pch = 18)

# Spanish threat status
plot3 <- boxplot(Indexes_HP_CP_complete1$RLS_2004_sc, Indexes_HP_CP_complete1$RLS_2021_sc, 
                 names = c("Historical", "Current"), 
                 main = "Spanish threat status", ylab = "Average score (scaled 0-1)",
                 col = c("white", "gray93"))
mean07 <- mean(Indexes_HP_CP_complete1$RLS_2004_sc)
mean23 <- mean(Indexes_HP_CP_complete1$RLS_2021_sc)

#T test to check if differences are significant
t_test_rarity <- t.test(Indexes_HP_CP_complete1$rarity_CP_sc, Indexes_HP_CP_complete1$rarity_HP_sc, paired = TRUE)


text(1, mean07, labels = round(mean07, 2), pos = 3, col = "black")
text(2, mean23, labels = round(mean23, 2), pos = 3, col = "black")
add_error_bars(plot3, Indexes_HP_CP_complete1$RLS_2004_sc, Indexes_HP_CP_complete1$RLS_2021_sc)
points(1, mean07, col = "red", pch = 18)
points(2, mean23, col = "red", pch = 18)

# European population status
plot4 <- boxplot(Indexes_HP_CP_complete1$EPS_2004_sc, Indexes_HP_CP_complete1$EPS_2022_sc, 
                 names = c("Historical", "Current"), 
                 main = "European population status", ylab = "Average score (scaled 0-1)",
                 col = c("white", "gray93"))
mean07 <- mean(Indexes_HP_CP_complete1$EPS_2004_sc)
mean23 <- mean(Indexes_HP_CP_complete1$EPS_2022_sc)

#T test to check if differences are significant
t_test_EPS <- t.test(Indexes_HP_CP_complete1$EPS_2022_sc, Indexes_HP_CP_complete1$EPS_2004_sc, paired = TRUE)

text(1, mean07, labels = round(mean07, 2), pos = 3, col = "black")
text(2, mean23, labels = round(mean23, 2), pos = 3, col = "black")
add_error_bars(plot4, Indexes_HP_CP_complete1$EPS_2004_sc, Indexes_HP_CP_complete1$EPS_2022_sc)
points(1, mean07, col = "red", pch = 18)
points(2, mean23, col = "red", pch = 18)

# SPEC status
plot5 <- boxplot(Indexes_HP_CP_complete1$SPEC_2004_sc, Indexes_HP_CP_complete1$SPEC_2022_sc, 
                 names = c("Historical", "Current"), 
                 main = "SPEC status", ylab = "Average score (scaled 0-1)",
                 col = c("white", "gray93"))
mean07 <- mean(Indexes_HP_CP_complete1$SPEC_2004_sc)
mean23 <- mean(Indexes_HP_CP_complete1$SPEC_2022_sc)

#T test to check if differences are significant
t_test_SPEC <- t.test(Indexes_HP_CP_complete1$SPEC_2022_sc, Indexes_HP_CP_complete1$SPEC_2004_sc, paired = TRUE)

text(1, mean07, labels = round(mean07, 2), pos = 3, col = "black")
text(2, mean23, labels = round(mean23, 2), pos = 3, col = "black")
add_error_bars(plot5, Indexes_HP_CP_complete1$SPEC_2004_sc, Indexes_HP_CP_complete1$SPEC_2022_sc)
points(1, mean07, col = "red", pch = 18)
points(2, mean23, col = "red", pch = 18)

# Combined index: First scaling 0-1, the other indexes are already scaled

basic_function <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
Indexes_HP_CP_complete1$cmbindx_HP <- basic_function(Indexes_HP_CP_complete1$cmbindx_HP)
Indexes_HP_CP_complete1$cmbindx_CP <- basic_function(Indexes_HP_CP_complete1$cmbindx_CP)

plot6 <- boxplot(Indexes_HP_CP_complete1$cmbindx_HP, Indexes_HP_CP_complete1$cmbindx_CP, 
                 names = c("Historical", "Current"), 
                 main = "r) Mean CIa for HP and CP", ylab = "Average score (scaled 0-1)",
                 col = c("white", "gray93"),
                 cex.main = 0.8) # Cambiar el tamaño del título
mean07 <- mean(Indexes_HP_CP_complete1$cmbindx_HP)
mean23 <- mean(Indexes_HP_CP_complete1$cmbindx_CP)
#T test to check if differences are significant
t_test_CIa <- t.test(Indexes_HP_CP_complete1$cmbindx_CP, Indexes_HP_CP_complete1$cmbindx_HP, paired = TRUE)


text(1, mean07, labels = round(mean07, 2), pos = 3, col = "black")
text(2, mean23, labels = round(mean23, 2), pos = 3, col = "black")
add_error_bars(plot6, Indexes_HP_CP_complete1$cmbindx_HP, Indexes_HP_CP_complete1$cmbindx_CP)
points(1, mean07, col = "red", pch = 18)
points(2, mean23, col = "red", pch = 18)

# Creating a layout to organize graphs in a single panel
layout(matrix(c(1, 4, 2, 5, 3, 6), nrow = 2, ncol = 3))

# Showing graphs in layout
par(mar = c(4, 4, 2, 2))
plot1
plot4
plot2
plot5
plot3
plot6

# Saving the image
dev.copy(pdf, "Figures/combined_plots_indexes_1December_scaled.pdf")
dev.off()

# Now plotting the frequencies of CI values
threshold_HP <- quantile(Indexes_HP_CP_complete1$cmbindx_HP, 0.95, na.rm = TRUE)
threshold_CP <- quantile(Indexes_HP_CP_complete1$cmbindx_CP, 0.95, na.rm = TRUE)

# Crear histograma para cmbindx_HP
p1 <- ggplot(Indexes_HP_CP_complete1, aes(x = cmbindx_HP)) +
  geom_histogram(bins = 30, fill = "gray", color = "black", alpha = 0.7) +
  geom_vline(xintercept = threshold_HP, color = "red", linetype = "dashed", linewidth = 1) +
  ggtitle("") +
  xlab("Area-based combined index (Historical period)") +
  ylab("Frequency") +
  theme_minimal()

# Crear histograma para cmbindx_CP
p2 <- ggplot(Indexes_HP_CP_complete1, aes(x = cmbindx_CP)) +
  geom_histogram(bins = 30, fill = "gray", color = "black", alpha = 0.7) +
  geom_vline(xintercept = threshold_CP, color = "red", linetype = "dashed", linewidth = 1) +
  ggtitle("") +
  xlab("Area-based combined index (Current period)") +
  ylab("Frequency") +
  theme_minimal()

# Mostrar los histogramas
print(p1)
print(p2)

mean(Indexes_HP_CP_complete1$rich_HP_sc, na.rm = TRUE)
mean(Indexes_HP_CP_complete1$rich_CP_sc, na.rm = TRUE)
sd(Indexes_HP_CP_complete1$rich_HP_sc, na.rm = TRUE)
sd(Indexes_HP_CP_complete1$rich_CP_sc, na.rm = TRUE)
sd(Indexes_HP_CP_complete1$rich_HP, na.rm = TRUE)
sd(Indexes_HP_CP_complete1$rich_CP, na.rm = TRUE)
sd(Indexes_HP_CP_complete1$rarity_HP_sc, na.rm = TRUE)
sd(Indexes_HP_CP_complete1$rarity_CP_sc, na.rm = TRUE)
sd(Indexes_HP_CP_complete1$RLS_2004_sc, na.rm = TRUE)
sd(Indexes_HP_CP_complete1$RLS_2021_sc, na.rm = TRUE)
sd(Indexes_HP_CP_complete1$EPS_2004_sc, na.rm = TRUE)
sd(Indexes_HP_CP_complete1$EPS_2022_sc, na.rm = TRUE)
sd(Indexes_HP_CP_complete1$SPEC_2004_sc, na.rm = TRUE)
sd(Indexes_HP_CP_complete1$SPEC_2022_sc, na.rm = TRUE)
sd(Indexes_HP_CP_complete1$cmbindx_HP, na.rm = TRUE)
sd(Indexes_HP_CP_complete1$cmbindx_CP, na.rm = TRUE)

# mapping the 5 indexes for the combined index 2004 vs 2022
# Selecting the indexes
combind1 <- Indexes_HP_CP_complete1[, c("rich_HP", "rich_CP","rarity_HP_sc", "rarity_CP_sc", "RLS_2004_sc", "RLS_2021_sc")]
combind2 <- Indexes_HP_CP_complete1[, c("EPS_2004_sc", "EPS_2022_sc", "SPEC_2004_sc","SPEC_2022_sc", "cmbindx_HP", "cmbindx_CP")]

# Variables en terciles
for (i in 1:6) {
  combind1[[paste0("tercios_", colnames(combind1)[i])]] <- cut(combind1[[i]], 
                                                               quantile(combind1[[i]], probs = c(0, 0.33, 0.67, 1), na.rm = TRUE), 
                                                               labels = c("Low", "Medium", "High"), include.lowest = TRUE)
}

# Maps titles
maptitles <- c("a) Richness 1998-2002 (Historical)", "b) Richness 2014-2022 (Current)", "d) Rarity 1998-2002 (Historical)", "e) Rarity 2014-2022 (Current)", "g) Spanish Threat Status 2004 (Historical)", "h) Spanish Threat Status 2021 (Current)")

# Maps with new categories
mapas <- list()
for (i in 1:6) {
  mapas[[i]] <- ggplot() +
    geom_sf(data = combind1, aes_string(fill = paste0("tercios_", colnames(combind1)[i])), color = "transparent") +
    geom_sf(data = comm, fill = "transparent", color = "black") + 
    scale_fill_manual(values = c("Low" = "thistle1", "Medium" = "plum2", "High" = "mediumorchid4"), na.value = "grey95") +
    labs(x = "", y = "", fill = colnames(combind1)[i]) +
    labs(title = maptitles[i]) +
    theme_bw() +
    theme(
      title = element_text(size = 12, face = "bold"),
      legend.title = element_text(size = 12, face = "bold"),
      legend.position = "none",
      axis.title = element_text(size = 15, face = "plain"),
      legend.key.size = unit(0.5, "cm")
    ) +
    guides(colour = "none", size = "none", alpha = "none")
}

# Final map
mapa_completo <- cowplot::plot_grid(plotlist = mapas, ncol = 2, align = "v", rel_heights = rep(1, length(mapas)))
mapa_completo

ggsave("Figures/indexes1_6_27Junesc.png", mapa_completo, wi = 34, he = 45, un = "cm", dpi = 300)

# The other 6 variables
for (i in 1:6) {
  combind2[[paste0("tercios_", colnames(combind2)[i])]] <- cut(combind2[[i]], 
                                                               quantile(combind2[[i]], probs = c(0, 0.33, 0.67, 1), na.rm = TRUE), 
                                                               labels = c("Low", "Medium", "High"), include.lowest = TRUE)
}

maptitles1 <- c("j) European threat status 2004 (Historical)", "k) European threat status 2022 (Current)", "m) SPEC 2004 (Historical)", "n) SPEC 2022 (Current)", "p) Combined index 1998-2002 (Historical)", "q) Combined index 2014-2022 (Current)")

# Maps with new categories
mapas1 <- list()
for (i in 1:6) {
  mapas1[[i]] <- ggplot() +
    geom_sf(data = combind2, aes_string(fill = paste0("tercios_", colnames(combind2)[i])), color = "transparent") +
    geom_sf(data = comm, fill = "transparent", color = "black", size=5.5) +
    scale_fill_manual(values = c("Low" = "thistle1", "Medium" = "plum2", "High" = "mediumorchid4"), na.value = "grey95") +
    labs(x = "", y = "", fill = colnames(combind2)[i]) +
    labs(title = maptitles1[i]) +
    theme_bw() +
    theme(
      title = element_text(size = 12, face = "bold"),
      legend.title = element_text(size = 12, face = "bold"),
      legend.position = "none",
      axis.title = element_text(size = 15, face = "plain"),
      legend.key.size = unit(0.5, "cm")
    ) +
    guides(colour = "none", size = "none", alpha = "none")
}

# Final map
mapa_completo1 <- cowplot::plot_grid(plotlist = mapas1, ncol = 2, align = "v", rel_heights = rep(1, length(mapas1)))
mapa_completo1

ggsave("Figures/indexes7_12_27Junesc.png", mapa_completo1, wi = 34, he = 45, un = "cm", dpi = 300)

maphotspots07 <- ggplot() +
  geom_sf(data = hotspfinal_HP, fill = "mediumorchid4") +
  geom_sf(data = comm, fill = "transparent", color = "black", size = 5.5) +
  labs(x = "", y = "", fill = "") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  theme_bw() +
  theme(
    title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.position = c(0.85, 0.15),
    axis.title = element_text(size = 15, face = "plain"),
    legend.key.size = unit(0.5, "cm")
  ) +
  guides(colour = "none", size = "none", alpha = "none") +
  labs(title = "Hotspots historical period")

maphotspots23 <- ggplot() +
  geom_sf(data = hotspfinal_CP, fill = "mediumorchid4") +
  geom_sf(data = comm, fill = "transparent", color = "black", size = 5.5) +
  labs(x = "", y = "", fill = "") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  theme_bw() +
  theme(
    title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.position = c(0.85, 0.15),
    axis.title = element_text(size = 15, face = "plain"),
    legend.key.size = unit(0.5, "cm")
  ) +
  guides(colour = "none", size = "none", alpha = "none") +
  labs(title = "Hotspots current period")

# Intersecting
overlap0 <- st_intersection(hotspfinal_HP, hotspfinal_CP)

# Filtering only polygon geometries
overlap0 <- overlap0[st_geometry_type(overlap0) %in% c("POLYGON", "MULTIPOLYGON"), ]

# Creating the map
Hotspots_HP_CP <- ggplot() +
  geom_sf(data = comm, fill = "white", color = "black") +
  geom_sf(data = hotspfinal_HP, aes(fill = "hotspots HP"), color = NA) +
  geom_sf(data = hotspfinal_CP, aes(fill = "hotspots CP"), color = NA) +
  geom_sf(data = overlap0, aes(fill = "hotspots HP + CP"), color = NA) +  # Muestra las áreas de superposición en rojo
  scale_fill_manual(values = c("hotspots HP" = "wheat", "hotspots CP" = "darkorange", "hotspots HP + CP" = "red")) +
  labs(x = "Longitude", y = "Latitude", fill = "") +
  labs(x = "Longitude", y = "Latitude", fill = "") +
  annotation_scale(location = "bl", width_hint = .3) +
  theme_bw() +
  theme(title = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.position = c(0.85, 0.25),
        axis.title = element_text(size = 12, face = "plain"), legend.key.size = unit(0.5, "cm")) +
  guides(colour = "none", size = "none", alpha = "none") +
  labs(title = "Historical and current hotspots")

ggsave("Figures/hotspotsHPCP_27Junesc.pdf", Hotspots_HP_CP, wi = 20, he = 25, un = "cm", dpi = 600)

hotspotsHP<-hotspfinal_HP
hotspotsCP<-hotspfinal_CP

hotspotsHP$geometry <- NULL
hotspotsCP$geometry <- NULL

oldhotsp <- anti_join(hotspotsHP, hotspotsCP, by= "UTMCODE")
newhotsp <- anti_join(hotspotsCP, hotspotsHP, by= "UTMCODE")

oldhotsp <- oldhotsp %>%
  distinct(UTMCODE, .keep_all = TRUE)

wtf <- rbind(hotspotsHP, hotspotsCP)

wtf <- wtf %>%
  distinct(UTMCODE, .keep_all = TRUE)

newhotsp <- anti_join(wtf, hotspotsHP, by= "UTMCODE")
oldhotsp <- anti_join(wtf, hotspotsCP, by= "UTMCODE")


#The intersection between hotspfinal_HP and hotspfinal_CP shows 104 cells that maintain as hotspots over time.

####################Coverage of PAs/SPAs over hotspots###################
############################################################################
overlapPA <- st_intersection(Protectedareas2004, SPAs2004)
intersectPAhot <- st_intersection(hotspfinal_HP, union_PAs2004)

# Creating the map
PASPAs2004 <- ggplot() +
  geom_sf(data = comm, fill = "white", color = "black") +
  geom_sf(data = hotspfinal_HP, aes(fill = "hotspots HP"), color = NA) +
  geom_sf(data = Protectedareas2004, aes(fill = "Parks"), color = NA) +
  geom_sf(data = SPAs2004, aes(fill = "SPAs"), color = NA) +
  geom_sf(data = intersectPAhot, aes(fill = "intersection"), color = NA) +
  geom_sf(data = overlapPA, aes(fill = "Parks + SPAs"), color = NA) +  # Muestra las áreas de superposición en rojo
  scale_fill_manual(values = c("hotspots HP" = "orange", "Parks" = "lightgreen", "SPAs" = "aquamarine", "intersection" = "blue", "Parks + SPAs" = "springgreen4"),
                    breaks = c("hotspots HP", "intersection", "Parks", "SPAs", "Parks + SPAs")) +
  labs(x = "Longitude", y = "Latitude", fill = "") +
  labs(x = "Longitude", y = "Latitude", fill = "") +
  annotation_scale(location = "bl", width_hint = .3) +
  theme_bw() +
  theme(title = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.position = c(0.85, 0.25),
        axis.title = element_text(size = 12, face = "plain"), legend.key.size = unit(0.5, "cm")) +
  guides(colour = "none", size = "none", alpha = "none") +
  labs(title = "Historical period: hotspots and Protected areas (PAs and SPAs)")

overlapPACP <- st_intersection(Protectedareas2022, SPAs2022)
intersectPAhotCP <- st_intersection(hotspfinal_CP, union_PAs2022)

SPAs2022 <- SPAs2022 %>%
  st_intersection(comm)
SPAs2022 <- st_union(SPAs2022) 

# Creating the map
PASPAs2022 <- ggplot() +
  geom_sf(data = comm, fill = "white", color = "black") +
  geom_sf(data = hotspfinal_CP, aes(fill = "hotspots CP"), color = NA) +
  geom_sf(data = Protectedareas2022, aes(fill = "Parks"), color = NA) +
  geom_sf(data = SPAs2022, aes(fill = "SPAs"), color = NA) +
  geom_sf(data = intersectPAhotCP, aes(fill = "intersection"), color = NA) +
  geom_sf(data = overlapPACP, aes(fill = "Parks + SPAs"), color = NA) +  # Muestra las áreas de superposición en rojo
  scale_fill_manual(values = c("hotspots CP" = "orange", "Parks" = "lightgreen", "SPAs" = "aquamarine", "intersection" = "blue", "Parks + SPAs" = "springgreen4"),
                    breaks = c("hotspots CP", "intersection", "Parks", "SPAs", "Parks + SPAs")) +
  labs(x = "Longitude", y = "Latitude", fill = "") +
  labs(x = "Longitude", y = "Latitude", fill = "") +
  annotation_scale(location = "bl", width_hint = .3) +
  theme_bw() +
  theme(title = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.position = c(0.85, 0.25),
        axis.title = element_text(size = 12, face = "plain"), legend.key.size = unit(0.5, "cm")) +
  guides(colour = "none", size = "none", alpha = "none") +
  labs(title = "Current period: hotspots and Protected areas (PAs and SPAs)")

ggsave("Figures/PASPAs2004_2Oct2024.png", PASPAs2004, wi = 20, he = 25, un = "cm", dpi = 600)
ggsave("Figures/PASPAs2022_2Oct2024.png", PASPAs2022, wi = 20, he = 25, un = "cm", dpi = 600)

