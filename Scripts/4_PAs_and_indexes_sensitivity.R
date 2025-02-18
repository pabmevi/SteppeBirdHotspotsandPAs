#This script is run without using ebird data. 

library(sf)
library(mapSpain)
library(dplyr)
library(lme4)
library(car)
library(effects)
library(emmeans)
library(MuMIn)
library(sjPlot)
library(ggeffects)
library(ggplot2)
library(gridExtra)
library(lmerTest)

# clean environment
rm(list = ls())

setwd("C:/Users/USUARIO/Documents/GitHub/SteppeBirdHotspotsandPAs")

#Load the combined file that includes all indexes for 5070 cells in 2006 and 2018
db3 <- st_read("Spatial_Data/HPCP_PASPA_27June_sensitivity.shp")

#Loading ACs
comm <-esp_get_ccaa()
comm <- comm[!comm$iso2.ccaa.name.es %in% c("Canarias"),]
comm <- st_transform(comm, st_crs(db3))

names (db3)[3] = "rich_HP_sc" #richness scaled HP
names (db3)[4] = "rarity_HP_sc" #rarity scaled HP
names (db3)[5] = "RLS07_sc" #Spanish threat status 2007
names (db3)[6] = "EPS07_sc" #European population status 2007
names (db3)[7] = "SPEC07_sc" #SPEC 2007
names (db3)[9] = "PA_SPAs07" #PA = Protected Area, SPA= Special Protected Area, PA_SPAs= PA + SPAs, no PA_SPAs= none PA or SPA
names (db3)[11] = "rich_CP_sc" ##richness scaled CP
names (db3)[12] = "rarity_CP_sc" #rarity scaled CP
names (db3)[13] = "RLS21_sc"
names (db3)[14] = "EPS23_sc"
names (db3)[15] = "SPEC23_sc"
names (db3)[17] = "PA_SPAs23" #PA = Protected Area, SPA= Special Protected Area, PA_SPAs= PA + SPAs, no PA_SPAs= none PA or SPA
names (db3)[18] = "ch_cmbn" # Change between HP combindex and CP combindex. This is a substraction, not a percentage of change.
names (db3)[25] = "pch_rich" #percentage of change for richness
names (db3)[26] = "pch_rarity" #percentage of change for rarity
names (db3)[28] = "pch_SPEC" #percentage of change for SPEC
names (db3)[30] = "arPASPA_07" #area PA + SPA 07
names (db3)[31] = "arPASPA_23" #area PA + SPA 23
names (db3)[32] = "diff_PA_SPAs" #area difference between arPASPA_07 and arPASPA_23
names (db3)[33] = "PA_SPA_g_cell" #gain/maintenance of PA_SPAs considering cells
names (db3)[34] = "PA_SPA_g_area" #gain/maintenance of PA_SPAs considering area

#Intersection between cellgrid and autonomous communities (AC)
intersection <- st_intersection(db3, comm)

intersection$area_interseccion <- st_area(intersection)

# Selecting the AC with the highest overlap for each cell, so we assign just one AC per cell
db3 <- intersection %>%
  group_by(UTMCODE) %>%
  slice(which.max(area_interseccion))

names (db3)[50] = "AC" #new name for ACs column

### 2 levels: protected or unprotected 

db3$PA<-ifelse(db3$PA_SPA_g_cell=="none", "unprotected", "protected")

db3 <- db3[,-c(35:49, 51:57)]

db3$rich_chabs <- db3$rich_CP - db3$rich_HP #this is the absolute change considering raw values
db3$rich_chscabs <- db3$rich_CP_sc - db3$rich_HP_sc #absolute richness change considering scaled values. This generates a better explained variance of the model

#ch_rich refers to the percentage change using scaled values of richness
################ANALYSING THE EFFECT OF RICHHNESS CHANGE##################

Mrichch <- lmer(rich_chabs ~ rich_HP + PA + PA*rich_HP + (1 | AC), data = db3)

Anova(Mrichch, type=3)
summary(Mrichch)
plot(Mrichch)
plot(allEffects(Mrichch))
emmeans(Mrichch, pairwise ~ PA)
r.squaredGLMM(Mrichch)

plot_model(Mrichch,type="pred",terms=c("rich_HP","PA"))

Rich <-ggpredict(Mrichch,terms=c("rich_HP","PA")) 

richplot <- ggplot()
richplot <- richplot + geom_ribbon(data=Rich,aes(x=x,ymin = conf.low, ymax =conf.high,group=group,fill=group),alpha = .4)
richplot <- richplot + geom_line(data=Rich,aes(x=x,y = predicted,group=group,colour=group),size=1.2)
richplot <- richplot + scale_colour_manual( values=c("#008B8B", "#EEAD0E"),labels=c("protected","unprotected"))
richplot <- richplot + scale_fill_manual( values=c("#008B8B", "#EEAD0E"),labels=c("protected","unprotected"))
richplot <- richplot + theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.richplotor = element_blank())
richplot <- richplot + theme(text=element_text(size=20))+theme(axis.line.x = element_line(color = 'black'))+theme(axis.line.y = element_line(color = 'black'))
richplot <- richplot + scale_y_continuous(limit=c(-6,4),breaks=c(-5,-4,-3,-2,-1,0,1,2,3,4))
richplot <- richplot + scale_x_continuous(limit = c(0,22),breaks = c(0,5,10,15,20))
richplot <- richplot + theme(axis.text=element_text(size=20),axis.title=element_text(size=22))
richplot <- richplot + labs(y="Richness change (raw values)",x="Historical Richness (raw values)")
richplot <- richplot + theme(legend.position = c(0.5,0.9), legend.background = element_rect(colour = "white" ),legend.title=element_blank())
richplot <- richplot + guides()

richplot

ggsave("Figures/richplotPA_4Julyraw.png", richplot, width = 9, height =9,dpi=300)   

################ANALYSING THE EFFECT OF RARITY CHANGE##################

db3$rarity_chsc <- db3$rarity_CP_sc - db3$rarity_HP_sc #absolute rarity change considering scaled values

Mrarch <- lmer(rarity_chsc ~ rarity_HP_sc + PA + PA*rarity_HP_sc + (1 | AC), data = db3)

avergararity <- aggregate(rarity_chsc ~ PA, data = db3, FUN = mean)

Anova(Mrarch, type=3)
summary(Mrarch)
plot(Mrarch)
plot(allEffects(Mrarch))
emmeans(Mrarch, pairwise ~ PA)
r.squaredGLMM(Mrarch)

plot_model(Mrarch,type="pred",terms=c("rarity_HP_sc","PA"))

Rarity <-ggpredict(Mrarch,terms=c("rarity_HP_sc","PA")) 

rarityplot <- ggplot()
rarityplot <- rarityplot + geom_ribbon(data=Rarity,aes(x=x,ymin = conf.low, ymax =conf.high,group=group,fill=group),alpha = .4)
rarityplot <- rarityplot + geom_line(data=Rarity,aes(x=x,y = predicted,group=group,colour=group),size=1.2)
rarityplot <- rarityplot + scale_colour_manual( values=c( "#008B8B", "#EEAD0E"),labels=c("protected","unprotected"))
rarityplot <- rarityplot + scale_fill_manual( values=c( "#008B8B", "#EEAD0E"),labels=c("protected","unprotected"))
rarityplot <- rarityplot + theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.rarityplotor = element_blank())
rarityplot <- rarityplot + theme(text=element_text(size=20))+theme(axis.line.x = element_line(color = 'black'))+theme(axis.line.y = element_line(color = 'black'))
rarityplot <- rarityplot + scale_y_continuous(limit=c(-0.5,0.1),breaks=c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1))
rarityplot <- rarityplot + scale_x_continuous(limit = c(0, 1),breaks = c(0,0.2,0.4,0.6,0.8,1))
rarityplot <- rarityplot + theme(axis.text=element_text(size=20),axis.title=element_text(size=22))
rarityplot <- rarityplot + labs(y="Rarity change",x="Historical rarity")
rarityplot <- rarityplot + theme(legend.position = c(0.5,0.9), legend.background = element_rect(colour = "white" ),legend.title=element_blank())
rarityplot <- rarityplot + guides(colour=FALSE, fill=FALSE)

rarityplot

################ANALYSING THE EFFECT OF RLS CHANGE##################
db3$RLS_ch_sc <- db3$RLS21_sc - db3$RLS07_sc #absolute richness change considering scaled values

MRLSch <- lmer(RLS_ch_sc ~ RLS07_sc + PA + PA*RLS07_sc + (1 | AC), data = db3)
avergRLS <- aggregate(RLS_ch_sc ~ PA, data = db3, FUN = mean)

Anova(MRLSch, type=3)
summary(MRLSch)
plot(MRLSch)
plot(allEffects(MRLSch))
emmeans(MRLSch, pairwise ~ PA)
r.squaredGLMM(MRLSch)

plot_model(MRLSch,type="pred",terms=c("RLS07_sc","PA"))

RLS <-ggpredict(MRLSch,terms=c("RLS07_sc","PA")) 

RLSplot <- ggplot()
RLSplot <- RLSplot + geom_ribbon(data=RLS,aes(x=x,ymin = conf.low, ymax =conf.high,group=group,fill=group),alpha = .4)
RLSplot <- RLSplot + geom_line(data=RLS,aes(x=x,y = predicted,group=group,colour=group),size=1.2)
RLSplot <- RLSplot + scale_colour_manual( values=c( "#008B8B", "#EEAD0E"),labels=c("protected","unprotected"))
RLSplot <- RLSplot + scale_fill_manual( values=c( "#008B8B", "#EEAD0E"),labels=c("protected","unprotected"))
RLSplot <- RLSplot + theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.RLSplotor = element_blank())
RLSplot <- RLSplot + theme(text=element_text(size=20))+theme(axis.line.x = element_line(color = 'black'))+theme(axis.line.y = element_line(color = 'black'))
RLSplot <- RLSplot + scale_y_continuous(limit=c(-0.4,0.3),breaks=c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3))
RLSplot <- RLSplot + scale_x_continuous(limit = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1))
RLSplot <- RLSplot + theme(axis.text=element_text(size=20),axis.title=element_text(size=22))
RLSplot <- RLSplot + labs(y="RLS change",x="Historical RLS")
RLSplot <- RLSplot + theme(legend.position = c(0.5,0.9), legend.background = element_rect(colour = "white" ),legend.title=element_blank())
RLSplot <- RLSplot + guides(colour=FALSE, fill=FALSE)

RLSplot

################ANALYSING THE EFFECT OF EPS CHANGE##################
db3$EPS_ch_sc <- db3$EPS23_sc - db3$EPS07_sc #absolute richness change considering scaled values

MEPSch <- lmer(EPS_ch_sc ~ EPS07_sc + PA + PA*EPS07_sc + (1 | AC), data = db3)
avergEPS <- aggregate(EPS_ch_sc ~ PA, data = db3, FUN = mean)

Anova(MEPSch, type=3)
summary(MEPSch)
plot(MEPSch)
plot(allEffects(MEPSch))
emmeans(MEPSch, pairwise ~ PA)
r.squaredGLMM(MEPSch)

plot_model(MEPSch,type="pred",terms=c("EPS07_sc","PA"))

EPS <-ggpredict(MEPSch,terms=c("EPS07_sc","PA")) 

EPSplot <- ggplot()
EPSplot <- EPSplot + geom_ribbon(data=EPS,aes(x=x,ymin = conf.low, ymax =conf.high,group=group,fill=group),alpha = .4)
EPSplot <- EPSplot + geom_line(data=EPS,aes(x=x,y = predicted,group=group,colour=group),size=1.2)
EPSplot <- EPSplot + scale_colour_manual( values=c( "#008B8B", "#EEAD0E"),labels=c("protected","unprotected"))
EPSplot <- EPSplot + scale_fill_manual( values=c( "#008B8B", "#EEAD0E"),labels=c("protected","unprotected"))
EPSplot <- EPSplot + theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.EPSplotor = element_blank())
EPSplot <- EPSplot + theme(text=element_text(size=20))+theme(axis.line.x = element_line(color = 'black'))+theme(axis.line.y = element_line(color = 'black'))
EPSplot <- EPSplot + scale_y_continuous(limit=c(-0.4,0.2),breaks=c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2))
EPSplot <- EPSplot + scale_x_continuous(limit = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1))
EPSplot <- EPSplot + theme(axis.text=element_text(size=20),axis.title=element_text(size=22))
EPSplot <- EPSplot + labs(y="EPS change",x="Historical EPS")
EPSplot <- EPSplot + theme(legend.position = c(0.5,0.9), legend.background = element_rect(colour = "white" ),legend.title=element_blank())
EPSplot <- EPSplot + guides(colour=FALSE, fill=FALSE)

EPSplot

################ANALYSING THE EFFECT OF SPEC CHANGE##################
db3$SPEC_ch_sc <- db3$SPEC23_sc - db3$SPEC07_sc #absolute richness change considering scaled values

MSPECch <- lmer(SPEC_ch_sc ~ SPEC07_sc + PA + PA*SPEC07_sc + (1 | AC), data = db3)
avergSPEC <- aggregate(SPEC_ch_sc ~ PA, data = db3, FUN = mean)

Anova(MSPECch, type=3)
summary(MSPECch)
plot(MSPECch)
plot(allEffects(MSPECch))
emmeans(MSPECch, pairwise ~ PA)
r.squaredGLMM(MSPECch)

plot_model(MSPECch,type="pred",terms=c("SPEC07_sc","PA"))

SPEC <-ggpredict(MSPECch,terms=c("SPEC07_sc","PA")) 

SPECplot <- ggplot()
SPECplot <- SPECplot + geom_ribbon(data=SPEC,aes(x=x,ymin = conf.low, ymax =conf.high,group=group,fill=group),alpha = .4)
SPECplot <- SPECplot + geom_line(data=SPEC,aes(x=x,y = predicted,group=group,colour=group),size=1.2)
SPECplot <- SPECplot + scale_colour_manual( values=c( "#008B8B", "#EEAD0E"),labels=c("protected","unprotected"))
SPECplot <- SPECplot + scale_fill_manual( values=c( "#008B8B", "#EEAD0E"),labels=c("protected","unprotected"))
SPECplot <- SPECplot + theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.SPECplotor = element_blank())
SPECplot <- SPECplot + theme(text=element_text(size=20))+theme(axis.line.x = element_line(color = 'black'))+theme(axis.line.y = element_line(color = 'black'))
SPECplot <- SPECplot + scale_y_continuous(limit=c(-0.4,0.2),breaks=c(-0.4,-0.3,-0.2,-0.1,0,0.1))
SPECplot <- SPECplot + scale_x_continuous(limit = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1))
SPECplot <- SPECplot + theme(axis.text=element_text(size=20),axis.title=element_text(size=22))
SPECplot <- SPECplot + labs(y="SPEC change",x="Historical SPEC")
SPECplot <- SPECplot + theme(legend.position = c(0.5,0.9), legend.background = element_rect(colour = "white" ),legend.title=element_blank())
SPECplot <- SPECplot + guides(colour=FALSE, fill=FALSE)

SPECplot

################ANALYSING THE EFFECT OF COMBINDEX CHANGE##################

Mcmbch <- lmer(ch_cmbn ~ cmbn_HP + PA + PA*cmbn_HP + (1 | AC), data = db3)

avergcmb <- aggregate(ch_cmbn ~ PA, data = db3, FUN = mean)

Anova(Mcmbch, type=3)
summary(Mcmbch)
plot(Mcmbch)
plot(allEffects(Mcmbch))
emmeans(Mcmbch, pairwise ~ PA)
r.squaredGLMM(Mcmbch)

plot_model(Mcmbch,type="pred",terms=c("cmbn_HP","PA"))

CMB <-ggpredict(Mcmbch,terms=c("cmbn_HP","PA")) 

CMBplot <- ggplot()
CMBplot <- CMBplot + geom_ribbon(data=CMB,aes(x=x,ymin = conf.low, ymax =conf.high,group=group,fill=group),alpha = .4)
CMBplot <- CMBplot + geom_line(data=CMB,aes(x=x,y = predicted,group=group,colour=group),size=1.2)
CMBplot <- CMBplot + scale_colour_manual( values=c( "#008B8B", "#EEAD0E"),labels=c("protected","unprotected"))
CMBplot <- CMBplot + scale_fill_manual( values=c( "#008B8B", "#EEAD0E"),labels=c("protected","unprotected"))
CMBplot <- CMBplot + theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.CMBplotor = element_blank())
CMBplot <- CMBplot + theme(text=element_text(size=20))+theme(axis.line.x = element_line(color = 'black'))+theme(axis.line.y = element_line(color = 'black'))
CMBplot <- CMBplot + scale_y_continuous(limit=c(-2,1),breaks=c(-4,-3,-2,-1,0,1,2,3,4))
CMBplot <- CMBplot + scale_x_continuous(limit = c(0,5),breaks = c(0,1,2,3,4,5))
CMBplot <- CMBplot + theme(axis.text=element_text(size=20),axis.title=element_text(size=22))
CMBplot <- CMBplot + labs(y="Combined index change",x="Historical CMB")
CMBplot <- CMBplot + theme(legend.position = c(0.5,0.9), legend.background = element_rect(colour = "white" ),legend.title=element_blank())
CMBplot <- CMBplot + guides()

CMBplot

allplots <- grid.arrange(rarityplot, RLSplot, EPSplot, SPECplot, CMBplot, ncol = 2)

ggsave("Figures/allplot_mixedmodelis_4July_sc.png", allplots, width = 20, height =20,dpi=300)   
