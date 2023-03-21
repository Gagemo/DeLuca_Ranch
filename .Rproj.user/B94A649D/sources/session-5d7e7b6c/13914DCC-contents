################################################################################
################################################################################
################### DeLuca -  Overstory  #######################################
################### By: Gage LaPierre    #######################################
################################################################################
################################################################################

############################ Installs Packages if Needed #######################

list.of.packages <- c("tidyverse", "agricolae", "labelled", "vegan", "labdsv")
new.packages <- list.of.packages[!(list.of.packages 
                                   %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

############################ Loads Packages  ###################################

library(tidyverse)
library(vegan)
library(agricolae)
library(labelled)
library(labdsv)

# Clears environment
rm(list=ls(all=TRUE))

# Clears history
cat("\014") 

set.seed(2)

################### DeLuca Overstory ###########################################

### Load Data ###
tree <- read.csv("Data/Master List of Plots - Canopy Cover.csv")

### Add in habitat column ###
tree = mutate(tree, habitat = str_extract(Plot..r...., "^.{2}")) 
tree$DBH..cm. = as.numeric(tree$DBH..cm.)
tree$habitat = as.character(tree$habitat)
tree$Height..m. = as.numeric(tree$Height..m.)
tree$Species = as.character(tree$Species)

################# DBH ##########################################################
DBH_Box = 
ggplot(tree %>% dplyr::filter(DBH..cm. != "NA"), 
       aes(x = habitat, y = DBH..cm., fill = habitat)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(alpha = 0.4, position=position_jitter(0.2)) +
  scale_fill_manual(values=c("#CC9900", "#660066", "#CC0000",
                                      "#FF66FF"))+
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  xlab("Habitat") +
  ylab("DBH (cm)")
DBH_Box
ggsave("Figures/DBH_Box.png")

MF_DBH_Box = 
ggplot(tree %>% dplyr::filter(Species != "NA") %>% subset(habitat=='MF'),
                       aes(x = Species, y = DBH..cm., fill = Species)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(alpha = 0.4, position=position_jitter(0.2)) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  xlab("Species") +
  ylab("Average DBH (cm)")
MF_DBH_Box
ggsave("Figures/MF_DBH_Box.png")

######################################## Height ################################
Height_Box =
ggplot(tree %>% dplyr::filter(DBH..cm. != "NA"), 
       aes(x = habitat, y = Height..m., fill = habitat)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.6, position=position_jitter(0.2)) +
  scale_fill_manual(values=c("#CC9900", "#660066", "#CC0000",
                                      "#FF66FF"))+
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  xlab("Habitat") +
  ylab("Height (m)")
Height_Box
ggsave("Figures/Height_Box.png")

MF_Height =
ggplot(tree %>% dplyr::filter(Species != "NA") %>% subset(habitat=='MF'),
       aes(x = Species, y = Height..m., fill = Species)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(alpha = 0.4, position=position_jitter(0.2)) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  xlab("Species") +
  ylab("Average Height (m)")
MF_Height
ggsave("Figures/Height_Box.png")

#################### Basal Area ################################################

basal.area.fn <- function(x){ (pi*((x)/2)^2) } # calculate basal area in m^2
tree$BA = basal.area.fn(tree$DBH..cm.)

Basal_Box =
ggplot(tree %>% dplyr::filter(BA != "NA"), 
       aes(x = habitat, y = BA, fill = habitat)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.6, position=position_jitter(0.2)) +
  scale_fill_manual(values=c("#CC9900", "#660066", "#CC0000",
                                      "#FF66FF"))+
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  xlab("Habitat") +
  ylab("Basal Area "~cm^2)
Basal_Box
ggsave("Figures/Basal_Box.png")

###################### Trees Per Acre  #########################################

tpha = filter(tree, DBH..cm. != "NA")
tpha = as.data.frame(table(tpha$Plot..r....))
# Expansion factor = 21.12 (1 hectare divide by the plots size 0.0452 hectares )
tpha$tpha = ((tpha$Freq)*21.12)

### Add in habitat column ###
tpha = mutate(tpha, habitat = str_extract(Var1, "^.{2}")) 

TPHA_Box =
ggplot(tpha, aes(x = habitat, y = tpha, fill = habitat)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.6, position=position_jitter(0.2)) +
  scale_fill_manual(values=c("#CC9900", "#660066", "#CC0000",
                                      "#FF66FF"))+
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  xlab("Habitat") +
  ylab("Trees per Hectare")
TPHA_Box
ggsave("Figures/TPHA_Box.png")
