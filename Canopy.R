############################ Installs Packages if Needed #####################################################

list.of.packages <- c("ggplot2", "tidyverse", "agricolae", "labelled", "vegan", "labdsv")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

############################ Loads Packages  #####################################################

library(ggplot2)
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

################### Canopy Cover ###############################################################

### Load Data ###
canopy <- read.csv("")

### Add in habitat column ###
canopy = mutate(canopy, habitat = str_extract(Plot..r...., "^.{2}")) 
canopy$ = as.numeric(canopy$DBH..cm.)
canopy$habitat = as.character(canopy$habitat)
canopy$Height..m. = as.numeric(canopy$Height..m.)

### Average DBH ##
ggplot(canopy, aes(x = habitat, y = DBH..cm., fill = habitat)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.6, position=position_jitter(0.2)) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  xlab("Habitat") +
  ylab("DBH")

### Height ##
ggplot(canopy, aes(x = habitat, y = Height..m., fill = habitat)) + 
  geom_boxplot() + 
  geom_jitter(alpha = 0.6, position=position_jitter(0.2)) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  xlab("Habitat") +
  ylab("Height")


