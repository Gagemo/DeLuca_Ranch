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

######################################## Canopy Cover ###############################################################

### Load Data ###
canopy <- read.csv("Data/Master List of Plots - % Canopy Coverage by Plot.csv")

### Add in habitat column & ensure numeric/character values ###
canopy = mutate(canopy, habitat = str_extract(Plot, "^.{2}")) 
canopy$Bin.Number = as.numeric(canopy$Bin.Number)
canopy$habitat = as.character(canopy$habitat)
canopy$Plot = as.character(canopy$Plot)

### Remove NA and empty Values ###
canopy = filter(canopy, Bin.Number != "NA") %>%
  filter(Bin.Number != "")

### Reclassify coverage data (Bin.Number) from 1-10 scale to percent scale ###
canopy <- mutate(canopy, Bin.Number = case_when(
  grepl(1, Bin.Number) ~ 0.1,
  grepl(2, Bin.Number) ~ 0.5,
  grepl(3, Bin.Number) ~ 1.5,
  grepl(4, Bin.Number) ~ 3.5,
  grepl(5, Bin.Number) ~ 7.5,
  grepl(6, Bin.Number) ~ 17.5,
  grepl(7, Bin.Number) ~ 37.5,
  grepl(8, Bin.Number) ~ 62.5,
  grepl(9, Bin.Number) ~ 85,
  grepl(10, Bin.Number) ~ 97.5
))

### Canopy Cover Box Plot ##
canopy_Box =
ggplot(canopy, aes(x = habitat, y = Bin.Number, fill = habitat)) + 
  geom_boxplot() +
  geom_jitter(alpha = 0.6, position=position_jitter(0.2)) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  xlab("Habitat") +
  ylab("Canopy Cover")
canopy_Box
ggsave("Figures/canopy_Box.png")



