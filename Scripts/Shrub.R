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

######################################## Shrub Cover ###############################################################

### Load Data ###
shrub <- read.csv("Data/Master List of Plots - Shrub Cover.csv")

### Add in habitat column & ensure numeric/character values ###
shrub = mutate(shrub, habitat = str_extract(Plot..r...., "^.{2}")) 
shrub$Avg..Height = as.numeric(shrub$Avg..Height)
shrub$X..Cover = as.numeric(shrub$X..Cover)
shrub$habitat = as.character(shrub$habitat)
shrub$Plot = as.character(shrub$Plot)

### Remove NA and empty Values ###
shrub = filter(shrub, X..Cover != "NA") %>%
  filter(X..Cover != "")

### Reclassify coverage data (X..Cover) from 1-10 scale to percent scale ###
shrub <- mutate(shrub, X..Cover = case_when(
  grepl(1, X..Cover) ~ 0.1,
  grepl(2, X..Cover) ~ 0.5,
  grepl(3, X..Cover) ~ 1.5,
  grepl(4, X..Cover) ~ 3.5,
  grepl(5, X..Cover) ~ 7.5,
  grepl(6, X..Cover) ~ 17.5,
  grepl(7, X..Cover) ~ 37.5,
  grepl(8, X..Cover) ~ 62.5,
  grepl(9, X..Cover) ~ 85,
  grepl(10, X..Cover) ~ 97.5
))

shrub = na.omit(shrub)
  
### shrub Cover Plots ##
Shrub_Cover_Box = 
ggplot(shrub, aes(x = habitat, y = X..Cover, fill = habitat)) + 
  geom_boxplot() +
  geom_jitter(alpha = 0.6, position=position_jitter(0.2)) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  xlab("Habitat") +
  ylab("Shrub Cover %")
Shrub_Cover_Box
ggsave("Figures/Shrub_Cover_Box.png")


ggplot(shrub, aes(x = Species, y = X..Cover, color = habitat)) + 
  geom_boxplot() +
  geom_jitter(alpha = 0.6, position=position_jitter(0.1)) +
  theme_classic(base_size = 14) +
  guides(color=guide_legend(title="Habitat")) +
  xlab("Species") +
  ylab("Shrub Cover %")

ggplot(shrub, aes(x = habitat, y = X..Cover, fill = habitat)) + 
  stat_summary(geom = "bar", fun = mean, width = 0.6, color = "gray50") +
  geom_errorbar(stat = "summary", width = 0.5) +
  geom_point(stat = "summary") +
  geom_jitter(alpha = 0.6, position=position_jitter(0.2)) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  xlab("Habitat") +
  ylab("Shrub Cover %")

### shrub Height Plots ##
Shrub_Heights_Box =
ggplot(shrub, aes(x = habitat, y = Avg..Height, fill = habitat)) + 
  geom_boxplot() +
  geom_jitter(alpha = 0.6, position=position_jitter(0.2)) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  xlab("Habitat") +
  ylab("Average Shrub Height (m)")
Shrub_Heights_Box
ggsave("Figures/Shrub_Heights_Box.png")

ggplot(shrub, aes(x = Species, y = Avg..Height, color = habitat)) + 
  geom_boxplot() +
  geom_jitter(alpha = 0.6, position=position_jitter(0.1)) +
  theme_classic(base_size = 14) +
  guides(color=guide_legend(title="Habitat")) +
  xlab("Species") +
  ylab("Shrub Height (m)")

ggplot(shrub, aes(x = habitat, y = Avg..Height, fill = habitat)) + 
  stat_summary(geom = "bar", fun = mean, width = 0.6, color = "gray50") +
  geom_errorbar(stat = "summary", width = 0.5) +
  geom_point(stat = "summary") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  xlab("Habitat") +
  ylab("Average Shrub Height (m)")


