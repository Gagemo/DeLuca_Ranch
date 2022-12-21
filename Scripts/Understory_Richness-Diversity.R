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

################### DeLuca Understory Species Richness/Diversity ###############################################################

### Load Data ###
A_120 <- read.csv("Data/Master List of Plots - 120 A.csv")
B_120 <- read.csv("Data/Master List of Plots - 120 B.csv")
A_240 <- read.csv("Data/Master List of Plots - 240 A.csv")
B_240 <- read.csv("Data/Master List of Plots - 240 B.csv")
A_360 <- read.csv("Data/Master List of Plots - 360A.csv")
B_360 <- read.csv("Data/Master List of Plots - 360 B.csv")

### Makes Each Data Set Unique so we can combine  ###
A_120$Plot..1mx1m. <- paste0(A_120$Plot..1mx1m, ".120A")
B_120$Plot..1mx1m. <- paste0(B_120$Plot..1mx1m, ".120B")
A_240$Plot..1mx1m. <- paste0(A_240$Plot..1mx1m, ".240A")
B_240$Plot..1mx1m. <- paste0(B_240$Plot..1mx1m, ".240B")
A_360$Plot..1mx1m. <- paste0(A_360$Plot..1mx1m, ".360A")
B_360$Plot..1mx1m. <- paste0(B_360$Plot..1mx1m, ".360B")

### Combine All Data Sets ###
data = rbind(A_120, B_120, A_240, B_240, A_360, B_360)

### Add in habitat column ###
data = mutate(data, habitat = str_extract(Plot..1mx1m., "^.{2}"))

### Add in plot column ###
data = mutate(data, plot = str_extract(Plot..1mx1m., "^.{3}"))

### Remove NA and empty Values ###
data = filter(data, X..Cover != "NA") %>%
  filter(X..Cover != "")

### Remove Physical Variables ###
sp_data = filter(data, !grepl('Bare ground', Species)) %>%
  filter(!grepl('CWD', Species)) %>%
  filter(!grepl('Leaf material', Species))

# Create Species Pivot Table #
Spp = dplyr::select(sp_data, Plot..1mx1m., Species, X..Cover) %>% matrify() 
Spp[] <- lapply(Spp, as.numeric)

# Drop first column in spp data for vegdist #
Spp <- subset(Spp, select = -1 )

# Create Grouped Treatment/ Environment Table and Summaries to fit Species Table #
Treat <- data.frame("names"=rownames(Spp), Spp)

Treat = mutate(Treat, habitat = str_extract(names, "^.{2}")) %>%
  group_by(names, habitat) %>% 
  summarise()
Treat = mutate(Treat, plot = str_extract(names, "^.{3}"))

############################### Species Richness ##############################################
table_SR <- table(sp_data$Species, sp_data$Plot..1mx1m.)
table_SR 

SR = specnumber(table_SR , MARGIN=2)
SR = as.data.frame(SR)

## Merge species richness with habitat/plot data for ggplot ##
SR_treat = cbind(Treat, SR) 

## Species Richness Boxplot ##
SR_Box = 
  ggplot(SR_treat) +
  geom_boxplot(aes(x = habitat, y = SR, color  = habitat)) +
  geom_jitter(aes(x = habitat, y = SR),
              color="black", size=0.4, alpha=0.9) +
  labs(x="", y = "Species Richness") +
  theme_classic() +
  theme(legend.position = "none")
SR_Box
ggsave("Figures/SR_Box.png")

############################### Diversity ################################################
simpsons = diversity(Spp, index = "simpson")
simpsons = as.data.frame(simpsons)

shannon = diversity(Spp, index = "shannon")
shannon = as.data.frame(shannon)

inv_shan = diversity(Spp, index = "inv")
inv_shan = as.data.frame(inv_shan)

## Merge with diversity data for ggplot ##
simp_treat = cbind(Treat, simpsons) 
shn_treat = cbind(Treat, shannon) 
in_shn_treat = cbind(Treat, inv_shan) 

## Simpsons Boxplot ##
Simp_Box = 
ggplot(simp_treat) +
  geom_boxplot(aes(x = habitat, y = simpsons, color  = habitat)) +
  geom_jitter(aes(x = habitat, y = simpsons),
              color="black", size=0.4, alpha=0.9) +
  labs(x="", y = "Shannon's Diversity Index") +
  ylim(0, 1.25) +
  theme_classic() +
  theme(legend.position = "none")
Simp_Box
ggsave("Figures/Simp_Box.png")

## Shannons Boxplot ##
Shn_Box =
ggplot(shn_treat) +
  geom_boxplot(aes(x = habitat, y = shannon, color  = habitat)) +
  geom_jitter(aes(x = habitat, y = shannon),
              color="black", size=0.4, alpha=0.9) +
  labs(x="", y = "Shannons Diversity Index") +
  theme_classic() +
  theme(legend.position = "none")
Shn_Box
ggsave("Figures/Shn_Box.png")


## Inv-Simp Boxplot ##
Inv_simp_Box = 
ggplot(in_shn_treat) +
  geom_boxplot(aes(x = habitat, y = inv_shan, color  = habitat)) +
  geom_jitter(aes(x = habitat, y = inv_shan),
              color="black", size=0.4, alpha=0.9) +
  labs(x="", y = "Inverse Simpson's Diversity Index") +
  theme_classic() +
  theme(legend.position = "none")
Inv_simp_Box
ggsave("Figures/Inv_simp_Box.png")
