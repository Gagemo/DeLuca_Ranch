################################################################################
################################################################################
################### DeLuca Understory - Leaf Cover  ############################
################################################################################
################################################################################

###################### Installs Packages if Needed #############################

list.of.packages <- c("ggplot2", "tidyverse", "agricolae", 
                      "labelled", "vegan", "labdsv")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

############################ Loads Packages  ###################################

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

### Reclassify coverage data (X..Cover) from 1-10 scale to percent scale ###
data <- mutate(data, X..Cover = case_when(
  grepl(0, X..Cover) ~ 0,
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

data = filter(data, Species == "Leaf material" )

# Replaces NA's with Zeros #
data = data %>% replace(is.na(.), 0)

### shrub Cover Plots ##
data_LM_Box = 
  ggplot(data, aes(x = habitat, y = X..Cover, fill = habitat)) + 
  geom_boxplot() +
  geom_jitter(alpha = 0.6, position=position_jitter(0.2)) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") +
  xlab("Habitat") +
  ylab("Leaf Material Cover %")
data_LM_Box
ggsave("Figures/data_BG_Box.png")
