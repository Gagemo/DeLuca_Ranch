################################################################################
################################################################################
################### DeLuca -  Understory  ######################################
################### By: Gage LaPierre     ######################################
################################################################################
################################################################################

################### Installs Packages if Needed ################################

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

################### DeLuca Understory ##########################################

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
  
# Create Species Pivot Table #

Spp = dplyr::select(data, Plot..1mx1m., Species, X..Cover) %>% matrify() 
Spp[] <- lapply(Spp, as.numeric)
Spp = na.omit(Spp)

Spp %>% select_if(colSums(.) != 0)

# Create Grouped Treatment/ Environment Table and Summaries to fit Species Table #
Treat <- data.frame("names"=rownames(Spp), Spp)

Treat = mutate(Treat, habitat = str_extract(names, "^.{2}")) %>%
  group_by(names, habitat) %>% summarise()

# Drop first column in spp data for vegdist #
Spp <- subset(Spp, select = -1 )

# Run Bray-Curtis Dissimilarity on Pre & Post Treatment Data #

vegdist_ = vegdist(Spp, method = "bray")

# Use dissimilarities to create scree plot - attain the number of dimensions for NMDS with least stress #
# Using function that produces a stress vs. dimensional plot #

NMDS.scree <- function(x) { # x is the name of the data frame variable
  plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress), 
       xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions", ylab = "Stress", 
       main = "NMDS Stress Plot")
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x, autotransform = F, k = i + 1)$stress))
  }
}

NMDS.scree(vegdist_)

# - Based on scree plot two dimensions will be sufficient for NMDS #

# Run MDS and plot stress using a Shepherd Plot #

MDS = metaMDS(vegdist_, distance = "bray", trymax = 500, maxit = 999, k=3, 
              trace = F, autotransform = FALSE, wascores = TRUE)
MDS$stress
stressplot(MDS) 
goodness(MDS)

# Shepherd plots showcase a not perfect, but acceptable R^2 value #

spp_scrs <- 
  sppscores(MDS) <- Spp

species.scores <- as.data.frame(scores(MDS, display="species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

# Turn MDS points into a dataframe with treatment data for use in ggplot #

NMDS = data.frame(MDS = MDS$points, Treat = Treat$habitat, 
                  Plot = Treat$names)

# NMDS Graphs

# Treatment #

ThePlot = 
  ggplot() +
  geom_point(data = NMDS, aes(x = MDS.MDS1, y = MDS.MDS2, color = Treat)) +
  stat_ellipse(data = NMDS, aes(x = MDS.MDS1, y = MDS.MDS2, color = Treat), linetype = "dashed", show.legend = T) +
  theme_bw() +
  labs(x="MDS1", y="MDS2", title = "", color = "Habitat") +
  theme(plot.title = element_text(hjust = 0.5)) 
ThePlot

ggsave("Figures\Understory_NMDS.png", width = 10, height = 7)

#################### Bare ground, CWD, Leaf Material Removed ################################

sp_data = filter(data, !grepl('Bare ground', Species)) %>%
  filter(!grepl('CWD', Species)) %>%
  filter(!grepl('Leaf material', Species))

# Create Species Pivot Table #

Spp = dplyr::select(sp_data, Plot..1mx1m., Species, X..Cover) %>% matrify() 
Spp[] <- lapply(Spp, as.numeric)
Spp = na.omit(Spp)

Spp = Spp %>% select_if(colSums(.) != 0)

# Create Grouped Treatment/ Environment Table and Summaries to fit Species Table #
Treat <- data.frame("names"=rownames(Spp), Spp)

Treat = mutate(Treat, habitat = str_extract(names, "^.{2}")) %>%
  group_by(names, habitat) %>% summarise()

# Drop first column in spp data for vegdist #
Spp <- subset(Spp, select = -1 )

# Run Bray-Curtis Dissimilarity on Pre & Post Treatment Data #

vegdist_ = vegdist(Spp, method = "bray")

# Use dissimilarities to create scree plot - attain the number of dimensions for NMDS with least stress #
# Using function that produces a stress vs. dimensional plot #

NMDS.scree <- function(x) { # x is the name of the data frame variable
  plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress), 
       xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions", ylab = "Stress", 
       main = "NMDS Stress Plot")
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x, autotransform = F, k = i + 1)$stress))
  }
}

NMDS.scree(vegdist_)

# - Based on scree plot two dimensions will be sufficient for NMDS #

# Run MDS and plot stress using a Shepherd Plot #

MDS = metaMDS(vegdist_, distance = "bray", trymax = 500, maxit = 999, k=3, 
              trace = F, autotransform = FALSE, wascores = TRUE)
MDS$stress
stressplot(MDS) 
goodness(MDS)

# Shepherd plots showcase a not perfect, but acceptable R^2 value #

spp_scrs <- 
  sppscores(MDS) <- Spp

species.scores <- as.data.frame(scores(MDS, display="species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

# Turn MDS points into a dataframe with treatment data for use in ggplot #

NMDS = data.frame(MDS = MDS$points, Treat = Treat$habitat, 
                  Plot = Treat$names)

# NMDS Graph without physical variables  #

ThePlot = 
  ggplot() +
  geom_point(data = NMDS, aes(x = MDS.MDS1, y = MDS.MDS2, color = Treat)) +
  stat_ellipse(data = NMDS, aes(x = MDS.MDS1, y = MDS.MDS2, color = Treat), 
               linetype = "dashed", show.legend = T) +
  theme_bw() +
  labs(x="MDS1", y="MDS2", title = "", color = "Habitat") +
  theme(plot.title = element_text(hjust = 0.5)) 
ThePlot

ggsave("ThePlot2.png", width = 10, height = 7)






