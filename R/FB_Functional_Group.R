rm(list=ls()) #Starting fresh

##########################
#Install and load package
##########################
remotes::install_github("ropensci/rfishbase")

library("rfishbase")
ls("package:rfishbase") #you can check all the functions available within the rfishbase package:
library("rstudioapi")       
library("dplyr")
library ("forcats")
######################
#set your working directory
######################
setwd(dirname(getActiveDocumentContext()$path))

#################
#load the data
#################
library(readxl)
dat <- read_excel("MEDigital_species_v2.xlsx") 

#rename columns and delete unnecessary ones
list_fish <- dat[, "Species"]

list_fish = list_fish$Species #Convert from db to list

#Get food items (can have multiple rows per species)
fish_fooditems <- fooditems(list_fish, fields = c("Species", "FoodI"))
#levels(as.factor(fish_ecology$FoodI))
                        
# Get ecology data (one row per species)
fish_ecology <- ecology(list_fish, fields = c("Species", "FeedingType", "FoodTroph"))
#levels(as.factor(fish_ecology$FeedingType))
                        
# Merge the two datasets: replicate ecology data to match fooditems
combined_data <- fish_fooditems %>%
left_join(fish_ecology, by = "Species")
