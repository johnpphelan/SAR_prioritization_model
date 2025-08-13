library(tidyverse)
library(sf)
library(dplyr)

# laod data frames
maxmodel<-read.xlsx("output/SARA_prioritization_model_2025-06-19.xlsx")
summedmodel<-read.xlsx("output/Summed_SARA_prioritization_model_2025-06-19.xlsx")


maxmodel <- maxmodel[, names(maxmodel) != ""]
summedmodel<- summedmodel[, names(summedmodel) != ""]


