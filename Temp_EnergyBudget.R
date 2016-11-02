## Analyzing and plotting ambient and chamber temperature data for BBLH energy budget paper
## To make a thermoregulatory model for HC and SC
## Script by Anusha Shankar
## Script started on: November 2, 2016

library(reshape)
library(ggplot2)

#### Reading in files and reshaping ####
## Set wd and read in file
setwd("C:\\Users\\ANUSHA\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget")

temp_details <- read.csv("BBLH_temperatures_compiled.csv")

