## Analyzing and plotting ambient and chamber temperature data for torpor paper
## Anusha Shankar*, Rebecca Schroeder*, Catherine Graham, Don Powers
## Script started on: Sept 24, 2016

require(ggplot2)
require(reshape)

## Set wd and read in files
setwd("C://Users//ANUSHA//Dropbox//Hummingbird energetics//Tables_for_paper/")

## Read in files
tatc <- read.csv("TempSummary_AllSites.csv")

## General functions
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

pd <- position_dodge(0.1) # move them .05 to the left and right

## Plots
ggplot(tatc, aes(Hour,Tc_Mean)) + my_theme + 
  geom_point(aes(col=Site)) + geom_line(aes(col=Site, group=Site)) +
  geom_errorbar(aes(ymin=Tc_min, ymax=Tc_max), width=.1, position=pd) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(tatc, aes(Hour,Ta_Mean)) + my_theme + 
  geom_point(aes(col=Site)) + geom_line(aes(col=Site, group=Site)) +
  geom_errorbar(aes(ymin=Ta_min, ymax=Ta_max), width=.1, position=pd) +
  theme(axis.text.x = element_text(angle = 90))
