## Floral census data 2013 HC/SC analyses
## Anusha Shankar
## Started April 27, 2017
## For BBLH energy budget paper

## Reading in packages
library(ggplot2)
library(reshape)

## Set working directory
setwd("C:\\Users\\shankar\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget\\Tables")

## Reda in file
floral <- read.csv("FloralCensus_cropped.csv", sep=";")
head(floral)

## Saving standard theme  
my_theme <- theme_classic(base_size = 30) + 
  theme(axis.title.y = element_text(color = "black", vjust = 2),
        panel.border = element_rect(colour = "black", fill=NA))

## Theme with slightly smaller font
my_theme2 <- my_theme + theme_classic(base_size = 15)

## Give sample size
give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

## plot of flowers per site and month
ggplot(floral, aes(Site,(TotalFlowers/length(unique(Date))))) + geom_bar(stat="identity", width=0.5) + 
  my_theme + facet_grid(.~Pre_post_monsoon)# + geom_text(aes(label = length(unique(Date))), stat= "count", vjust = -.5)
  
  geom_text(aes(length(unique(floral$Date)), vjust=4, size=10))

