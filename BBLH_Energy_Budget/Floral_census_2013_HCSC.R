## Floral census data 2013 HC/SC analyses
## Anusha Shankar
## Started April 27, 2017
## For BBLH energy budget paper

## Reading in packages
require(ggplot2)
require(reshape)
require(plyr)

## Set working directory
setwd("C:\\Users\\ANUSHA\\Dropbox\\Anusha Committee\\BBLH_EnergyBudget\\Tables")

## Read in file
floral <- read.csv("FloralCensus_cropped.csv", sep=";")
floral$Site_monsoon <- paste(floral$Site,floral$Pre_post_monsoon, sep = "_")
floral$Pre_post_monsoon <- factor(floral$Pre_post_monsoon, levels = c("Pre", "Post"))
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
flor_sum <- ddply(floral,~Site_monsoon,summarise,
                  scaled_flowers=sum(TotalFlowers)/(length(unique(Date))))
flor_sum$Site_monsoon <- factor(flor_sum$Site_monsoon, 
                                   levels = c("HC_Pre", "PL/SC_Pre", "HC_Post", "PL/SC_Post"))

ggplot(flor_sum, aes(Site_monsoon, scaled_flowers)) + 
  geom_bar(stat = "identity", width=0.5) + my_theme

ggplot(floral, aes(Site,(TotalFlowers))) + 
  geom_bar(stat="identity", width=0.5) + 
  my_theme + facet_grid(.~Pre_post_monsoon)

