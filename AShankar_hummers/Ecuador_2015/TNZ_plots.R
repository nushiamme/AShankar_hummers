## TNZ trial plot
## By Anita Morales and Anusha Shankar
## August 7, 2016

library(ggplot2)

## Read in the csv file
tnz_trial <- read.csv("C://Users//ANUSHA//Desktop//TNZ_shortlist.csv")
tnz_trial2 <- read.csv("C://Users//ANUSHA//Desktop//TNZ_shortlist_2015.csv")

tnz_hevi <- tnz_trial2[tnz_trial2$Species=="HEVI",]

tnz_hevi <- tnz_hevi[tnz_hevi$EE_J >= 2,]

## Make a simple theme to make plots pretty for poster/publication
my_theme <- theme_classic(base_size = 30) + 
  theme(axis.title.y = element_text(vjust = 2),
        panel.border = element_rect(colour = "black", fill=NA))

## Make the plot! This one has points, colored by individual, and lines connecting all the points for an individual
## To make it easy, each line has one category of functions. First, the base plot. Next, the aesthetic stuff. 
## Next, the layers- points, lines, etc.
ggplot(tnz_hevi, aes(Temp_step, EE_J)) + 
  my_theme + 
  geom_point(size=2) #(aes(color=BirdID), size=3) + geom_line(aes(group=BirdID)) # color by individual; draw lines connecting indivs
