## Plotting Schuchmann 1983 data to look at min Tbs for a number of species
library(ggplot2)

schuchmann_data <- 
  read.csv("C:\\Users\\ANUSHA\\Dropbox\\Hummingbird energetics\\Schuchmann1983_data_digitized_savings.csv")

my_theme <- theme_classic(base_size = 32) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

ggplot(schuchmann_data, aes(Ta, MRtor_as_percentage_of_activityMR)) + my_theme +
  geom_line(aes(group=Indiv_ID, col=Species)) + geom_point(aes(col = Species, size=Mass), alpha=0.6)

