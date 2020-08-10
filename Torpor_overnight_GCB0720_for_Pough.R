## Code for paper titled:
#"Hummingbird torpor in context: duration, more than temperature, 
# is key to nighttime energy savings"
## Paper authors: Anusha Shankar*, Rebecca J Schroeder*, 
# Susan M Wethington, Catherine H Graham, Donald R Powers
## *Equal authors
## Code by: Anusha Shankar, github/nushiamme
## Contact: nushiamme<at>gmail<dot>com for questions about code
# This script contains code for torpor graph over the course of the night for a green-crowned brilliant
# individual


## Read in required packages
library(ggplot2)

## Read in file
setwd("C:\\Users\\nushi\\Dropbox\\Hummingbird energetics\\July2018\\Data")
gcb_0720 <- read.csv(".//E14_0720_GCB_no_bsln_Rewarm.csv")


## Theme for Fig A2 
my_theme_blank <- theme_classic(base_size = 30) + 
  theme(axis.title.y = element_text(vjust = 2),
        panel.border = element_blank())


## Assign colors and name categories appropriately
gcb_0720$Category <- factor(gcb_0720$Category, levels=c("B", "N", "R", "T"), 
                            labels=c("B", "Normothermy", "Rewarming", "Torpor"))
torCol <- c("white", "black", "red", "purple")
names(torCol) <- levels(gcb_0720$Category)
colScale <- scale_colour_manual(name = "Category", values = torCol)


## Plot
gcb_0720$EE_per_second_J <- gcb_0720$EE_J/60
ggplot(NULL, aes(x=SampleNo, y=EE_per_second_J, col=Category)) +
  geom_path(data=gcb_0720[gcb_0720$SampleNo<20000 & gcb_0720$Category=="Normothermy",], size=1.25) +
  geom_path(data=gcb_0720[gcb_0720$SampleNo>30000 & gcb_0720$Category=="Normothermy",], size=1.25) +
  geom_path(data=gcb_0720[gcb_0720$Category=="Torpor",], size=1.25) +
  geom_path(data=gcb_0720[gcb_0720$Category=="Rewarming",], size=1.25) +
  my_theme_blank + colScale + 
  theme(axis.text.x = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  scale_x_continuous(breaks= seq(0,36000,3600), labels = seq(0,10,1)) +
  #scale_y_continuous(breaks= seq(0,50,10)) +
  xlab("Hours") + 
  ylab("Energy expended (J) per second")
