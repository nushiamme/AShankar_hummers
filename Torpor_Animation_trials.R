##Trying out animation for torpor plots

library(animation)
#library(requireR)
#library(magick)
library(caTools)
library(png)
library(gapminder)
library(ggplot2)
#devtools::install_github("dgrtwo/gganimate")
library(gganimate)

setwd("C:\\Users\\ANUSHA\\Dropbox\\Hummingbird energetics\\Tables_for_paper")

gcb_0720 <- read.csv("E14_0720_GCB_no_bsln.csv")

my_theme <- theme_classic(base_size = 30) + 
  theme(axis.title.y = element_text(vjust = 2),
        panel.border = element_rect(colour = "black", fill=NA))

my_theme_blank <- theme_classic(base_size = 30) + 
  theme(axis.title.y = element_text(vjust = 2),
        panel.border = element_blank())

gcb_0720$Category <- factor(gcb_0720$Category, levels=c("N","T"))
torCol <- c("black", "red")
names(torCol) <- levels(gcb_0720$Category)
colScale <- scale_colour_manual(name = "Category", values = torCol)


# Make a ggplot, but add frame=year: one image per year
#gpminder_data <- gapminder::gapminder

for (tslot in unique(gcb_0720$TimeSlot)){
  p_temp <- subset(gcb_0720, TimeSlot==tslot)
  gcb_gif_temp <- ggplot(p_temp, aes(Time2, EE_J, frame = Time_chunks, col=Category)) +
    geom_path(aes(cumulative=T)) +
    my_theme_blank + colScale + 
    theme(axis.text.x = element_text(angle=30, hjust=1, size=20),
          legend.key.height=unit(3,"line"),
          axis.line.x = element_line(colour = "grey50"),
          axis.line.y = element_line(colour = "grey50")) +
    ylim(0,50) + xlab("Time (seconds)") + ylab("Energy expenditure (J)")
  gganimate(gcb_gif_temp, paste("gcb0720_",tslot,".gif"), interval=0.05, 
            ani.width=1500, ani.height=800)
}

gcb_gif_1 <- ggplot(gcb_0720[gcb_0720$TimeSlot==c(5,6),], aes(Time2, EE_J, frame = BirdID, col=Category)) +
  geom_path(aes(cumulative=T)) + my_theme_blank + theme(axis.text.x = element_text(angle=60, hjust=1, size=20),
                                                        axis.line.x = element_line(colour = "grey50"),
                                                        axis.line.y = element_line(colour = "grey50")) +
  scale_color_manual(values = c("black", "red")) + xlab("Time") #+ xlim(0,4060)

gcb_gif_5 <- ggplot(gcb_0720[gcb_0720$TimeSlot==5,], aes(Time2, EE_J, frame = Time_chunks, col=Category)) +
  geom_path(aes(cumulative=T)) + my_theme_blank + theme(axis.text.x = element_text(angle=60, hjust=1, size=20),
                                                        axis.line.x = element_line(colour = "grey50"),
                                                        axis.line.y = element_line(colour = "grey50")) +
  scale_color_manual(values = c("black", "red")) + xlab("Time") #+ xlim(0,4060)

## Animate torpor entry
gcb_gif_5_6 <- ggplot(gcb_0720[gcb_0720$TimeSlot==c(5,6),], aes(Time_hour, EE_J, frame = Time_chunks, col=Category)) +
  geom_path() + my_theme_blank + theme(axis.text.x = element_text(angle=60, hjust=1, size=20),
                                                        axis.line.x = element_line(colour = "grey50"),
                                                        axis.line.y = element_line(colour = "grey50")) +
  scale_color_manual(values = c("black", "red")) + xlab("Time") #+ xlim(0,4060)

## Plotting torpor entry
gcb_gif_5_6 <- ggplot(gcb_0720[gcb_0720$TimeSlot==c(5,6),], aes(SampleNo, EE_J, col=Category)) +
  geom_path() + my_theme_blank + theme(axis.text.x = element_text(angle=30, hjust=1, size=20),
                                       axis.line.x = element_line(colour = "grey50"),
                                       axis.line.y = element_line(colour = "grey50")) +
  scale_color_manual(values = c("black", "red")) + xlab("Time") #+ xlim(0,4060)
gcb_gif_5_6 ## plot still graph

# Make the animation!
gganimate(gcb_gif_5_6, interval=0.075, ani.width=1500, ani.height=800)

# Save it to Gif
gganimate(gcf_gif_5_6, "C:\Users\ANUSHA\Dropbox\Hummingbird energetics\Tables_for_paper\\gcb_0720_5_6.gif")

gcb_gif_total <- ggplot(gcb_0720, aes(SampleNo, EE_J, frame = BirdID, col=Category)) +
  geom_path(aes(cumulative=T)) +
  my_theme_blank + colScale + 
  theme(axis.text.x = element_text(angle=30, hjust=1, size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  ylim(0,50) + xlab("Time (seconds)") + ylab("Energy expenditure (J)")
gganimate(gcb_gif_total, "gcb_0720_total.gif", interval=0.05, ani.width=1200, ani.height=600)



#im.convert("*.png", output = "bm-animation1.gif")

#image <- readPNG(system.file("*.png"))
#write.gif(delay = 3, image = "*.png", filename = "example.gif")

frames = 50

saveGIF(for(i in 1:frames){
  # creating a name for each plot file with leading zeros
  if (i < 10) {name = paste('000',i,'plot.png',sep='')}
  if (i < 100 && i >= 10) {name = paste('00',i,'plot.png', sep='')}
  if (i >= 100) {name = paste('0', i,'plot.png', sep='')}
  x = seq(0, i, 1)
  f.3 = dbinom(x, size = i, prob=.3)
  f.7 = dbinom(x, size = i, prob=.7)
  #saves the plot as a .png file in the working directory
  png(name)
  plot(x, f.3, type='h', xlim = c(0,frames), ylim = c(0,.7), ylab ='probability',   
       main = paste('Binomial density with n = ', i), col = 'red')
  
  lines(x,f.7,type='h',col='blue')
  text(45, .6, 'p = .3', col='red')
  text(45, .6, 'p = .7', col='blue', pos=1)
  dev.off()
  },
  movie.name = "animation.gif", img.name = "Rplot", im.convert = "magick", 
  cmd.fun = system, clean = TRUE, extra.opts = ""
)

saveGIF({
  for (i in 1:10) plot(runif(10), ylim = 0:1)
})

setwd(".\\gif_files")
shell(' "C:\\Program Files\\ImageMagick-7.0.7-Q16\\magick.exe" magick -delay 80 *.png example_1.gif')
