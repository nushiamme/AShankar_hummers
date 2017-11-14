##Trying out animation for torpor plots

library(animation)
#library(requireR)
library(magick)
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

im.convert("*.png", output = "bm-animation1.gif")

image <- readPNG(system.file("*.png"))
write.gif(delay = 3, image = "*.png", filename = "example.gif")

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

# Make a ggplot, but add frame=year: one image per year
#gpminder_data <- gapminder::gapminder

gcb_gif <- ggplot(gcb_0720, aes(Time_hour, EE_J, frame = TimeSlot)) +
  geom_point() +  my_theme #+ scales

# Make the animation!
gganimate(gcb_gif)

# Save it to Gif
gganimate(p_gif, "271_gganimate.gif")
