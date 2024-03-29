### Color palettes
## From coolers website
my_colors_five <- c("#23988aff",  "#F38BA8", "#440558ff",  "#9ed93aff", "darkgoldenrod2") #NSTDA
my_colors2 <- c("#efdb00", "#3d5887", "#032b43", "#a0b9c6", "#b24c63") ## Approved by Mario
my_colors3 <- c("#ca054d", "#ffd400", "#032b43", "#7b9e89", "#1c448e") ## Approved by Mario
my_colors4 <- c("#ca054d", "#ffd400", "#032b43", "#f75c03", "#1c448e")
my_colors5 <- c("#004e64", "#ffba08", "#73a580", "#f786aa", "#685369")
my_colors6 <- c("#004e64", "#ffba08", "#f7b2bd", "#c60f7b", "#bbc7a4")
my_colors7 <- c("#ffe74c", "#508aa8", "#242f40", "#c60f7b", "#bbc7a4")
my_colors8 <- c("#7f7caf", "#fcbf49", "#171738", "#f71735", "#c9e4ca")
my_colors9 <- c("#5bba6f", "#297373", "#171738", "#9e4770", "#f8c630")
my_colors10 <- c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51")
my_gradient <- c("#823de9", "#7855ce", "#6e6eb2", "#648697", "#599e7c", "#4fb760", "#45cf45")
my_colors_green <- c("#cad2c5", "#84a98c", "#52796f", "#354f52", "#2f3e46")
my_colors11 <- c("#355070", "#6d597a", "#b56576", "#e56b6f", "#eaac8b")
my_col_rainbows <- c("#f94144", "#f3722c", "#f8961e", "#f9844a",
"#f9c74f", "#90be6d", "#43aa8b", "#4d908e", "#577590", "#277da1")


## TRY THIS
##MetBrewer package for colors
#colorblindr for simulating colorblindness on R figures
# install.packages("PNWColors")


#display.brewer.pal(n = 5, name = 'Dark2',)
par(mfrow=c(5,2))
pie(rep(1, length(my_colors2)), col = my_colors2)
# par(mar = rep(0, 4))
pie(rep(1, length(my_colors3)), col = my_colors3)
pie(rep(1, length(my_colors4)), col = my_colors4)
pie(rep(1, length(my_colors5)), col = my_colors5)
pie(rep(1, length(my_colors6)), col = my_colors6)
pie(rep(1, length(my_colors7)), col = my_colors7)
pie(rep(1, length(my_colors8)), col = my_colors8)
pie(rep(1, length(my_colors9)), col = my_colors9)
pie(rep(1, length(my_colors10)), col = my_colors10)
pie(rep(1, length(my_colors_five)), col = my_colors_five)
#pie(rep(1, length(my_gradient)), col = my_gradient)
