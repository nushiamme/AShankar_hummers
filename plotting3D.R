## 3D surface plot of daily energy expenditure vs. activity and temperature

# Sign in to plotly - like Git
library(plotly)
py <- plot_ly()

energymodels <- 
  read.csv("C:\\Users/ANUSHA/Dropbox/Anusha Committee/BBLH_EnergyBudget/Trial_EnergyBudget_models_act_thermo.csv")

df.list <- list(x1 = energymodels$Thermoreg_mlO2_daytime,
                y1 = energymodels$Activity_cost_mlO2_daytime,
                z1 = energymodels$Daytime_EE)

x_vec = c(seq(-5, 4.9, 0.1))
x_matrix = matrix(c(x_vec), nrow = 100, ncol = 1)
y_matrix = matrix(c(x_vec), nrow = 1, ncol = 100)

data1 <- list(
  x = x_vec,
  y = x_vec,
  z = matrix(c(cos(x_matrix %*% y_matrix) + sin(x_matrix %*% y_matrix)), nrow = 100, ncol = 100),
  type = "surface")

layout <- list(
  title = "Waaaves in r",
  scene = list(bgcolor = "rgb(244, 244, 248)"))

response <- plot_ly(data1$x, data1$y, data1$z, type='surface')


df.list <- list(x = 1:100,
                y = 500:599,
                z = matrix(rnorm(10000), nrow = 100))

df.dataframe <- data.frame(x1 = 1:100,
                           y1 = 500:599,
                           z1 = sample(1:200, size = 100))


# Works fine
plot_ly(x = df.list$x1, y = df.list$y1, z = df.list$z1, type = "surface")


## Half-torus script
par(mar = c(2, 2, 2, 2))
par(mfrow = c(1, 1))
R <- 3
r <- 2
x <- seq(0, 2*pi,length.out=50)
y <- seq(0, pi,length.out=50)
M <- mesh(x, y)

alpha <- M$x
beta <- M$y


surf3D(x = energymodels$Thermoreg_mlO2_daytime,
       y = energymodels$Activity_cost_mlO2_daytime,
       z = energymodels$Daytime_EE,
       colkey=FALSE
       )

surf3D(x = 1
       y = data.frame(c(energymodels$Thermoreg_mlO2_daytime, energymodels$Daytime_EE))
       z = energymodels$Activity_cost_mlO2_daytime
       colvar=x
       )

x2 <- energymodels$Thermoreg_mlO2_daytime
y2 <- energymodels$Daytime_EE
z2 <- energymodels$Activity_cost_mlO2_daytime

M2 <- mesh(x2,y2)
alpha2 <- M2$x2
beta2 <- M2$y2

surf3D(x = alpha2,
       y = beta2,
       z = r * sin(alpha),
       colkey=FALSE,
       bty="b2",
       main="Half of a Torus")
