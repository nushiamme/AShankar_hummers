## 3D surface plot of daily energy expenditure vs. activity and temperature

# Sign in to plotly - like Git
library(plotly)
py <- plot_ly()

x_vec = c(seq(-5, 4.9, 0.1))
x_matrix = matrix(c(x_vec), nrow = 100, ncol = 1)
y_matrix = matrix(c(x_vec), nrow = 1, ncol = 100)

data <- list(
  x = x_vec,
  y = x_vec,
  z = matrix(c(cos(x_matrix %*% y_matrix) + sin(x_matrix %*% y_matrix)), nrow = 100, ncol = 100),
  type = "surface")

layout <- list(
  title = "Waaaves in r",
  scene = list(bgcolor = "rgb(244, 244, 248)"))

response <- py$plotly(data,
                      kwargs = list(
                        layout = layout,
                        filename = "waves example",
                        fileopt = "overwrite"))

df.list <- list(x = 1:100,
                y = 500:599,
                z = matrix(rnorm(10000), nrow = 100))

df.dataframe <- data.frame(x1 = 1:100,
                           y1 = 500:599,
                           z1 = sample(1:200, size = 100))


# Works fine
plot_ly(x = df.list$x1, y = df.dataframe$y1, z = df.dataframe$z1, type = "surface")
