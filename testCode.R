library(plotly)

fig <- plot_ly(mtcars, x = ~disp, color = I("black"))
fig <- fig %>% add_markers(y = ~mpg, text = rownames(mtcars), showlegend = FALSE)
fig <- fig %>% add_lines(y = ~fitted(loess(mpg ~ disp)),
                         line = list(color = '#07A4B5'),
                         name = "Loess Smoother", showlegend = TRUE)
fig <- fig %>% layout(xaxis = list(title = 'Displacement (cu.in.)'),
                      yaxis = list(title = 'Miles/(US) gallon'),
                      legend = list(x = 0.80, y = 0.90))

fig