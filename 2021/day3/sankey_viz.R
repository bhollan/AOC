library(plotly)

fig <- plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = c("Root",  "OS-1", "O2-2", "O2-3", "O2-4",  "O2-5",  "O2-6",
                       "O2-7", "O2-8", "O2-9", "O2-10", "O2-11",
                      "CO2-1","CO2-2","CO2-3","CO2-4", "CO2-5", "CO2-6",
                      "CO2-7","CO2-8","CO2-9","CO2-10","CO2-11","CO2-12"),
    color = c(rep("blue", 12), rep("green", 12)),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  
  link = list(
    source = c(0,1,2,3,4,5,6,7,8, 9,10, 0,12,13,14,15,16,17,18,19),
    target = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
    value =  c(519, 263, 138, 72, 36, 18, 10, 6, 3, 2, 1, 
                            481, 225, 111, 55, 27, 12, 6, 3, 1)
  )
)
fig <- fig %>% layout(
  title = "Basic Sankey Diagram",
  font = list(
    size = 16
  )
)

fig