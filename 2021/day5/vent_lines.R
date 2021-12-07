library(dplyr)
library(tidyr)
library(stringr)
# library(RcppRoll)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# fileName <- "input1.txt"
fileName <- "input2.txt"

contents <- readChar(fileName, file.info(fileName)$size)
lines <- data.frame(vertices = unlist(str_split(contents, "\r\n")))
points <- unlist(str_split(lines$vertices, " -> "))
data <- as.data.frame(matrix(as.numeric(unlist(strsplit(points, ","))), byrow=TRUE, ncol=4))
data <- data + 1 #R uses 1-indexed everything.  ie (0,0) does not exist and should become (1,1)
colnames(data) <- c("x1", "y1", "x2", "y2")

data <- mutate(data, Xdelta = x2 - x1, Ydelta = y2 - y1)

maxX <- max(data$x1, data$x2)
maxY <- max(data$y1, data$y2)

seabed <- matrix(0, maxX, maxY)
# vent_points <- data[which(data$Xdelta==0 | data$Ydelta==0),]
vent_points <- data


connect_the_dots <- function(point,sea){
  # print(point$x1)
  if(point$Xdelta==0){
    vert <- c(point$y1:point$y2)
    hori <- c(rep(point$x1,abs(point$Ydelta)+1))
    sea[vert, hori] <- sea[vert, hori] + 1
  } else if(point$Ydelta==0) {
    vert <- c(rep(point$y1,abs(point$Xdelta)+1))
    hori <- c(point$x1:point$x2)
    sea[vert, hori] <- sea[vert, hori] + 1
  } else {
    vert <- c(point$y1:point$y2)
    hori <- c(point$x1:point$x2)
    diag(sea[vert, hori]) <- diag(sea[vert, hori]) + 1
  }
  return(sea)
}

for(n in 1:nrow(vent_points)){
  seabed <- connect_the_dots(vent_points[n,], seabed)
}


print(length(which(seabed>1)))
