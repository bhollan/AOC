library(tidyverse)
library(here)
library(reshape)
# library(purrr)
# library(daggity)

# fileName <- "input1.txt"
fileName <- "input2.txt"

fileName <- here("2021", "day13", fileName)

contents <- readChar(fileName, file.info(fileName)$size)
lines <- unlist(str_split(contents, "\r\n"))
points <- lines[1:which(lines == "") - 1]
points <- unlist(strsplit(points, ","))
mpoints <- matrix(as.numeric(points), ncol=2, byrow=TRUE)
mpoints <- mpoints + 1

folds <- lines[(which(lines=="")+1):length(lines)]
folds <- sapply(folds, function(x){gsub("fold along ","", x)})
names(folds) <- NULL
num_folds <- length(folds)
folds_data <- unlist(str_split(folds, "="))

axis <- folds_data[seq(1,num_folds*2,2)]
vals <- folds_data[seq(2,num_folds*2,2)]
foldsdf <- data.frame("axis"=axis, "vals"=vals)
foldsdf$vals <- as.numeric(foldsdf$vals) + 1

# maxX <- max(mpoints[,1]) WHY DID THIS NOT WORK!!!???!??
# maxY <- max(mpoints[,2])

maxX <- max(foldsdf$vals[foldsdf$axis == "x"])*2 - 1
maxY <- max(foldsdf$vals[foldsdf$axis == "y"])*2 - 1

field <- matrix(0, ncol=maxX, nrow=maxY)

for(i in 1:nrow(mpoints)){
  x <- mpoints[i,1]
  y <- mpoints[i,2]
  field[y, x] <- 1
}


for(i in 1:nrow(foldsdf)){
  ax <- foldsdf$axis[i]
  thresh <- foldsdf$vals[i]
  
  if(ax == "y"){
    #flip bottom half up
    upper <- field[1:(thresh-1),]
    lower <- field[nrow(field):(thresh+1),]
    field <- upper + lower
  } else if(ax == "x"){
    #flip the right half to the left
    left <- field[,1:(thresh-1)]
    right <- field[,ncol(field):(thresh+1)]
    field <- left + right
  }
}

# PART 2
# field[field != 0] <- 1
# plot(raster(field))


