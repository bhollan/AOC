library(dplyr)
library(tidyr)
library(stringr)
# library(RcppRoll)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# fileName <- "input1.txt"
fileName <- "input2.txt"

contents <- readChar(fileName, file.info(fileName)$size)
field_scan <- unlist(str_split(contents, "\r\n"))
w <- nchar(field_scan[1])
h <- length(field_scan)

raw_data <- as.numeric(unlist(strsplit(field_scan, "")))
scan <- matrix(raw_data, byrow=TRUE, nrow=h)
risk <- matrix(rep(0,w*h), byrow=TRUE, nrow=h)

is_low <- function(i, j, scan){
  if(i < nrow(scan)){ #down is less, FALSE
    if(scan[i+1,j] <= scan[i,j]){
      return(FALSE) 
    }
  }
  if(j < ncol(scan)){ #right is less, FALSE
    if(scan[i,j+1] <= scan[i,j]){
      return(FALSE) 
    }
  }
  if(i > 1){ #above is less, FALSE
    if(scan[i-1,j] <= scan[i,j]){
      return(FALSE) 
    }
  }
  if(j > 1){ #left is less, FALSE
    if(scan[i,j-1] <= scan[i,j]){
      return(FALSE) 
    }
  }
  return(TRUE)
}

for(i in 1:h){
  for(j in 1:w){
    if(is_low(i, j, scan)){
      risk[i,j] <- scan[i,j] + 1
    }
  }
}
#Part1
# print(sum(risk))



mark_neighbors <- function(index, scan){
  h <- nrow(scan)
  w <- ncol(scan)
  out <- c()
  if(index %% h != 0){ #down
    if(scan[index + 1] != 9 & scan[index + 1] > 0){
      out <- c(out, index + 1)
    }
  }
  if(index + h <= h*w){ #right
    if(scan[index + h] != 9 & scan[index + h] > 0){
      out <- c(out, index + h)
    }
  }
  if(index %% h != 1){ #above
    if(scan[index - 1] != 9 & scan[index - 1] > 0){
      out <- c(out, index - 1)
    }
  }
  if(index > h){ #left
    if(scan[index - h] != 9 & scan[index - h] > 0){
      out <- c(out, index - h)
    }
  }
  for(o in out){
    scan[o] <- -2
  }
  return(scan)
}


peel <- function(temp){
  to_check <- which(temp == -2)
  for(spot in to_check){
    temp[spot] <- -1
  }
  for(spot in to_check){
    temp <- mark_neighbors(spot, temp)
  }
  return(temp)
}

bottoms <- which(risk != 0)
i <- 1
basin_counts <- c()

for(bottom in bottoms){
  temp <- scan
  temp[bottom] <- -2
  while(length(which(temp == -2)) > 0){
    temp <- peel(temp)
  }
  basin_counts[i] <- sum(temp == -1)
  i <- i + 1
}

# Part 2
print(prod(tail(sort(basin_counts), 3)))
# image(scan)
