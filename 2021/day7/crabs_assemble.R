  # library(dplyr)
  # library(tidyr)
  # library(stringr)
  # library(RcppRoll)
  
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# fileName <- "input1.txt"
fileName <- "input2.txt"

contents <- readChar(fileName, file.info(fileName)$size)
crab_locs <- as.numeric(unlist(str_split(contents, ",")))

middle <- floor(mean(crab_locs))
median_fuel <- sum(abs(crab_locs - middle))
# print(sum(abs(crab_locs - middle)))

whatever <- function(n, mid){
  return(sum(abs(seq(n, mid)-max(n, mid))))
}
print(sum(sapply(crab_locs, whatever, mid=middle)))
