# library(dplyr)
# library(tidyr)
# library(stringr)
# library(RcppRoll)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# fileName <- "input1.txt"
fileName <- "input2.txt"

contents <- readChar(fileName, file.info(fileName)$size)
fish_days <- as.numeric(unlist(str_split(contents, ",")))
#Example"hashed" out to create the "correct" mappings for the first 18 days: input1_as_tables.txt
#R is 1-indexed so day "0" fish are actually day "1" fish here

fish <- as.numeric(c())
for(i in 1:9){
  fish[i] <- sum(fish_days == (i-1))
}

age_fish_by <- function(fish, n){
  for(i in 1:n){
    fish <- c(fish[-(1)], fish[1])
    fish[7] <- fish[7] + fish[9]
    print(fish)
  }
  return(fish)
}

fish <- sum(age_fish_by(fish, 256))
#options(scipen = 100) sets the R session's scientific notation to show us the whole number
print(fish)