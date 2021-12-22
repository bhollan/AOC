library(tidyverse)
library(here)
library(zeallot)

fileName <- "example_points.txt"

filePath <- here("2021", "day17", fileName)

examples <- readLines(filePath) %>%
  str_split("  ") %>%
  unlist() %>%
  keep(function(x) x != "") %>%
  str_split(",") %>%
  unlist() %>%
  as.numeric()

exmat <- matrix(examples, byrow=TRUE, ncol=2)
exdf <- data.frame("X"=exmat[,1], "Y"=exmat[,2]) %>%
  arrange(X, Y)

Ys <- unique(exdf$Y)
Xs <- unique(exdf$X)
print(Ys)
print(Xs)
