library(dplyr)
library(tidyr)
library(stringr)
# library(RcppRoll)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


fileName <- "input1.txt"
# fileName <- "input2.txt"

contents <- readChar(fileName, file.info(fileName)$size)
fish <- as.numeric(unlist(str_split(contents, ",")))
#Take example and "hash" it out to create the "correct" mappings for the first 18 days

#allocate quantities to vector slots
# every day: 
#   create new "eights"
#   add "zeros" to "six" (and clear)