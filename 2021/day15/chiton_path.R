library(tidyverse)
library(here)
library(igraph)

# fileName <- "input1.txt"
fileName <- "input2.txt"

filePath <- here("2021", "day15", fileName)

contents <- readLines(filePath) %>%
  str_split("", simplify = TRUE) %>%
  apply(c(1,2), FUN = as.numeric)


#PART 1
# lat <- make_lattice(c(dim(contents))) %>%
#   as.directed(mode = "mutual")
#
# pth <- shortest_paths(lat,
#                       1,
#                       length(contents),
#                       mode = "out",
#                       weights = contents[get.edgelist(lat)[,2]])
# pth <- unlist(pth$vpath)
# pth <- pth[2:length(pth)]
# print(sum(contents[pth]))

n = 4

larger <- contents
basic <- contents
for(i in 1:n){
  # bind a new copy of the original below
  basic <- (basic %% 9) + 1
  larger <- rbind(larger, basic)
}

w <- ncol(larger)
basic <- larger

for(i in 1:n){
  # bind a new copy of the original to-right
  basic <- (basic %% 9) + 1
  larger <- cbind(larger, basic)
}

#PART 2
lat <- make_lattice(c(dim(larger))) %>%
  as.directed(mode = "mutual")
pth <- shortest_paths(lat,
                      1,
                      length(larger),
                      mode = "out",
                      weights = larger[get.edgelist(lat)[,2]])
pth <- unlist(pth$vpath)
pth <- pth[2:length(pth)]
print(sum(larger[pth]))






