library(dplyr)
library(tidyr)
# library(RcppRoll)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Part one, count lines of increasing depth
data <- read.delim("input2.txt", header=FALSE, sep="\n")
names(data) <- c("Action")
data <- data %>%
  separate(Action, c("direction", "mag"), sep=" ")
data <- data %>%
  mutate(down = if_else(direction=="down" | direction == "up",
                         if_else(direction == "down", as.integer(mag), -as.integer(mag)),
                         as.integer(0)))
data <- data %>%
  mutate(forward = if_else(direction=="forward",
                         as.integer(mag),
                         as.integer(0)))
# Part1:
# print(sum(data$down)*sum(data$forward))

data <- data %>%
  mutate(position = cumsum(forward)) %>%
  mutate(aim = cumsum(down)) %>%
  mutate(dives = aim*forward) %>%
  mutate(depth = cumsum(dives))


print(tail(data, 1)$position * tail(data, 1)$depth)

