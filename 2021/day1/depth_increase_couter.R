library(dplyr)
library(RcppRoll)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Part one, count lines of increasing depth
data <- read.delim("input2.txt", header=FALSE, sep="\n")
names(data) <- c("Depth")
data <- data %>%
  mutate("DepthDelta" = lag(Depth) - Depth)
total_increasing_depths <- tally(data, data$DepthDelta < 0)
# print(total_increasing_depths)

#Part two, count lines of increasing rolling depth sum
data <- data %>%
  mutate("WindowDepth" = roll_sum(Depth, 3, align="right", fill=NA)) %>%
  mutate("WindowDelta" = lag(WindowDepth) - WindowDepth)
total_increasing_windows_depths <- tally(data, data$WindowDelta < 0)