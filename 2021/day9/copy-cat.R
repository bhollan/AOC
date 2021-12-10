#stolen from GH: https://raw.githubusercontent.com/MCMaurer/advent_of_code/main/2021/day_9/day_9.R
library(tidyverse)

d <- read_lines("input2.txt")

n_col <- str_length(d[1])
n_row <- length(d)

d <- d %>% 
  str_c(collapse = "") %>% 
  str_split("") %>% 
  pluck(1) %>% 
  as.numeric() %>% 
  matrix(nrow = n_row, byrow = T)

b <- rbind(d[-1,], rep(10, n_col))
a <- rbind(rep(10, n_col), d[-n_row,])
l <- cbind(rep(10, n_row), d[,-n_col])
r <- cbind(d[,-1], rep(10, n_row))

sum(d[d < b & d < a & d < l & d < r]+1)

# part 2 ------------------------------------------------------------------

# why reinvent the wheel?
library(raster)

m <- d
m[m != 9] <- 1
m[m == 9] <- 0

r <- raster(m)
plot(r)
regions <- clump(r, directions = 4)
plot(regions)

freq(regions) %>% 
  as_tibble() %>% 
  filter(!is.na(value)) %>% 
  slice_max(count, n = 3) %>% 
  pull(count) %>% 
  prod()


r <- raster(d)
extent(r) <- extent(c(0, n_row, 0, n_col) + 0.5)

## Find the maximum value within the 9-cell neighborhood of each cell
min_narm <- function(x) min(x, na.rm=TRUE)
ww <- matrix(1, nrow=3, ncol=3) ## Weight matrix for cells in moving window
ww[cbind(c(1,1,3,3), c(1,3,1,3))] <- 0
ww
localmin <- focal(r, fun = min_narm, w = ww, pad = F)

## Does each cell have the maximum value in its neighborhood?
r2 <- r == localmin

## Get x-y coordinates of those cells that are local maxima
minima <- xyFromCell(r2, Which(r2==1, cells=TRUE))
minima

dmin <- d[minima]
sum(dmin[dmin != 9]+1)

