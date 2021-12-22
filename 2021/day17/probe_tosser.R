library(tidyverse)
library(here)
library(zeallot)

# I know I kind of "phoned it in" on this one.  I'm just really tired

# fileName <- "input1.txt"
fileName <- "input2.txt"

filePath <- here("2021", "day17", fileName)

contents <- readLines(filePath) %>%
  str_split(": |, ", simplify = T)

trenchX <- contents %>%
  pluck(2) %>%
  str_split("=", simplify = T) %>%
  pluck(2) %>%
  str_split("\\.\\.", simplify = T) %>%
  as.numeric
trenchX <- seq(trenchX[1], trenchX[2])

trenchY <- contents %>%
  pluck(3) %>%
  str_split("=", simplify = T) %>%
  pluck(2) %>%
  str_split("\\.\\.", simplify = T) %>%
  as.numeric
trenchY <- seq(trenchY[1], trenchY[2])


tossX <- function(velX, trenchX){
  pos <- 0
  while(pos <= tail(trenchX,1)){
    pos <- pos + velX
    if(pos %in% trenchX){
      return(TRUE)
    }
    velX <- velX + (-1)*sign(velX)
    if(velX == 0){
      return(FALSE)
    }
  }
  return(FALSE)
}

tossY <- function(velY, trenchY, limit=10000){
  pos <- 0
  maxY <- 0
  tries <- 0
  while(pos > trenchY[1]){
    dir <- sign(velY)
    pos <- pos + velY
    velY <- velY - 1
    tries <- tries + 1
    if(pos > maxY){
      maxY <- pos
    }
    if(dir == -1 & pos %in% trenchY){
      return(1)
    }
    if(tries > limit){
      return(-1)
    }
  }
  return(-1)
}

# Xx <- seq(1:tail(trenchX, 1))
# tossedX <- map_dbl(Xx, tossX, trenchX=trenchX)
# trenchHitX <- which(tossedX == 1)
# 
# trenchHitY <- c()
# for(i in -1000:1000){
#   y <- tossY(i, trenchY)
#   if(y != -1){
#     trenchHitY <- c(trenchHitY, i)
#   }
# }

tossPair <- function(vel, trench){
  trenchX <- trench[[1]]
  trenchY <- trench[[2]]
  pos <- c(0,0)
  velX <- vel[1]
  velY <- vel[2]
  
  while(TRUE){
    pos[1] <- pos[1] + velX
    pos[2] <- pos[2] + velY

    velX <- velX + (-1)*sign(velX)
    velY <- velY - 1
    if(pos[1] %in% trenchX & pos[2] %in% trenchY){
      return(TRUE)
    }
    if(pos[2] < min(trenchY)){
      return(FALSE)
    }
  }
  #execution should never reach here
  print("SHOULD NEVER HAPPEN")
}

# trenchHitX <- unique(trenchHitX)
# trenchHitY <- unique(trenchHitY)

# hits <- 0
# for(i in trenchHitX){
#   for(j in trenchHitY){
#     hits <- hits + tossPair(c(i, j), list(trenchX, trenchY))
#   }
# }

