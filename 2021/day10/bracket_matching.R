library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# raw <- read_lines("input1.txt")
raw <- read_lines("input2.txt")

is_corrupt <- function(raw_line, return_stk=FALSE){
  openers <- c("<",   "[", "{", "(")
  closers <- c(">",   "]", "}", ")")
  scoring <- c(25137, 57, 1197, 3)
  stk <- c()
  for(s in unlist(strsplit(raw_line, ""))){
    if(s %in% openers){
      stk <- c(stk, s)
    } else {
      q <- openers[which(closers == s)]
      if(length(stk) == 0 | tail(stk, 1) != q){
        return(scoring[which(closers == s)])
      }
      if(length(stk) > 1){
        stk <- stk[1:(length(stk) - 1)]
      } else {
        stk <- c()
      }

    }
  }
  if(return_stk){
    return(stk)
  }
  return(0)
}

corrupt <- sapply(raw, is_corrupt, USE.NAMES=FALSE)
# PART 1
# print(sum(corrupt))

incomplete_inds <- which(corrupt == 0)
needed <- sapply(raw[incomplete_inds], is_corrupt, return_stk=TRUE, USE.NAMES=FALSE)


score_completion <- function(need){
  openers <- c("<", "[", "{", "(")
  scoring <- c( 4,   2,   3,   1)
  points <- c()
  for(ch in need){
    points <- c(points, scoring[which(openers == ch)])
  }
  total <- 0
  for(p in points[length(points):1]){
    total <- 5*total + p
  }
  return(total)
}

# score_completion(needed[[5]])
print(median(sort(sapply(needed, score_completion))))