library(dplyr)
library(tidyr)
library(stringr)
# library(RcppRoll)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# fileName <- "input1.txt"
fileName <- "input2.txt"

contents <- readChar(fileName, file.info(fileName)$size)
signals <- unlist(str_split(contents, " | "))
signals <- signals[signals != "|"]
breaks <- grep("\r", signals)
vals <- c(signals, rep("", length(breaks)))
id <- c(seq_along(signals), breaks+0.5)
signals <- vals[order(id)]
breaks <- grep("\r", signals)

split_and_copy <- function(n, sigs){
  parts <- unlist(strsplit(sigs[n], "\r\n"))
  return(parts)
}

num_of_parts <- length(breaks)*2
odd <- seq(1,num_of_parts, 2)
even <- seq(2,num_of_parts, 2)

parts <- unlist(lapply(breaks, split_and_copy, sigs=signals))
signals[breaks] <- parts[odd]
signals[breaks+1] <- parts[even]

signals <- matrix(signals, byrow=TRUE, ncol=14)

inputs <- signals[,1:10]
outputs <- signals[,11:14]

nsegs <- apply(outputs, c(1,2), nchar)

# Part1
# print(sum(nsegs == 2 | nsegs == 3 | nsegs == 4 | nsegs == 7))
input_seg_counts <- apply(inputs, c(1,2), nchar)

sorter <- function(s){
  return(paste(sort(unlist(strsplit(s, ""))), collapse=""))
}

signals <- apply(signals, c(1,2), sorter)

find_match <- function(segs, s, s_is_sub=TRUE){
  s <- unlist(strsplit(s, ""))
  for(seg in segs){
    letters <- unlist(strsplit(seg, ""))
    if(s_is_sub & setequal(intersect(s,letters),s)){
      return(seg)
    }
    if(!s_is_sub & setequal(intersect(s,letters),letters)){
      return(seg)
    }
  }
}

# Order-of-knowing: (1,4,7,8,3,9,5,6,0,2)

sigint <- function(s){
  inp <- s[1:10]
  output <- s[11:14]
  segs5 <- inp[nchar(inp) == 5]
  segs6 <- inp[nchar(inp) == 6]

  lookup <- c()
  # nseg:1,4,7,8
  #"one"
    lookup[1] <- inp[nchar(inp) == 2]
  #"four"
    lookup[4] <- inp[nchar(inp) == 4]
  #"seven"
    lookup[7] <- inp[nchar(inp) == 3]
  #"eight"
    lookup[8] <- inp[nchar(inp) == 7]
  #"three"
    # 5seg, and overlaps w/1: 3
    seg <- find_match(segs5, lookup[1])
    lookup[3] <- seg
    segs5 <- segs5[segs5 != seg]
  #"nine"
    # 6seg, and overlaps w/ 3: 9
    seg <- find_match(segs6, lookup[3])
    lookup[9] <- seg
    segs6 <- segs6[segs6 != seg]
  #"five"
    # 5seg, and "underlaps" w/ 9: 5
    seg <- find_match(segs5, lookup[9], s_is_sub=FALSE)
    lookup[5] <- seg
    segs5 <- segs5[segs5 != seg]
  #"six"
    # 6seg, and overlaps w/ 5: 6
    seg <- find_match(segs6, lookup[5])
    lookup[6] <- seg
    segs6 <- segs6[segs6 != seg]
  #"zero"
    # 6seg, last one: 0
    lookup[10] <- segs6[1]
  #"two"
    # 5seg, last one: 2
    lookup[2] <- segs5[1]
    
  aa <- match(output[1], lookup) %% 10
  bb <- match(output[2], lookup) %% 10
  cc <- match(output[3], lookup) %% 10
  dd <- match(output[4], lookup) %% 10
  return(as.numeric(paste(c(aa,bb,cc,dd), collapse="")))
}
print(sum(apply(signals, 1, sigint)))