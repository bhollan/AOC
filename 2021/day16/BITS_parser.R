library(tidyverse)
library(here)
library(zeallot)


#CREDIT WHERE CREDIT IS DUE (I straight-up copied from drob)
# SOME FROM /R/ADVENTOFCODE, SOME FROM R4DS SLACK AOC GROUP
# https://davidyue.live/aoc/app/packetcode.html
# https://i.redd.it/m0r4gl1wsx581.png
# https://vikithedev.eu/aoc/2021/16/
# https://pbs.twimg.com/media/FGx7aVAXwAEG72A.jpg
# https://pbs.twimg.com/media/FGx7UIZWYAYr3Ip.jpg



# fileName <- "input1.txt"
fileName <- "input2.txt"

filePath <- here("2021", "day16", fileName)

contents <- readLines(filePath) %>%
  str_split(",", simplify = TRUE)

#several "practice" lines
packets <- contents[,1]
# answers <- contents[,2] %>%
#   as.numeric()

to_binary <- function(s){
  s %>%
    str_split("") %>%
    pluck(1) %>%
    map_dbl(strtoi, base = 16) %>%
    map(intToBits) %>%
    map(as.integer) %>%
    map_chr(~ paste0 (rev(.[1:4]), collapse = "")) %>%
    paste0(collapse = "")
}

to_integer <- function(s){
  s <- as.numeric(str_split(s, "")[[1]])
  sum(s * 2^seq(length(s) - 1, 0))
}

chomp <- function(s, i){
  list(str_sub(s, 1, i), str_sub(s, i + 1))
}

chomp_int <- function(s, i){
  parts <- chomp(s, i)
  list(to_integer(parts[[1]]), str_sub(parts[[2]]))
}

vtot <- 0

packet_parser <- function(s){
  c(version, s) %<-% chomp_int(s, 3)
  vtot <<- vtot + version
  c(type, s) %<-% chomp_int(s, 3)
  
  val_bits <- ""
  if (type == 4){         # literal (operand)
    while(TRUE){
      c(chunk, s) %<-% chomp(s, 5)
      c(keep_chunking, four_bits) %<-% chomp(chunk, 1)
      val_bits <- paste0(val_bits, four_bits)
      if(keep_chunking != "1"){
        break
      }
    }
    value <- to_integer(val_bits)
    packets <- NULL
  } else {                # operator
    c(indicator, s) %<-% chomp(s, 1)
    if(indicator == "0"){ # 15 bits: #of bits remaining
      c(bits_following, s) %<-% chomp_int(s, 15)
      c(following, s) %<-% chomp(s, bits_following)
      
      packets <- list()
      while(str_length(following) > 0){
        c(packet, following) %<-% packet_parser(following)
        packets <- c(packets, list(packet))
      }
    } else {              # 11 bits: #of subpackets remaining
      c(packets_following, s) %<-% chomp_int(s, 11)
      
      packets <- list()
      for(i in seq_len(packets_following)){
        c(packet, s) %<-% packet_parser(s)
        packets <- c(packets, list(packet))
      }
    }
    packet_values <- map_dbl(packets, "value")
    value <- if (type == 0){
      sum(packet_values)
    } else if (type == 1){
      prod(packet_values)
    } else if (type == 2){
      min(packet_values)
    } else if (type == 3){
      max(packet_values)
    } else if (type == 5){
      as.integer(packet_values[1]  > packet_values[2])
    } else if (type == 6){
      as.integer(packet_values[1]  < packet_values[2])
    } else if (type == 7){
      as.integer(packet_values[1] == packet_values[2])
    }
  }
  # cat(value)
  # cat("\n")
  list(packet = list(version = version,
                     type = type,
                     packets = packets,
                     value = value),
       remainder = s)
}


