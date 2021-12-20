library(tidyverse)
library(here)
library(reshape)
# library(purrr)
# library(daggity)

# fileName <- "input1.txt"
fileName <- "input2.txt"

filePath <- here("2021", "day14", fileName)

contents <- readChar(filePath, file.info(filePath)$size)
lines <- unlist(str_split(contents, "\r\n"))
template <- lines[1]
mappers <- lines[3:length(lines)]
ending_letter <- str_sub(template, -1)



make_polymer <- function(template, mapping){
  template <- unlist(str_split(template, ""))
  N <- length(template)
  ind1 <- seq(1:(N-1))
  ind2 <- ind1 + 1
  pairs <- data.frame(link1=template[ind1], link2=template[ind2]) %>%
    unite("pair", link1, link2, sep="")
  pairs <- pairs %>%
    count(pair)
  
  output <- data.frame(pair = mapping$before, n = 0) %>%
    arrange(pair)
  output <- left_join(output, pairs, "pair") %>%
    mutate(n = n.y) %>%
    select("pair", "n")
  output$n <- replace_na(output$n, 0)
  return(output)
}

make_mapping <- function(crude){
  mapping <- data.frame(crude) %>%
    separate(crude, into = c("before", "insert"), sep = " -> ") %>%
    separate(before, into = c("first", "second"), sep = 1, remove = FALSE) %>%
    unite(after, first, insert, second, sep = "", remove = FALSE) %>%
    unite(new_coup1, first, insert, sep = "", remove = FALSE) %>%
    unite(new_coup2, insert, second, sep = "", remove = FALSE) %>%
    arrange(before) %>%
    relocate("before", "first", "insert", "second", "after", "new_coup1", "new_coup2")
  return(mapping)
}

grow <- function(polymap){
  m <- data.frame(pair = polymap$before, m = 0)
  for(i in 1:nrow(polymap)){
    row <- polymap[i,]
    count <- row$n
    dest1 <- row$new_coup1
    dest2 <- row$new_coup2
    m[which(m$pair == dest1),]$m <- m[which(m$pair == dest1),]$m + count
    m[which(m$pair == dest2),]$m <- m[which(m$pair == dest2),]$m + count
  }
  polymap$n <- m$m
  return(polymap)
}

grow_N_times <- function(polymap, N){
  for(n in 1:N){
    polymap <- grow(polymap)
  }
  return(polymap)
}

mapping <- make_mapping(mappers)
polymer <- make_polymer(template, mapping)
polymap <- left_join(mapping, polymer, by = c("before" = "pair"))


N=40
polymap <- grow_N_times(polymap, N)

letter_counts <- polymap %>% 
  group_by(first) %>% 
  summarise(sum = sum(n))

least <- which(letter_counts == ending_letter)
letter_counts$sum[least] <- letter_counts$sum[least] + 1

maxmin <- max(letter_counts$sum) - min(letter_counts$sum)
print(maxmin)

