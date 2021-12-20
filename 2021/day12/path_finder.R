library(tidyverse)
library(here)
library(purrr)
# library(daggity)

# fileName <- "input1a.txt"
# fileName <- "input1b.txt"
# fileName <- "input1c.txt"
fileName <- "input2.txt"

#1a:
#     start
#     /   \
# c--A-----b--d
#     \   /
#      end

# start,A,b,A,c,A,end
# start,A,b,A,end
# start,A,b,end
# start,A,c,A,b,A,end
# start,A,c,A,b,end
# start,A,c,A,end
# start,A,end
# start,b,A,c,A,end
# start,b,A,end
# start,b,end

fileName <- here("2021", "day12", fileName)

contents <- readChar(fileName, file.info(fileName)$size)
connections <- unlist(str_split(contents, "\r\n"))
conns <- length(connections)

raw_data  <- unlist(strsplit(connections, "-"))
edges <- matrix(raw_data, byrow=TRUE, nrow=conns)
# Swap spots so 'start' is always in left and 'end' is always in right
edges[(edges[,2] == "start"),] <- rev(edges[(edges[,2] == "start"),])
edges[(edges[,1] == "end"),] <- rev(edges[(edges[,1] == "end"),])

extend_paths <- function(edgs, pths){
  if(all(sapply(pths, function(p){tail(p,1) == "end"}))){
    return(pths)
  }
  out_paths <- list()
  out_index <- 1
  for(p in 1:length(pths)){
    ending_node <- tail(pths[[p]], 1)
    if(ending_node == "end"){
      out_paths[[out_index]] <- c(pths[[p]])
      out_index <- out_index + 1
      next
    }
    next_nodes <- c(edgs[edgs[,1] == ending_node,2], edgs[edgs[,2] == ending_node,1])
    next_nodes <- next_nodes[next_nodes != "start"]
    for(nn in 1:length(next_nodes)){
      next_node <- next_nodes[nn]
      if(next_node == tolower(next_node)){
        if(next_node %in% pths[[p]]){
          next
        }
      }
      out_paths[[out_index]] <- c(pths[[p]], next_node)
      out_index <- out_index + 1
    }
  }
  return(extend_paths(edgs, out_paths))
}

setup_paths <- function(grph){
  paths <- list()
  #build starting paths
  first_phase <- c(edges[edges[,1] == "start",2], edges[edges[,2] == "start",1])
  for(stage in first_phase){
    paths[[length(paths) + 1]] <- c("start", stage)
  }
  return(paths)
}

build_graph <- function(edges){
  web <- list()
  for(n in 1:nrow(edges)){
    src <- edges[n,1]
    dest  <-  edges[n,2]
    if(src %in% names(web)){
      web[[src]] <- c(web[[src]], dest)
    } else {
      web[[src]] <- c(dest)
    }
    # place the "reverse" edge for non-start/end rows
    if(src == "start" | dest == "end"){
      next
    }
    if(dest %in% names(web)){
      web[[dest]] <- c(web[[dest]], src)
    } else {
      web[[dest]] <- c(src)
    }
  }
  return(web)
}


g <- build_graph(edges)
ps <- setup_paths(g)
single_small <- extend_paths(edges, ps)

# PART 1
# length(single_small)


# PART 2
  # for each lowercase node 'N':
  #    take out all paths that don't use N
  #    cut off each path where it touches N
#    "extend" each of those paths, keeping only 2-touch paths

bastardizer <- function(edges, orig_paths){
  #process lowercase transit nodes
  #CHANGE TO NOT CUT UP PRIOR BASTARD PATHS
  #ALSO ENSURE SINGLE_PASS PATHS ARE PRESERVED
  nodes <- unique(as.vector(edges[edges != "start" & edges != "end"]))
  nodes <- nodes[tolower(nodes) == nodes]
  all_paths <- list()
  all_paths <- orig_paths
  for(node in nodes){
    paths <- c(orig_paths, sever_paths(edges, orig_paths, node))
    all_paths <- c(all_paths, hair_extensions(edges, paths, node))
    all_paths <- unique(sapply(all_paths, function(p){paste(p, collapse=",")}))
    # pathos <- sapply(pathos, function(p){str_split(p, ",")})
  }
  #DEDUPLICATION!!
  return(all_paths)
}

sever_paths <- function(edges, pathis, bastard){
  #take in all paths, keep only paths with bastard, cut off at bastard
  pathos <- pathis[sapply(pathis, function(p){bastard %in% p})]
  pathos <- sapply(pathos, function(p){p[1:which(p == bastard)]})
  pathos <- unique(sapply(pathos, function(p){paste(p, collapse=",")}))
  pathos <- sapply(pathos, function(p){str_split(p, ",")})
  names(pathos) <- NULL
  return(pathos)
}

hair_extensions <- function(edgs, pths, bastard){
  if(all(sapply(pths, function(p){tail(p,1) == "end"}))){
    return(pths)
  }
  out_paths <- list()
  out_index <- 1
  for(p in 1:length(pths)){
    path <- pths[[p]]
    ending_node <- tail(path, 1)
    if(ending_node == "end"){
      count <- length(which(path == bastard))
      if(count == 1){
        next
      }
      out_paths[[out_index]] <- c(path)
      out_index <- out_index + 1
      next
    }
    next_nodes <- c(edgs[edgs[,1] == ending_node,2], edgs[edgs[,2] == ending_node,1])
    next_nodes <- next_nodes[next_nodes != "start"]
    for(nn in 1:length(next_nodes)){
      next_node <- next_nodes[nn]
      if(next_node == tolower(next_node)){
        count <- length(which(path == bastard))
        if(next_node == bastard & count == 2){
          next
        }
        if(next_node != bastard & next_node %in% path) {
          next
        }
      }
      out_paths[[out_index]] <- c(path, next_node)
      out_index <- out_index + 1
    }
  }
  return(hair_extensions(edgs, out_paths, bastard))
}
