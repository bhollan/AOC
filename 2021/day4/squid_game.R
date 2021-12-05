library(dplyr)
library(tidyr)
library(stringr)
# library(RcppRoll)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

fileName <- "input2.txt"

lines <- readChar(fileName, file.info(fileName)$size)

data <- data.frame(info = unlist(str_split(lines, "\r\n\r\n")))
draws <- as.numeric(strsplit(data[1,], ",")[[1]])
boards <- data[2:nrow(data),]

play_move <- function(board, draw){
  loc <- which(board==draw)
  if(length(loc)==0){
    return(board)
  }
  board[loc] = -1
  return(board)
}

board_wins <- function(board){
  return(any(colSums(board)==-5) | any(rowSums(board)==-5))
}

prep_board <- function(board){
  board <- gsub("\r\n", " ", board)
  board <- gsub("\\s+", " ", board)
  if(substr(board,1,1)==" " ){
    board <- substr(board, 2,nchar(board))
  }
  board <- as.numeric(strsplit(board, " ")[[1]])
  board <- matrix(board, byrow=TRUE, ncol=5)
  
  return(board)
}

play_board <- function(board, draws, returning_index=TRUE){
  i <- 1
  for(draw in draws){
    board <- play_move(board, draw)
    if(board_wins(board)){
      if(returning_index){
        return(i)
      } else {
        return(board)
      }
    }
    i <- i + 1
  }
  return(-1)
}

score_board <- function(board, winning_draw){
  unmarked_sum <- sum(rowSums(board)) + length(which(board==-1))
  return(winning_draw*unmarked_sum)
}

play_boards <- function(boards, draws){
  B <- length(boards)
  D <- length(draws)
  speeds <- c(rep(D, B))
  scores <- c(rep(0,D))
  board_index <- 1
  for(board in boards){
    board <- prep_board(board)
    winning_index <- play_board(board, draws)
    won_board <- play_board(board, draws, returning_index=FALSE)
    if(winning_index == -1){
      speeds[board_index] <- D
      scores[board_index] <- 0
    } else {
      speeds[board_index] <- winning_index
      board_score <- score_board(won_board, draws[winning_index])
      scores[board_index] <- board_score
      board_index <- board_index + 1
    }
  }
  # print(scores)
  # print(speeds)
  # return(scores[which.min(speeds)])
  return(scores[which.max(speeds)])
}

print(play_boards(boards, draws))


