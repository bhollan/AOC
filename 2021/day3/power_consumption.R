library(dplyr)
library(tidyr)
library(stringr)
# library(RcppRoll)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

fileName <- "input2.txt"

lines <- readChar(fileName, file.info(fileName)$size)

data <- data.frame(bits = unlist(str_split(lines, "\r\n")))

W <- nchar(data[1,1])

data <- data %>%
  separate(bits, sprintf("Bit%d", 0:W), "", convert=TRUE, fill="right") %>%
  select(-Bit0) 

gamma_bits <- paste(round(colMeans(data)), collapse="")
gamma <- strtoi(gamma_bits, base=2)
epsilon_bits <- paste(1 - round(colMeans(data)), collapse="")
epsilon <- strtoi(epsilon_bits, base=2)

# Part1
# print(gamma*epsilon)

bits <- data.frame(bits = unlist(str_split(lines, "\r\n")))
numbers <- apply(bits, 1, strtoi, base=2)


peel <- function(data, col_ind, CO2=FALSE){
  if(nrow(data)==1){
    out <- unite(data[1,], col="final", sep="")
    return(strtoi(out$final, base=2))
  }
  col_name <- colnames(data)[col_ind]
  quotient <- mean(unlist(data[col_name]))
  target <- round(quotient)
  if(quotient == 0.5){
    target <- 1
  }
  if(CO2){
    target <- 1 - target
  }
  data <- data[which(data[col_name]==target),]
  col_ind <- col_ind + 1

  return(peel(data, col_ind, CO2))
}


O2 <- peel(data, 1, FALSE)
CO2 <- peel(data, 1, TRUE)
print(O2)
print(CO2)

