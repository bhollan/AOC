library(tidyverse)
library(stringr)
library(here)
library(raster)
library(reshape2)
library(gganimate)
library(magick)

# fileName <- "input0.txt"
# fileName <- "input1.txt"
fileName <- "input2.txt"

fileName <- here("2021", "day11", fileName)

contents <- readChar(fileName, file.info(fileName)$size)
field_scan <- unlist(str_split(contents, "\r\n"))
w <- nchar(field_scan[1])
h <- length(field_scan)

raw_data  <- as.numeric(unlist(strsplit(field_scan, "")))
brights   <- matrix(raw_data, byrow=TRUE, nrow=h)

flash <- function(brights){
  ww <- matrix(1, nr=3, nc=3)
  ww[2,2] <- 0
  to_flash <- brights > 9
  has_flashed <- to_flash
  while(sum(to_flash) > 0){
    tf <- raster(to_flash)
    flash_adds <- focal(tf, ww, pad=T, padValue=0)
    brights <- brights + as.matrix(flash_adds)
    to_flash <- brights > 9 & !has_flashed
    has_flashed <- has_flashed | to_flash
  }
  brights[brights > 9] <- 0
  output <- list()
  output$brights <- brights
  output$flashes <- sum(has_flashed)
  output$has_flashed <- has_flashed
  return(output)
}


flashNTimes <- function(brights, N, return_counts=FALSE){
  flashes <- 0
  flash_counts <- brights
  flash_counts[,] <- 0
  for(n in 1:N){
    brights <- brights + 1
    results <- flash(brights)
    brights <- results$brights
    flashes <- flashes + results$flashes
    flash_counts <- flash_counts + results$has_flashed
  }
  print(brights)
  if(return_counts){
    return(flash_counts) ### change to counts!! ###
  }
  return(flashes)
}

findFlashPoint <- function(brights){
  flashes <- 0
  step <- 0
  N <- prod(dim(brights))
  while(flashes < N){
    step <- step + 1
    brights <- brights + 1
    results <- flash(brights)
    brights <- results$brights
    flashes <- results$flashes
  }
  print(brights)
  return(step)
}

# flash_counts <- flashNTimes(brights, 100, return_counts = TRUE)
# plot(raster(flash_counts), main="2D Histogram of cell flash counts")


make_many_flashes <- function(brights, N){
  flashes <- 0
  flash_counts <- array(dim=c(10,10,N+1))
  flash_counts[,,] <- 0
  for(n in 1:N){
    brights <- brights + 1
    results <- flash(brights)
    brights <- results$brights
    flash_counts[,,n+1] <- flash_counts[,,n] + as.numeric(results$has_flashed)
  }
  return(flash_counts)
}

make_gif_of_N_steps <- function(brights, N){
  flash_count_per_step <- make_many_flashes(brights, N)
  for(n in 1:N){
    outPNG <- here("2021", "day11", "raster_plots", paste("step", str_pad(n, 3, pad="0"),".png", sep=""))
    png(outPNG, w=600, height=500)
    p <- flash_count_per_step[,,n] %>%
      raster() %>%
      plot() %>%
      title("Flashes per square")
    p
    mtext(paste("Step: ", str_pad(n, 3, pad="0"), sep=""), side=3, col="blue")
    dev.off()
  }
  
  PNGpath <- here("2021", "day11", "raster_plots")
  GIFname <- here("2021", "day11", paste("RastersOf", n, "Steps", ".gif", sep=""))
  list.files(path=PNGpath, pattern = '*.png', full.names = TRUE) %>% 
    image_read() %>%
    image_join() %>%
    image_animate(fps=4) %>%
    image_write(GIFname)
}
















# p <- ggplot(fcps, aes(x=x, y=y, fill=flashes)) +
#   geom_tile() +
#   scale_fill_continuous(low="blue", high="red",
#                         limits=c(min(fcps$flashes), max(fcps$flashes)),
#                         breaks=seq(0,100,by=25),)
# 
# anim <- p +
#   transition_states(
#     step,
#     transition_length = 0.5,
#     state_length = 0.2
#   ) +
#   labs(title = 'Step: {frame_time}', x = 'x', y = 'y') +
#   transition_time(step) +
#   ease_aes('linear')

# anim_save(here("2021", "day11"))





