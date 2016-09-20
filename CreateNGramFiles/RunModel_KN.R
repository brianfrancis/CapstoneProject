##change backoff to only consider lower order backoffs if none found at
##higher level
##use log of probabilities
# clean out anything not in the dictionary

library(stylo)
library(stringi)
library(tm)
library(stringr)
library(data.table)

setwd("C:/Users/bfrancis/Desktop/Coursera/Capstone")
source("MyFunctions.R")


highestngram <- 4
ngramfolder <- "alltrain"

##read in probability tables
onegram.dt <- fread(paste(ngramfolder,"onegram.prob.csv", sep="/"))
twogram.dt <- fread(paste(ngramfolder,"twogram.prob.csv", sep="/"))
threegram.dt <- fread(paste(ngramfolder,"threegram.prob.csv", sep="/"))
fourgram.dt <- fread(paste(ngramfolder,"fourgram.prob.csv", sep="/"))


cleanInput <- function (input){
  clean <- cleanRawImport(input, forprediction=TRUE)
  corp <- unlist(getCorp(clean))
  dictionary <- onegram.dt$ngram
  corp <-  corp[corp %in% dictionary]
  output <- paste(corp, sep="", collapse=" ")
  
  #remove stop words
}


predictNextWordKN <- function(input, answers) {
  x <- cleanInput(input)
  print(x)
  x <- unlist(strsplit(x, " "))
  l <- length(x)
  ##just keep the last maxngram - 1 words
  if (l > (highestngram-1)) {
    x <- x[(l-highestngram+2):l]
  }
  
  p1 <- data.frame()
  p2 <- data.frame()
  p3 <- data.frame()
  p4 <- data.frame()
  for (i in length(x):0) {
    
    if (i == 3) {
      p1 <- fourgram.dt[lookup==paste(x, collapse=" "),
                        .(prediction, p)]
    }
    if (i == 2) {
      l <- paste(x[(length(x)-1): length(x)], collapse=" ")
      p2 <- threegram.dt[lookup==l,
                         .(prediction, p)]
    }
    if (i == 1) {
      l <- x[length(x)]
      p3 <- twogram.dt[lookup==l,
                       .(prediction, p)]
    }
    if (i == 0) {
      p4 <- onegram.dt[,.(ngram, p)]
      names(p4) <- c("prediction", "p")
    }
    
  }
      allp <- data.frame()   
       #backoff in case our condition doesn't exist in higher order n-gram
           if (nrow(p1) > 0) {
               p1$ngramlevel <- 4
               allp <- p1
             
           } 
           if (nrow(p2) > 0) {
             p2$ngramlevel <- 3
             allp <- rbind(allp,p2)
             
           } 
          if (nrow(p3) > 0) {
             p3$ngramlevel <- 2
             allp <- rbind(allp,p3)
           } 
          p4$ngramlevel <- 1
          allp <- rbind(allp,p4)
          allp <- data.table(allp)
          setkey(allp, "prediction", "ngramlevel")
  
  maxngram <- data.table(aggregate(allp$ngramlevel, by=list(allp$prediction), max))
  names(maxngram) <- c("prediction", "ngramlevel")
  setkey(maxngram, "prediction", "ngramlevel")
  finalp <- maxngram[allp, nomatch=0]
  finalp <- finalp[order(finalp$p, decreasing=TRUE),]
  #head(finalp)
  #subsetting different for data tables  ?????

  finalp[finalp$prediction %in% answers]
  #finalp
}


quiz1 <- read.csv("quiz1.csv")

i<-10
answers <- c(as.character(quiz1[i,2]),
             as.character(quiz1[i,3]),
             as.character(quiz1[i,4]),
             as.character(quiz1[i,5]))

predictNextWordKN(as.character(quiz1[i,1]), answers)


quiz2 <- read.csv("quiz2.csv")

i<-1
answers <- c(as.character(quiz2[i,3]),
             as.character(quiz2[i,4]),
             as.character(quiz2[i,5]),
             as.character(quiz2[i,6]))

as.character(quiz2[i,2])
predictNextWordKN(as.character(quiz2[i,2]), answers)
