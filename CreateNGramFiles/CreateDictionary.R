library(tm)
library(stringi)
library(RWeka)
library(slam)
library(stylo); 
library(data.table)

setwd("C:/Users/bfrancis/Desktop/Coursera/Capstone/CapstoneProject/CreateNGramFiles")
source("MyFunctions.R")


for (j in 1:2) {

  rawdatapath <- "C:/Users/bfrancis/Desktop/Coursera/Capstone/en_US"
  
  con <- file(paste(rawdatapath,"en_US.blogs.txt",sep="/"), "rb", encoding="UTF-8")
  blogdata <- readLines(con)
  close(con)
  i <-  sample(rep(1:10),size=length(blogdata), replace=TRUE)
  
  blogsub <- blogdata[i==j]
  
  con <- file(paste(rawdatapath,"en_US.twitter.txt",sep="/"), "rb", encoding="UTF-8")
  twitterdata <- readLines(con)
  close(con)
  i <-  sample(rep(1:10),size=length(twitterdata), replace=TRUE)
  
  twittersub <- twitterdata[i==j]
  
  con <- file(paste(rawdatapath,"en_US.news.txt",sep="/"), "rb", encoding="UTF-8")
  newsdata <- readLines(con)
  close(con)
  i <-  sample(rep(1:10),size=length(newsdata), replace=TRUE)
  
  newssub <- newsdata[i==j]
  
  rm(blogdata, twitterdata, newsdata)
  
  rawsub <- c(blogsub, twittersub, newssub)
  rm(blogsub, twittersub, newssub)
  
  processeddata <- cleanRawImport(rawsub)
  rm(rawsub)
  
  corp <- getCorp(processeddata)
  
  if (j==1) initialfile <- TRUE else initialfile <- FALSE
  
  appendToDictionary(corp, "dictionary.csv", "ngramfrequencies", initialfile)
  rm(corp)
  rm(processeddata)
}


appendToDictionary <- function(corp, 
                               dict.filename, dict.foldername,
                               initialfile){
  
  unigramfreq <- data.table(make.frequency.list(corp, value=TRUE, relative=FALSE))
  setnames(unigramfreq,"data", "word")
  setnames(unigramfreq, "N", "freq")
  #setnames ??
  
  
  ##check if this is the first file for the dictionary
  ##if not bring in the existing info and get the next word ID
  ##otherwise next word ID = 1
  if (initialfile == FALSE){
    dictionary <- fread(paste(dict.foldername, dict.filename, sep="/"))
    nextindex <- max(dictionary$wordID) + 1
    unigramfreq[,wordID := seq(nextindex, (nextindex-1+nrow(unigramfreq)))]
    combined <- rbind(dictionary, unigramfreq)
    setkey(combined, word)
    dictionary <- combined[, .(min(wordID), sum(freq)), by=.(word)]
    setnames(dictionary,"V1","wordID")
    setnames(dictionary,"V2","freq")
  } else
  { 
    nextindex <- 1
    unigramfreq[,wordID := seq(1, nrow(unigramfreq))]
    dictionary <- unigramfreq
  }
  
  
  write.csv(dictionary, paste(dict.foldername, dict.filename, sep="/"), 
            row.names=FALSE)

  dictionary  
}

replaceWordsWithIDs <- function(corp, dictionary){
  lapply(corp, )
  
}

