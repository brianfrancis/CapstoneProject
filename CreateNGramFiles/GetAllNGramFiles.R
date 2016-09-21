library(tm)
library(stringi)
library(RWeka)
library(slam)
library(stylo); 
library(data.table)



foldernames <- c("train1", "train2", "train3", "train4", "train5"
                 , "train6", "train7", "train8", "test", "validation")


setwd("C:/Users/bfrancis/Desktop/Coursera/Capstone/CapstoneProject/CreateNGramFiles")
source("MyFunctions.R")
source("CreateDictionary.R")
source("CreateProbFiles.R")



for (j in 1:2) {

    nbrlines <- -1L
  nbrlines <- 1000
  
  #set the seed
  set.seed(123)
  
  rawdatapath <- "C:/Users/bfrancis/Desktop/Coursera/Capstone/en_US"
  
  con <- file(paste(rawdatapath,"en_US.blogs.txt",sep="/"), "rb", encoding="UTF-8")
  blogdata <- readLines(con, n=nbrlines)
  close(con)
  i <-  sample(rep(1:10),size=length(blogdata), replace=TRUE)
  
  blogsub <- blogdata[i==j]
  
  con <- file(paste(rawdatapath,"en_US.twitter.txt",sep="/"), "rb", encoding="UTF-8")
  twitterdata <- readLines(con, n=nbrlines)
  close(con)
  i <-  sample(rep(1:10),size=length(twitterdata), replace=TRUE)
  
  twittersub <- twitterdata[i==j]
  
  con <- file(paste(rawdatapath,"en_US.news.txt",sep="/"), "rb", encoding="UTF-8")
  newsdata <- readLines(con, n=nbrlines)
  close(con)
  i <-  sample(rep(1:10),size=length(newsdata), replace=TRUE)
  
  newssub <- newsdata[i==j]
  
  rm(blogdata, twitterdata, newsdata)
  
  rawsub <- c(blogsub, twittersub, newssub)
  rm(blogsub, twittersub, newssub)
  
  processeddata <- cleanRawImport(rawsub)
  rm(rawsub)
  
  wordCorp <- getCorp(processeddata)
  rm(processeddata)
  
  if (j==1) initialfile <- TRUE else initialfile <- FALSE
  
  dictionary <- appendToDictionary(wordCorp, "dictionary.csv", "ngramfrequencies", initialfile)
  
  idCorp <- replaceWordsWithIDs(wordCorp, dictionary)
  rm(wordCorp)
  
  appendToNgram(idCorp, filename="twogramfreq.csv", foldername="ngramfrequencies", 
                initialfile, ngramsize=2)
  
  appendToNgram(idCorp, filename="threegramfreq.csv", foldername="ngramfrequencies", 
                initialfile, ngramsize=3)
  
  appendToNgram(idCorp, filename="fourgramfreq.csv", foldername="ngramfrequencies", 
                initialfile, ngramsize=4)
  
  
#  createProbFiles("ngramfrequencies")
  
  
}


