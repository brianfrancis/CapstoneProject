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



for (j in 1:2) {
  j <- 1
  #set the seed
  set.seed(123)
  
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
  
 # createNgramFiles(corp, foldername)
  
  
}


# 
# for (j in 1:10) {
# 
#   con <- file("en_US/en_US.blogs.txt", "rb", encoding="UTF-8")
#   blogdata <- readLines(con)
#   close(con)
#   i <-  sample(rep(1:10),size=length(blogdata), replace=TRUE)
#   
#   blogsub <- blogdata[i==j]
#   
#   con <- file("en_US/en_US.twitter.txt", "rb", encoding="UTF-8")
#   twitterdata <- readLines(con)
#   close(con)
#   i <-  sample(rep(1:10),size=length(twitterdata), replace=TRUE)
#   
#   twittersub <- twitterdata[i==j]
#   
#   con <- file("en_US/en_US.news.txt", "rb", encoding="UTF-8")
#   newsdata <- readLines(con)
#   close(con)
#   i <-  sample(rep(1:10),size=length(newsdata), replace=TRUE)
#   
#   newssub <- newsdata[i==j]
#   
#   rm(blogdata, twitterdata, newsdata)
#   
#   allsub <- c(blogsub, twittersub, newssub)
#   rm(blogsub, twittersub, newssub)
#    
#   processeddata <- cleanRawImport(allsub)
#   rm(allsub)
#   
#   foldername <- foldernames[j]
#   
#   createNGramFreqsFromRawData(foldername=foldername, rawdata=processeddata)
#   rm(processeddata)
#   
# }

#merge the 8 training files of ngram freqs into one file with one row per ngram
mergeFreqFiles <- function(filename, foldernames) {
  
  library(data.table)
  for (j in 1:3) {
    newdt <- fread(paste(foldernames[j],filename, sep="/"))
    newdt <- newdt[,.(data,Freq)]
    
    if (j==1) alldt <- newdt
    else {
      alldt <- rbind(alldt, newdt)
      setkey(alldt, data)
      rm(newdt)
      gc()
      alldt <- alldt[, sum(Freq), by=.(data)]
      
      names(alldt) <- c("data", "Freq")
    }
  }
    
  
  write.csv(alldt, paste("alltrain",filename, sep="/"))
  rm(alldt)
  
}

#merge the 8 training files of ngram freqs into one file for each order ngram
mergeFreqFiles("onegramfreqs.csv",foldernames )
mergeFreqFiles("twogramfreqs.csv",foldernames )
mergeFreqFiles("threegramfreqs.csv",foldernames )
mergeFreqFiles("fourgramfreqs.csv",foldernames )