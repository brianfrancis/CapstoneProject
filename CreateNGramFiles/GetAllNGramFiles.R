setwd("C:/Users/bfrancis/Desktop/Coursera/Capstone/CapstoneProject/CreateNGramFiles")


# take a raw data file and append to ngram freqs (val and training)
  # make sure to set intial.run = TRUE if this is the first data being processed and
  # we're not appending to an previously processed ngrams
rawDataToNgramFreqs.Train <- function(rawdata.folder, rawdata.filename, 
                                      train.folder="train",
                                      dictionary.folder="dictionary", 
                                      dictionary.filename="dictionary.csv",
                                      nbrlines = -1L, initial.run=FALSE)
{
  library(data.table)
  
  #set the seed
  set.seed(123)
  
  source("MyFunctions.R")
  source("CreateDictionary.R")
  source("CreateProbFiles.R")
  
  con <- file(paste(rawdata.folder,rawdata.filename,sep="/"), "rb", encoding="UTF-8")
  rawdata <- readLines(con, n=nbrlines)
  close(con)
  i <-  sample(rep(1:100),size=length(rawdata), replace=TRUE)
  
  dictionary <- data.table()
    
  for (j in 1:80) {
    print(j)
      
    foldername <- train.folder
    
    rawsub <- rawdata[i==j]
    
    s <- Sys.time()
    processeddata <- cleanRawImport(rawsub)
    e <- Sys.time()
    print(paste(e-s,"Time to process raw data", sep=" "))
    
    rm(rawsub)
    
    s <- Sys.time()
    wordCorp <- getCorp(processeddata)
    e <- Sys.time()
    print(paste(e-s,"Time to get corp", sep=" "))
    
    rm(processeddata)
    
    
    # check if this is the first time we're running anything - if so over-write files
    # otherwise we append to the existing files
    if (j==1 && initial.run==TRUE) initialfile <- TRUE else initialfile <- FALSE
    
    
    olddictionary <- dictionary
    
    
    # what to do if not training data ???
    dictionary <- appendToDictionary(wordCorp, dictionary.filename, 
                                     dictionary.folder, initialfile)
    
    s <- Sys.time()
    wordCorp <- replaceFirstWordWithUNK(olddictionary,dictionary,wordCorp)
    e <- Sys.time()
    print(paste(e-s,"Time to repalce UNK", sep=" "))
    
    s <- Sys.time()
    idCorp <- replaceWordsWithIDs(wordCorp, dictionary)
    e <- Sys.time()
    print(paste(e-s,"Time to repalce words with IDs", sep=" "))
    
    rm(wordCorp)
    gc()
    

    appendToNgram(idCorp, filename="onegramfreq.csv", foldername=foldername, 
                  initialfile, ngramsize=1)
    
    appendToNgram(idCorp, filename="twogramfreq.csv", foldername=foldername, 
                  initialfile, ngramsize=2)
    
    appendToNgram(idCorp, filename="threegramfreq.csv", foldername=foldername, 
                  initialfile, ngramsize=3)
    
    appendToNgram(idCorp, filename="fourgramfreq.csv", foldername=foldername, 
                  initialfile, ngramsize=4)
    
    rm(idCorp)
    gc()
    
  }
}

rawDataToNgramFreqs.Test <- function(rawdata.folder, rawdata.filename, 
                                      test.folder="test",
                                      val.folder="validation",
                                      dictionary.folder="dictionary", 
                                      dictionary.filename="dictionary.csv",
                                      nbrlines = -1L, initial.run=FALSE)
{
  library(data.table)  
  
  #set the seed
  set.seed(123)
  
  source("MyFunctions.R")
  source("CreateDictionary.R")
  source("CreateProbFiles.R")
  
  con <- file(paste(rawdata.folder,rawdata.filename,sep="/"), "rb", encoding="UTF-8")
  rawdata <- readLines(con, n=nbrlines)
  close(con)
  i <-  sample(rep(1:100),size=length(rawdata), replace=TRUE)
  
  dictionary <- data.table()
  
  for (j in 81:100) {
    print(j)
    if (j<90) foldername <- test.folder else foldername <- val.folder
    
    rawsub <- rawdata[i==j]
    
    processeddata <- cleanRawImport(rawsub)
    rm(rawsub)
    
    wordCorp <- getCorp(processeddata)
    rm(processeddata)
    
    dictionary <- fread(paste(dictionary.folder, dictionary.filename, sep="/"))
    
    # what to do if not training data ???
    wordCorp <- replaceOOVWords(wordCorp, dictionary)
    
    idCorp <- replaceWordsWithIDs(wordCorp, dictionary)
    
    rm(wordCorp)
    gc()
    
    #initial ngram creation if intial run and j = 1 (first train) or 9 / 10 (first train / validation)
    if (initial.run==TRUE) initialfile <- TRUE else initialfile <- FALSE
    
    appendToNgram(idCorp, filename="onegramfreq.csv", foldername=foldername, 
                  initialfile, ngramsize=1)
    
    appendToNgram(idCorp, filename="twogramfreq.csv", foldername=foldername, 
                  initialfile, ngramsize=2)
    
    appendToNgram(idCorp, filename="threegramfreq.csv", foldername=foldername, 
                  initialfile, ngramsize=3)
    
    appendToNgram(idCorp, filename="fourgramfreq.csv", foldername=foldername, 
                  initialfile, ngramsize=4)
    
    rm(idCorp)
    gc()
    
  }
}



setwd("C:/Users/bfrancis/Desktop/Coursera/Capstone/CapstoneProject/CreateNGramFiles")

case <- "all"

trainfolder <- paste(case,"train", sep="/")
testfolder <- paste(case,"test", sep="/")
valfolder <- paste(case,"validation", sep="/")
dictfolder <- paste(case,"dictionary", sep="/")
nbr_rows <- -1L

rawDataToNgramFreqs.Train(rawdata.folder = "C:/Users/bfrancis/Desktop/Coursera/Capstone/Coursera-SwiftKey/final/en_US",
                    rawdata.file = "en_US.blogs.txt",
                    nbrlines = nbr_rows,
                    initial.run = TRUE,
                    train.folder=trainfolder,
                    dictionary.folder=dictfolder)

rawDataToNgramFreqs.Train(rawdata.folder = "C:/Users/bfrancis/Desktop/Coursera/Capstone/Coursera-SwiftKey/final/en_US",
                    rawdata.file = "en_US.news.txt",
                    nbrlines = nbr_rows,
                    train.folder=trainfolder,
                    dictionary.folder=dictfolder)

rawDataToNgramFreqs.Train(rawdata.folder = "C:/Users/bfrancis/Desktop/Coursera/Capstone/Coursera-SwiftKey/final/en_US",
                    rawdata.file = "en_US.twitter.txt",
                    nbrlines = nbr_rows,
                    train.folder=trainfolder,
                    dictionary.folder = dictfolder)


source("CreateProbFiles.R")

createProbFiles(trainfolder)

rawDataToNgramFreqs.Test(rawdata.folder = "C:/Users/bfrancis/Desktop/Coursera/Capstone/Coursera-SwiftKey/final/en_US",
                        rawdata.file = "en_US.blogs.txt",
                        nbrlines = nbr_rows,
                        initial.run = TRUE,
                        test.folder=testfolder,
                        val.folder = valfolder,
                        dictionary.folder=dictfolder)

rawDataToNgramFreqs.Test(rawdata.folder = "C:/Users/bfrancis/Desktop/Coursera/Capstone/Coursera-SwiftKey/final/en_US",
                         rawdata.file = "en_US.news.txt",
                         nbrlines = nbr_rows,
                         initial.run = FALSE,
                         test.folder=testfolder,
                         val.folder = valfolder,
                         dictionary.folder=dictfolder)

rawDataToNgramFreqs.Test(rawdata.folder = "C:/Users/bfrancis/Desktop/Coursera/Capstone/Coursera-SwiftKey/final/en_US",
                         rawdata.file = "en_US.twitter.txt",
                         nbrlines = nbr_rows,
                         initial.run = FALSE,
                         test.folder=testfolder,
                         val.folder = valfolder,
                         dictionary.folder=dictfolder)

