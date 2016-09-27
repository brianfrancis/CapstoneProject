# U.S. turning in us ?????
# how to handle abbreviations?????
# sentence detection may also help.

# /S /???  - remove individual characters other than a / i ???
# change U.S. to United States and U.S.A. to United States


# library(tm)
# library(stringi)
# library(RWeka)
# library(slam)
# library(stylo); 
# library(data.table)
# 
# 
# 
# foldernames <- c("train1", "train2", "train3", "train4", "train5"
#                  , "train6", "train7", "train8", "test", "validation")
# 

#code to check unigrams (look for weird words)

 fourgram <- fread("case4/train/fourgramfreq.csv")
 dictionary <- fread("case4/dictionary/dictionary.csv")
 setkey(dictionary,wordID)

  setnames(dictionary,"word","cond1")
 setkey(fourgram,cond1)
 x <- fourgram[dictionary]
 
 setnames(dictionary,"cond1","prediction")
 setkey(x,prediction)
 x <- x[dictionary]
 x <- x[order(-freq)]


setwd("C:/Users/bfrancis/Desktop/Coursera/Capstone/CapstoneProject/CreateNGramFiles")


# take a raw data file and append to ngram freqs (val and training)
  # make sure to set intial.run = TRUE if this is the first data being processed and
  # we're not appending to an previously processed ngrams
rawDataToNgramFreqs <- function(rawdata.folder, rawdata.filename, 
                                train.folder="train",test.folder="test",
                                val.folder="validation",
                                dictionary.folder="dictionary", 
                                dictionary.filename="dictionary.csv",
                                nbrlines = -1L, initial.run=FALSE)
{
  

  #set the seed
  set.seed(123)

  source("MyFunctions.R")
  source("CreateDictionary.R")
  source("CreateProbFiles.R")
  
  con <- file(paste(rawdata.folder,rawdata.filename,sep="/"), "rb", encoding="UTF-8")
  rawdata <- readLines(con, n=nbrlines)
  close(con)
  i <-  sample(rep(1:10),size=length(rawdata), replace=TRUE)


  for (j in 1:10) {
    
    if (j < 9) {
      foldername <- train.folder
    } else if (j==9) {
      foldername <- test.folder
    } else if (j==10) {
      foldername <- val.folder
    } else {
      stop("j not valid")
    }

    rawsub <- rawdata[i==j]
    
    processeddata <- cleanRawImport(rawsub)
    rm(rawsub)
    
    s <- Sys.time()
  
    wordCorp <- getCorp(processeddata)
    rm(processeddata)
   
     
    # check if this is the first time we're running anything - if so over-write files
    # otherwise we append to the existing files
    if (j==1 && initial.run==TRUE) initialfile <- TRUE else initialfile <- FALSE
    
    # what to do if not training data ???
    dictionary <- appendToDictionary(wordCorp, dictionary.filename, 
                                     dictionary.folder, initialfile)
    
    idCorp <- replaceWordsWithIDs(wordCorp, dictionary)
    rm(wordCorp, dictionary)
    gc()
    
    #initial ngram creation if intial run and j = 1 (first train) or 9 / 10 (first train / validation)
    if ((j==1 || j > 8)  && initial.run==TRUE) initialfile <- TRUE else initialfile <- FALSE
    
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
    
    e <- Sys.time()
    print('tiime to go from processed data to ngrams')
    print(e-s)
    

  }
}



setwd("C:/Users/bfrancis/Desktop/Coursera/Capstone/CapstoneProject/CreateNGramFiles")

case <- "case4"

trainfolder <- paste(case,"train", sep="/")
testfolder <- paste(case,"test", sep="/")
valfolder <- paste(case,"validation", sep="/")
dictfolder <- paste(case,"dictionary", sep="/")

rawDataToNgramFreqs(rawdata.folder = "C:/Users/bfrancis/Desktop/Coursera/Capstone/Coursera-SwiftKey/final/en_US",
                    rawdata.file = "en_US.blogs.txt",
                    nbrlines = 10000,
                    initial.run = TRUE,
                    train.folder=trainfolder,test.folder=testfolder,
                    val.folder=valfolder,
                    dictionary.folder=dictfolder)

rawDataToNgramFreqs(rawdata.folder = "C:/Users/bfrancis/Desktop/Coursera/Capstone/Coursera-SwiftKey/final/en_US",
                    rawdata.file = "en_US.news.txt",
                    nbrlines = 10000,
                    train.folder=trainfolder,test.folder=testfolder,
                    val.folder=valfolder,
                    dictionary.folder=dictfolder)

rawDataToNgramFreqs(rawdata.folder = "C:/Users/bfrancis/Desktop/Coursera/Capstone/Coursera-SwiftKey/final/en_US",
                    rawdata.file = "en_US.twitter.txt",
                    nbrlines = 10000,
                    train.folder=trainfolder,test.folder=testfolder,
                    val.folder=valfolder,
                    dictionary.folder = dictfolder)


source("CreateProbFiles.R")

createProbFiles(trainfolder)
