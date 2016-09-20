#? Replace numbers with a character ??? N
#? unknown words - first instance <UNK>

cleanRawImport <- function(data, forprediction=FALSE) {
  Encoding(data) <- "UTF-8"
  
  #get rid of weird characters
  data <- gsub("[^[:graph:]]", " ", data)
  
  data <- stri_trans_general(data, "latin-ascii")
  
  #remove numbers
  data <- removeNumbers(data)
  
  #remove space between proper names (only works for two word names)
  data <- gsub("([A-Z][a-z]+)( )([A-Z])", "\\1\\3", data)
  
  data <- tolower(data)
  
  ##replace a / or . with a space
  data <- gsub("/", " " ,data)
  
  ##change new york to newyork
  data <- gsub("new york", "newyork", data)
  #change san diego san francisco et.c to sandiego sanfransico etc.
  data <- gsub("san ", "san", data)
  
  #remove a single letter followed by a period (probably an initial)
  gsub("( [A-Za-z])\\. ", " ", data)
  
  
  # fix contractions
  data <- gsub("can't|can not", "cannot", data)
  data <- gsub("shan't", "should not", data)
  data <- gsub("won't", "will not", data)
  #he or she
  data <- gsub("he's", "he is", data)
  data <- gsub("it's", "it is", data)
  data <- gsub("let's", "let us", data)
  data <- gsub("who's", "who is", data)
  data <- gsub("what's", "what is", data)
  data <- gsub("that's", "that is", data)
  #here or there or where
  data <- gsub("here's", "here is", data)
  data <- gsub("when's", "when is", data)
  data <- gsub("who's", "who is", data)
  data <- gsub("how's", "how is", data)
  data <- gsub("([a-z])(n't)", "\\1 not", data)
  data <- gsub("([a-z])('d)", "\\1 would", data)
  data <- gsub("([a-z])('re)", "\\1 are", data)
  data <- gsub("([a-z])('m)", "\\1 am", data)
  data <- gsub("([a-z])('ve)", "\\1 have", data)
  
  # remove apostrophe s
  data <- gsub("([a-z])('s)", "\\1", data)
  
  
  ##remove punctuation excpet dashes or apostrophes
  data <- gsub("[^[:alnum:][:space:]-]", "", data)
  
  ##remove a leading or trailing - 
  # remove repeated dashes
  data <- gsub("-+", "-", data)
  data <- gsub(" -|- ", " ", data)
  
  #replace any remaining dash with a space
  data <- gsub("-", " ", data)
  
  #plain text document
  data <- PlainTextDocument(data)$content
  
  if (forprediction==FALSE){
    # add end of document characters so we have a full n-gram for every word
    data <- paste(data, " <end> <end> <end>")
  }

  data <- stripWhitespace(data)
  
  #stem ?? - do or don't
 # data <- stemDocument(data)
  
  data
  
}

getCorp <- function(data){
  
  scorp <- lapply(data, txt.to.words, splitting.rule="[[:space:]]")
  
  #delete pronouns
  scorp <- lapply(scorp, delete.stop.words,
                  stop.words = stylo.pronouns(language = "English"))
  
  #remove other very common and unhelpful words
  scorp <- lapply(scorp, delete.stop.words,
                  stop.words = c("the", "a", "an", "and", "but", "it"))
  
  # replace words with very similiar meaning (expand list to other "stop" words)
  
#!!!!!!!!!!!! this needs to be fixed (after test set processed??)
  f <- function(x) gsub("is|are|am","be",x)
  scorp <- lapply(scorp, f)
  
  f <- function(x) gsub("this|these|those","that",x)
  scorp <- lapply(scorp, f)
  
}



createNgramFiles <- function(scorp, foldername, percov=.99, isTraining=FALSE) {
  
  
  unigramfreq <- make.frequency.list(scorp, value=TRUE, relative=FALSE)
  
#  if (isTraining){
#    dictionary <- read.csv(dictionaryfile)
    
#    removewords <- unigramfreq$data[-(unigram$data %in% dictionary$data)]
#    scorp <- delete.stop.words(scorp, stopwords=removewords)
#  }    
  
  unigramfreq <- make.frequency.list(scorp, value=TRUE, relative=FALSE)
  
  write.csv(unigramfreq, paste(foldername, "onegramfreqs.csv", sep="/"))
  rm(unigramfreq)
  gc()
  
  x <- sapply(scorp, length)
  
  my.word.pairs <- lapply(scorp[x>1], txt.to.features, ngram.size=2)
  bigramfreq <- make.frequency.list(my.word.pairs, value=TRUE, relative=FALSE)
  rm(my.word.pairs)
  gc
  write.csv(bigramfreq, paste(foldername, "twogramfreqs.csv", sep="/"))
  rm(bigramfreq)
  gc()
  
  my.word.triplets <- lapply(scorp[x>2], txt.to.features, ngram.size=3)
  trigramfreq <- make.frequency.list(my.word.triplets, value=TRUE, relative=FALSE)
  rm(my.word.triplets)
  gc()
  write.csv(trigramfreq, paste(foldername, "threegramfreqs.csv", sep="/"))
  rm(trigramfreq)
  gc()
  
  #stick to trigrams for now
   my.word.quad <- lapply(scorp[x>3], txt.to.features, ngram.size=4)
   rm(scorp)
   gc()
   
   quadgramfreq <- make.frequency.list(my.word.quad, value=TRUE, relative=FALSE)
   rm(my.word.quad)
   gc()
   write.csv(quadgramfreq, paste(foldername, "fourgramfreqs.csv", sep="/"))
   rm(quadgramfreq)
   gc()
  
}


smoothP <- function(x1, x2,  x3 ,x4, discount=.75){
  
  firstterm <- pmax((x1 - discount),0)/x2
  secondterm <- (discount/x2)*x3
  thirdterm <- x4
  p <- firstterm + (secondterm*thirdterm)
  
  data.table(p)
  
}


createNGramFreqsFromRawData <- function(foldername, rawdata) {
  
  processeddata <- cleanRawImport(rawdata)
  rm(rawdata)
  
  corp <- getCorp(processeddata)
  rm(processeddata)
  
  createNgramFiles(corp, foldername)
  rm(corp)

}

