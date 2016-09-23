

cleanRawImport <- function(data, forprediction=FALSE) {
  library(tm)
  library(stylo); 
  
  data <- tolower(data)
  
  #remove web sites
  data <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", data)
  
  data <- gsub("(?<=^|\\s)#\\S+","", data, perl=TRUE)
  
  #remove emails
  data <- gsub("[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", "", data, perl=TRUE)
  
  #remove numbers
  data <- removeNumbers(data)
  
  ##replace a / or . with a space
  data <- gsub("/", " " ,data)
  
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
  data <- gsub("([a-z])('ll)", "\\1 will", data)
  
  # remove apostrophe s
  data <- gsub("([a-z])('s)", "\\1", data)
  
  #data <- gsub("usa", "united states")  - replace in corp ???
  data <- gsub("u.s.a.", "united states", data)
  data <- gsub("u.s.", "united states", data)
  
  
  ##remove punctuation excpet dashes or apostrophes
  data <- gsub("[^[:alnum:][:space:]-]", "", data)
  
  ##remove a leading or trailing - 
  # remove repeated dashes
  data <- gsub("-+", "-", data)
  data <- gsub(" -|- ", " ", data)
  
  #replace any remaining dash with a space
  data <- gsub("-", " ", data)
  
  
  if (forprediction==FALSE){
    # add end of document characters so we have a full n-gram for every word
    data <- paste(data, " <end> <end> <end>")
  }
  
  data <- stripWhitespace(data)
  
  data
  
}

getCorp <- function(data){
  
  scorp <- lapply(data, txt.to.words, splitting.rule="[[:space:]]")
  
}


cleanInput <- function (input){
  #take off anything not followed by a space (part of the prediction not the lookup)
  partial <- getPartial(input)
  input <- substr(input, 1, nchar(input) - nchar(partial))

  clean <- cleanRawImport(input, forprediction = TRUE)
  
  # add leading characters to help predict if beginning of sentence
  clean <- paste("<start> <start> <start> ", clean)
  
  corp <- unlist(getCorp(clean))
  corp <- replaceWordsWithIDs(corp, dictionary)
  
  #remove words not in dictionary (change to UNK them instread???)
  corp <-  corp[corp %in% dictionary$wordID]
  corp
}

#get part of the word we're trying to predict in case the user
#started typing it
getPartial <- function(input) {
  if (substrRight(input,1)==" ") {
    last <- ""
  } else {
    words <- unlist(strsplit(input, split=" "))
    last <- words[length(words)]
    last <- cleanRawImport(last, forprediction=TRUE)
    #create regex expression (starts with this phrase)
    last <- paste("^", last, sep="")
  }
  last
}


predictNextWordKN <- function(input) {
  s <- Sys.time()
  
  partial <- getPartial(input)
  
  ##need to remove the last part of the phrase if there is a space ???
  x <- cleanInput(input)
  

  l <- length(x)
  ##just keep the last maxngram - 1 words
  if (l > (highestngram-1)) {
    x <- x[(l-highestngram+2):l]
  }
  
  p1 <- data.table()
  p2 <- data.table()
  p3 <- data.table()
  p4 <- data.table()
  
    
  if (l == 3) {

    p1 <- fourgram.dt[cond1==x[(length(x)-2)] & cond2==x[(length(x)-1)] & cond3==x[length(x)] ,
                      .(prediction, p)]
  }
  if (l >= 2) {
    
    p2 <- threegram.dt[cond1==x[(length(x)-1)] & cond2==x[length(x)],
                       .(prediction, p)]
  }
  if (l >= 1) {
    p3 <- twogram.dt[cond1==x[length(x)],
                     .(prediction, p)]
  }
  p4 <- onegram.dt[,.(prediction, p)]
  
  allp <- data.table()   
  
  #backoff in case our condition doesn't exist in higher order n-gram
  if (nrow(p1) > 0) {
    p1[,ngramlevel := 4]
    allp <- p1
    
  } 
  if (nrow(p2) > 0) {
    p2[,ngramlevel := 3]
    allp <- rbind(allp,p2)
    
  } 
  if (nrow(p3) > 0) {
    p3[,ngramlevel := 2]
    allp <- rbind(allp,p3)
  } 
  p4[,ngramlevel := 1]
  allp <- rbind(allp,p4)
  allp <- data.table(allp)
  
  #setkey(allp, "ngramlevel", "p")
  #allp[,maxngramlevel:= max(ngramlevel), by=prediction]
  #allp <- allp[ngramlevel==maxngramlevel,]
  allp <- allp[order(-ngramlevel,-p)]
  
  e <- Sys.time()
  print("done")
  print (e-s)
  
  
  predictions <- unique(allp$prediction)
  
  predictions <- replaceIDsWithWords(predictions, dictionary)
  
  if (nchar(partial)>0){
    predictions <- predictions[grepl(partial, predictions)]
  }
  
  predictions
}

#trim whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#get last n characters of string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}



replaceWordsWithIDs <- function(v, dictionary){
  idlookup <- as.vector(dictionary$wordID)
  names(idlookup) <- dictionary$word
  
  #replace words with ids
  v <- idlookup[v]
  names(v) <- NULL
  
  v
}

replaceIDsWithWords <- function(v, dictionary){
  wordlookup <- as.vector(dictionary$word)
  names(wordlookup) <- dictionary$wordID
  
  #replace words with ids
  v <- wordlookup[v]
  names(v) <- NULL
  
  v
}