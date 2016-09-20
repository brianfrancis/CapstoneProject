

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
  
  # add end of document characters so we have a full n-gram for every word
  #don't do this when predicting
  if (forprediction==FALSE) {
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


cleanInput <- function (input){
  #take off anything not followed by a space (part of the prediction not the lookup)
  partial <- getPartial(input)
  input <- substr(input, 1, nchar(input) - nchar(partial))

  clean <- cleanRawImport(input, forprediction = TRUE)
  corp <- unlist(getCorp(clean))
  dictionary <- onegram.dt$prediction
  corp <-  corp[corp %in% dictionary]
  output <- paste(corp, sep="", collapse=" ")
  output
  #remove stop words
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
  
  x <- unlist(strsplit(x, " "))
  l <- length(x)
  ##just keep the last maxngram - 1 words
  if (l > (highestngram-1)) {
    x <- x[(l-highestngram+2):l]
  }
  
  p1 <- data.table()
  p2 <- data.table()
  p3 <- data.table()
  p4 <- data.table()
  for (i in length(x):0) {
    
    if (i == 3) {
  
      p1 <- fourgram.dt[lookup==paste(x, collapse=" ") ,
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
      p4 <- onegram.dt[,.(prediction, p)]
      
    }
    
  }
  
  
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
  
  if (nchar(partial)>0){
    setkey(allp, "prediction")
    allp <- allp[grepl(partial, prediction),]
  }
  
  setkey(allp, "ngramlevel", "p")
  #allp[,maxngramlevel:= max(ngramlevel), by=prediction]
  #allp <- allp[ngramlevel==maxngramlevel,]
  allp <- allp[order(-ngramlevel,-p)]
  
  e <- Sys.time()
  print("done")
  print (e-s)
  
  
  unique(allp$prediction)
  
}

#trim whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#get last n characters of string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

