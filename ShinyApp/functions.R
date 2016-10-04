source("DataCleaning.R")

#clean input and return bag of words
cleanInput <- function (input){
  
  #take off anything not followed by a space (part of the prediction not the lookup)
  clean <- cleanRawImport(input)
  
  
  partial <- getPartial(clean)
  clean <- substr(clean, 1, nchar(clean) - nchar(partial))

  corp <- getCorp(clean)
  
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
  #  last <- cleanRawImport(last)
    #create regex expression (starts with this phrase)
    last <- paste("^", last, sep="")
  }
  last
}


predictNextWordKN <- function(input) {
  
  partial <- getPartial(input)
  
  s <- Sys.time()
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

  if (length(x) >=3) {kminus3 <- x[(length(x)-2)]}
  if (length(x) >=2) {kminus2 <- x[(length(x)-1)]}
  kminus1 <- x[length(x)]
  
  e <- Sys.time()
  print("initial")
  print (e-s)
  
  s <- Sys.time()
  
  
  if (l >= 3) {
     p1 <- fourgram.dt[cond1==kminus3 
                       & cond2==kminus2
                       & cond3==kminus1,
                       .(prediction, p)]
    
  }
  
  e <- Sys.time()
  print("p1")
  print (e-s)
  
  s <- Sys.time()
  
  
  if (l >= 2) {

    p2 <- threegram.dt[cond1==kminus2 
                       & cond2==kminus1,
                       .(prediction, p)]
  }
  
  e <- Sys.time()
  print("p2")
  print (e-s)
  
  s <- Sys.time()
  
  
  if (l >= 1) {
  
    p3 <- twogram.dt[cond1==kminus1,
                     .(prediction, p)]
  }

  e <- Sys.time()
  print("p3")
  print (e-s)
  
  s <- Sys.time()
  
  
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
  
  allp <- allp[order(-ngramlevel,-p)]
  
  e <- Sys.time()
  print("get allp")
  print (e-s)
  
  s <- Sys.time()
  
  predictions <- unique(allp$prediction)
  predictions <- replaceIDsWithWords(predictions)
  
   if (nchar(partial)>0){
     predictions <- predictions[grepl(partial, predictions)]
   }
  
  
  e <- Sys.time()
  print("done")
  print (e-s)
  
  predictions

}


#trim whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#get last n characters of string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

