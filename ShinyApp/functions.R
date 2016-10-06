source("DataCleaning.R")

#clean input and return bag of words
cleanInput <- function (input){
  
  #take off anything not followed by a space (part of the prediction not the lookup)
  clean <- tail(cleanRawImport(input),1)
  
  
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
  s <- Sys.time()
  
  partial <- getPartial(input)
  
  #s <- Sys.time()
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
  
  allp <- data.table()
  
  
  if (l >= 3) {
    sql <- paste("SELECT prediction, p, 4 as ngramlevel FROM FOURGRAM
                 WHERE cond1=", kminus3, " AND cond2=", kminus2,
                 " AND cond3=", kminus1, sep="") 
    allp <- dbGetQuery(ngramdb,sql)
    
  }
  
  e <- Sys.time()
  print("p1")
  print (e-s)
  
  s <- Sys.time()
  
  
  if (l >= 2) {

    sql <- paste("SELECT prediction, p, 3 as ngramlevel FROM THREEGRAM
                 WHERE cond1=", kminus2, " AND cond2=", kminus1,
                 sep="") 
    allp <- rbind(allp,dbGetQuery(ngramdb,sql))
    
  }
  
  e <- Sys.time()
  print("p2")
  print (e-s)
  
  s <- Sys.time()
  
  
  if (l >= 1) {
  
    sql <- paste("SELECT prediction, p, 2 as ngramlevel FROM TWOGRAM
                 WHERE cond1=", kminus1,
                 sep="") 
    allp <- rbind(allp,dbGetQuery(ngramdb,sql))
                  
  }

  e <- Sys.time()
  print("p3")
  print (e-s)
  
  s <- Sys.time()
  
  onegram.dt[,ngramlevel:=1]
  allp <- rbind(allp,onegram.dt[,.(prediction, p,ngramlevel)])
  #setnames(allp,"V3","ngramlevel")

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
  
  head(predictions,3)

}


#trim whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#get last n characters of string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

