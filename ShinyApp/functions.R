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
  # #  last <- cleanRawImport(last)
  #   #create regex expression (starts with this phrase)
     last <- paste("", last, sep="")
  }
  last
}


predictNextWordKN <- function(input) {
  s <- Sys.time()
  
  partial <- getPartial(input)
  
  #s <- Sys.time()
  ##need to remove the last part of the phrase if there is a space ???
  x <- cleanInput(input)
  
  e <- Sys.time()
  print("clean")
  print (e-s)
  
  s <- Sys.time()
  
  
  l <- length(x)
  ##just keep the last maxngram - 1 words
  if (l > (highestngram-1)) {
    x <- x[(l-highestngram+2):l]
  }
  
  if (length(x) >=4) {kminus4 <- x[(length(x)-3)]}
  if (length(x) >=3) {kminus3 <- x[(length(x)-2)]}
  if (length(x) >=2) {kminus2 <- x[(length(x)-1)]}
  kminus1 <- x[length(x)]
  
  e <- Sys.time()
  print("initial")
  print (e-s)
  
  s <- Sys.time()
  
  sql <- character(0)
  
  if (l >= 4) {
    
    sql <- paste(sql, "SELECT word, p, ngramlevel FROM (SELECT p, 5 as ngramlevel, d.word as word
                 FROM FIVEGRAM as ng",
                 " INNER JOIN DICTIONARY as d",
                 " ON ng.prediction = d.wordID",
                 " WHERE ng.cond1=", kminus4, " AND ng.cond2=", kminus3,
                 " AND ng.cond3=", kminus2, " AND ng.cond4=", kminus1 ,
                 sep="")
    
    if (nchar(partial)>0){
      sql <- paste(sql," AND d.word like '", partial,"%'", sep="")
    }
    sql <- paste(sql," ORDER BY p DESC LIMIT 3) S0",sep="")
    sql <- paste(sql," UNION ", sep="")
  }
  
  
  if (l >= 3) {
    
      sql <- paste(sql, "SELECT word, p, ngramlevel FROM (SELECT p, 4 as ngramlevel, d.word as word
                   FROM FOURGRAM as ng",
                    " INNER JOIN DICTIONARY as d",
                    " ON ng.prediction = d.wordID",
                    " WHERE ng.cond1=", kminus3, " AND ng.cond2=", kminus2,
                   " AND ng.cond3=", kminus1, 
                    sep="")

      if (nchar(partial)>0){
        sql <- paste(sql," AND d.word like '", partial,"%'", sep="")
      }
      sql <- paste(sql," ORDER BY p DESC LIMIT 3) S1",sep="")
      sql <- paste(sql," UNION ", sep="")
  }
  
  if (l >= 2) {

    sql <- paste(sql, " SELECT word, p, ngramlevel FROM (SELECT p, 3 as ngramlevel, d.word as word
                 FROM THREEGRAM as ng",
                 " INNER JOIN DICTIONARY as d",
                 " ON ng.prediction = d.wordID",
                 " WHERE ng.cond1=", kminus2, " AND ng.cond2=", kminus1,
                 sep="")

    if (nchar(partial)>0){
      sql <- paste(sql," AND d.word like '", partial,"%'", sep="")
    }
    sql <- paste(sql," ORDER BY p DESC LIMIT 3) S2",sep="")
    sql <- paste(sql," UNION ", sep="")


  }

  if (l >= 1) {

    sql <- paste(sql, " SELECT word, p, ngramlevel FROM (SELECT p, 2 as ngramlevel, d.word as word
                 FROM TWOGRAM as ng",
                 " INNER JOIN DICTIONARY as d",
                 " ON ng.prediction = d.wordID",
                 " WHERE ng.cond1=", kminus1,
                 sep="")

    if (nchar(partial)>0){
      sql <- paste(sql," AND d.word like '", partial,"%'", sep="")
    }
    sql <- paste(sql," ORDER BY p DESC LIMIT 3) S3",sep="")
    sql <- paste(sql," UNION ", sep="")

  }

  sql <- paste(sql, " SELECT word, p, ngramlevel FROM (SELECT p, 1 as ngramlevel, d.word as word
               FROM ONEGRAM as ng",
               " INNER JOIN DICTIONARY as d",
               " ON ng.prediction = d.wordID",
               sep="")

  if (nchar(partial)>0){
    sql <- paste(sql," AND d.word like '", partial,"%'", sep="")
  }
  sql <- paste(sql," ORDER BY p DESC LIMIT 3) S4",sep="")
  sql <- paste(sql, " ORDER BY ngramlevel DESC, p DESC")

  sql <- paste("SELECT DISTINCT word FROM (", sql,
               ") as S LIMIT 3", sep="")

  
  e <- Sys.time()
  print("get allp")
  print (e-s)
  
  unlist(dbGetQuery(ngramdb,sql))
  
}


#trim whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#get last n characters of string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

