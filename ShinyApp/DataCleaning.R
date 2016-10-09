#? number replacement not capturing all punctuation

cleanRawImport <- function(data) {
  library(tm)
  library(stringi)
  #library(RWeka)
  #library(slam)
  library(stylo); 
  #library(data.table)
  
#  Encoding(data) <- "UTF-8"
  
#  data <- stri_trans_general(data, "latin-ascii")
  
  #plain text document
#  data <- PlainTextDocument(data)$content
  data <- stripWhitespace(data)
  
  #get rid of weird characters
  data <- gsub("[^[:graph:]]", " ", data)
  
  #remove web sites
  data <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", data)
  
  #remove emails
  data <- gsub("[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", "", data, perl=TRUE)
  
  #remove twitter @ thingies
  data <- gsub("(?<=^|\\s)@\\S+","", data, perl=TRUE)
  
  
  data <- tail(as.vector(endOfSentence(data)),1)
  
  ##replace a / with a space
  data <- gsub("/+", " " ,data)
  
  ##remove punctuation excpet dashes or apostrophes or periods
  data <- gsub("[^[:alnum:][:space:][\\.'-]", "", data)
  
  #make first character in sentence lower case
  data <- gsub("^([A-Z])", "\\L\\1", data, perl=TRUE)
  
  #if first word is I or I'll, or I'd, etc. make it upper case
  data <- gsub("^(i[ '])", "\\U\\1", data, perl=TRUE)
  
  #replace numbers with <NUM>
  data <- gsub("[a-zA-Z0-9\\.\\'-]*[0-9][a-zA-Z0-9\\.\\'-]*","<NUM>",data)
  
  #clean up . - ' punctuation
  data <- gsub("\\.+", ".",data)
  data <- gsub("^[-\\.']+| [-\\.']+|[-']+ | [-\\.']+ ", " ", data)
  
  data <- gsub("[-\\.']$", "", data)
  
  data <- paste("<start> <start> <start> <start>", data, sep=" ")
  
  #plain text document
#  data <- PlainTextDocument(data)$content
  data <- stripWhitespace(data)
  
  #clean up . - ' punctuation
  data <- gsub("\\.+", ".",data)
  data <- gsub("^[-\\.']+| [-\\.']+|[-']+ | [-\\.']+ |[-\\.']$", " ", data)
  
  data
  
}


##get a coprus from the data
getCorp <- function(data){
  
  corp <- txt.to.words(data, splitting.rule="[[:space:]]", 
                  preserve.case=TRUE)
  
  corp <- replaceWords(profanity, newvalue = "<profanity>",
                       corp = corp)  
 
   #replace out of vocabulary words with unk
  corp <- replaceOOVWords(corp)
  
  #replace words with word IDs
  corp <- replaceWordsWithIDs(corp)
  
  corp
  
}

#parse sentences
endOfSentence <- function(data){
  library(openNLP)
  library(NLP)
  library(stringr)
  
  # set in intialization instead
#  sent_token_annotator <- Maxent_Sent_Token_Annotator(language = "en")
  
  #make sure each document ends in a single period
  if (nchar(data)==0){
    x <- data
  } else{
    data <- paste(data,".",sep="")
    data <- gsub("[\\.\\?!]+$",".",data)
    
    data <- as.String(paste(data,"",sep=""))
    a1 <- annotate(data, sent_token_annotator)
    x <- as.vector(data[a1])
  }
  x
}

#replace words with given value (e.g., replace profanity)
replaceWords <- function(replaceWords, newvalue, corp){
  
  v <- corp
  
  v[tolower(v) %in% replaceWords] <- newvalue
  
  #turn the vector back into a list
  #newCorp <- v
  #names(newCorp) <- NULL      
  
  #newCorp
  v
}


#replace actual words with IDs
replaceWordsWithIDs <- function(v, dictionary=dictionary.by.word){
  
  dt <- data.table(word=v)
  
  dictionary[word %in% v][dt,on=c(word="word")]$wordID
  
}

#replace word IDs with actual words
replaceIDsWithWords <- function(v, dictionary=dictionary.by.id){
 
  dt <- data.table(wordID=v)

  dictionary[dt,on=c(wordID="wordID")]$word
  
}

#replace out of vocabulary words with UNK
replaceOOVWords <- function(corp, d=dictionary.by.word) {
  #vector
  v <- corp
  
  dt <- data.table(v)
  setnames(dt,"v","word")
  
  x <- d[word %in% v][dt, on=c(word="word")]
  
  x[is.na(x$wordID), word:="<unk>"]
  
  #newCorp <- x$word
  #names(newCorp) <- NULL

  #newCorp
  x$word
}