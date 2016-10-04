# remove urls?? (not working properly)
# dont' remove punctuation
#? Replace numbers with a character ??? N
#? unknown words - first instance <UNK>

#clean up the raw text
cleanRawImport <- function(data) {
  library(tm)
  library(stringi)
  #library(RWeka)
  #library(slam)
  library(stylo); 
  #library(data.table)
  
  Encoding(data) <- "UTF-8"
  
  data <- stri_trans_general(data, "latin-ascii")
  
  #get rid of weird characters
  data <- gsub("[^[:graph:]]", " ", data)
  
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
  
  #return an entry per sentence
  data <- endOfSentence(data)
  
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
  
  data <- paste("<start> <start> <start>", data, sep=" ")
  
  
  data <- stripWhitespace(data)
  
  data
}

##get a coprus from the data
getCorp <- function(data){
  library(stylo)
  
  corp <- txt.to.words(data, splitting.rule="[[:space:]]")
 
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
  
  
  sent_token_annotator <- Maxent_Sent_Token_Annotator(language = "en")
  data <- as.String(paste(data,".\r\n",sep=""))
  a1 <- annotate(data, sent_token_annotator)
  data[a1]
}

#replace words with given value (e.g., replace profanity)
replaceWords <- function(replaceWords, newvalue, corp){
  
  v <- corp
  
  v[v %in% replaceWords] <- newvalue
  
  #turn the vector back into a list
  newCorp <- v
  names(newCorp) <- NULL      
  
  newCorp
}


#replace actual words with IDs
replaceWordsWithIDs <- function(v, dictionary=dictionary.by.word){
  
  dt <- data.table(word=v)
  
  dictionary[dt,on=c(word="word")]$wordID
  
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
  
  x <- d[dt, on=c(word="word")]

  x[is.na(x$wordID)]$word <- "<unk>"
  
  newCorp <- x$word
  names(newCorp) <- NULL
  
  
  newCorp
}