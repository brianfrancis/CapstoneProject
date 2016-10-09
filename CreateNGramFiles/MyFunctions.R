#? number replacement not capturing all punctuation

cleanRawImport <- function(data) {
  library(tm)
  library(stringi)
  #library(RWeka)
  #library(slam)
  library(stylo); 
  #library(data.table)
  
  Encoding(data) <- "UTF-8"
  
  data <- stri_trans_general(data, "latin-ascii")
  
  #plain text document
  data <- PlainTextDocument(data)$content
  data <- stripWhitespace(data)
  
  #get rid of weird characters
  data <- gsub("[^[:graph:]]", " ", data)
  
  #remove web sites
  data <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", data)
  
  #remove emails
  data <- gsub("[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", "", data, perl=TRUE)
  
  #remove twitter @ thingies
  data <- gsub("(?<=^|\\s)@\\S+","", data, perl=TRUE)
  
  
  data <- endOfSentence(data)

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
  data <- gsub("^[-\\.']+| [-\\.']+|[-']+ | [-\\.']+ |[-\\.']$", " ", data)
  
  
  data <- paste("<start> <start> <start> <start>", data, sep=" ")
  
  #plain text document
  data <- PlainTextDocument(data)$content
  data <- stripWhitespace(data)
  
  #clean up . - ' punctuation
  data <- gsub("\\.+", ".",data)
  data <- gsub("^[-\\.']+| [-\\.']+|[-']+ | [-\\.']+ |[-\\.']$", " ", data)
  
  data
  
}

getCorp <- function(data){
  library(stylo)
  
  scorp <- lapply(data, txt.to.words, splitting.rule="[[:space:]]", 
                  preserve.case=TRUE)

  profanitypath <- "profanity.txt"
  scorp <- replaceWords(wordListPath = profanitypath, newvalue = "<profanity>",
                        corp = scorp)  

}


endOfSentence <- function(data){
  library(openNLP)
  library(NLP)
  library(stringr)
  
  
  sent_token_annotator <- Maxent_Sent_Token_Annotator(language = "en")
  
  #make sure each document ends in a single period
  data <- paste(data,".",sep="")
  data <- gsub("[\\.\\?!]+$",".",data)
  
  data <- as.String(paste(data,"",sep=""))
  a1 <- annotate(data, sent_token_annotator)
  data[a1]
}

replaceWords <- function(wordListPath, newvalue, corp){
  library(data.table)  
  
  replaceWords <- unlist(read.table(wordListPath,header=TRUE, stringsAsFactors = FALSE))
    
  v <- unlist(corp)
  #get indices to recreate list from big vector
  f <- rep(1:length(corp),sapply(corp,length))
  
  v[tolower(v) %in% replaceWords] <- newvalue
    
  #turn the vector back into a list
  newCorp <- split(v,f)
  names(newCorp) <- NULL      
    
  newCorp
}


