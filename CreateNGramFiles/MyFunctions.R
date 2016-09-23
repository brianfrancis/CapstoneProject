#? Replace numbers with a character ??? N
#? unknown words - first instance <UNK>

cleanRawImport <- function(data, forprediction=FALSE) {
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
  
  #remove space between proper names (only works for two word names)
  #data <- gsub("([A-Z][a-z]+)( )([A-Z])", "\\1\\3", data)
  
  ##replace a / or . with a space
  data <- gsub("/", " " ,data)
  
  ##change new york to newyork
  #data <- gsub("new york", "newyork", data)
  
  #change san diego san francisco et.c to sandiego sanfransico etc.
  #data <- gsub("san ", "san", data)
  
  #remove a single letter followed by a period (probably an initial)
  #gsub("( [A-Za-z])\\. ", " ", data)
  
  
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
  
  #plain text document
  data <- PlainTextDocument(data)$content
  
  # add leading characters to help predict if beginning of sentence
  data <- paste("<start> <start> <start> ", data)
  
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
  library(stylo)
  
  scorp <- lapply(data, txt.to.words, splitting.rule="[[:space:]]")
  
#!!!!!!! try keeping stop words and pronouns???  might be something we want to predict
  #delete pronouns
#  scorp <- lapply(scorp, delete.stop.words,
#                  stop.words = stylo.pronouns(language = "English"))
  
  #remove other very common and unhelpful words
#  scorp <- lapply(scorp, delete.stop.words,
#                  stop.words = c("the", "a", "an", "and", "but", "it"))
  
  # replace words with very similiar meaning (expand list to other "stop" words)
  
#!!!!!!!!!!!! this needs to be fixed (after test set processed??)
    #just take it out ??  not good for prediction anyway
#  f <- function(x) gsub("is|are|am","be",x)
#  scorp <- lapply(scorp, f)
  
#  f <- function(x) gsub("this|these|those","that",x)
#  scorp <- lapply(scorp, f)
  
}



sizeGB <- function(x){object.size(x)/1e+09}



