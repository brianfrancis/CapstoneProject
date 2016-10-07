

appendToDictionary <- function(corp, 
                               dict.filename, dict.foldername,
                               initialfile){
 
  library(data.table)
  library(stylo)
  
  words <- data.table(make.frequency.list(corp))
  setnames(words,"V1", "word")
  setkey(words,word)
  #setnames(unigramfreq, "N", "freq")
  
  #setnames ??
  
  
  ##check if this is the first file for the dictionary
  ##if not bring in the existing info and get the next word ID
  ##otherwise next word ID = 1
  if (initialfile == FALSE){
    dictionary <- fread(paste(dict.foldername, dict.filename, sep="/"))
    setkey(dictionary,word)
    nextindex <- max(dictionary$wordID) + 1
    
    # get words not in dictionary
    new.words <- dictionary[words,on=c(word="word")]
    new.words <- new.words[is.na(wordID),]
    new.words[,wordID := seq(nextindex, (nextindex-1+nrow(new.words)))]
    dictionary <- rbind(dictionary, new.words)
    
  } else
  { 
    nextindex <- 1
    
    #add <unk> to the dictionary
    words <- rbind(words,list("<unk>"))
    words <- words[order(word)]
    
    words[,wordID := seq(1, nrow(words))]
    dictionary <- words
  }
  
  
  write.csv(dictionary, paste(dict.foldername, dict.filename, sep="/"),row.names=FALSE)

  
  dictionary  
}

replaceWordsWithIDs <- function(corp, dictionary){
  wordlookup <- as.vector(dictionary$wordID)
  names(wordlookup) <- dictionary$word
  
  #turn the list into a big vector and replace the word with the word ID
  v <- unlist(corp)
  v <- wordlookup[v]
  names(v) <- NULL
  
  #get indices to recreate list from big vector
  f <- rep(1:length(corp),sapply(corp,length))

  #turn the vector back into a list
  newCorp <- split(v,f)
  names(newCorp) <- NULL      
  
  newCorp
  
}

appendToNgram <- function(corp, filename, foldername,
                               initialfile, ngramsize){
  
  library(data.table)
  library(stylo)
  
  x <- sapply(corp, length)
  
  ngrams <- lapply(corp[x>(ngramsize-1)], txt.to.features, ngram.size=ngramsize)
  new.ngramfreq <- data.table(make.frequency.list(ngrams, value=TRUE, relative=FALSE))
  rm(ngrams)
  gc()
  setnames(new.ngramfreq,"data", "wordID")
  setnames(new.ngramfreq, "N", "freq")
  
  #split the ngram into a column per wordID
  ids <- new.ngramfreq[, tstrsplit(wordID, " ", fixed=TRUE)]
  #rename the columns
  for(i in 1:ncol(ids)){
    if (i < ncol(ids)){
      setnames(ids,i,paste("cond",i, sep=""))
      
    } else
    {setnames(ids,i,"prediction")}
  }
  #change the wordIDs from character to integer (huge memory savings) 
  cols <- names(ids)
  ids[,(cols):=lapply(.SD,as.integer),.SDcols=cols]
  
  #link it all back together
  new.ngramfreq <- cbind(ids, new.ngramfreq)
  rm(ids, cols)
  new.ngramfreq[,wordID:=NULL]
  
  ##check if this is the first file for the dictionary
  ##if not bring in the existing info and get the next word ID
  ##otherwise next word ID = 1
  if (initialfile == FALSE){
    repo.ngramfreq <- readRDS(paste(foldername, filename, sep="/"))
    combined <- rbind(repo.ngramfreq, new.ngramfreq)
    rm(repo.ngramfreq,new.ngramfreq)
    
    #get the columns we'll group by (all expect freq)
    bycols <- names(combined)[names(combined)!="freq"]
   
    setkeyv(combined, bycols)
    repo.ngramfreq <- combined[, .(sum(freq)), by=bycols]
    rm(combined)
    setnames(repo.ngramfreq,"V1","freq")
  } else
  { 
    
    repo.ngramfreq <- new.ngramfreq
  }
  
  
  saveRDS(repo.ngramfreq, paste(foldername, filename, sep="/"))
  
}


replaceFirstWordWithUNK <- function(olddictionary, dictionary,corp){
  setkey(dictionary,word)
  
  #check if old dictionary has anything and get new words if so
  #otherwise everything in the dictionary is new
  if (nrow(olddictionary) > 0) {
    setkey(olddictionary,word)
    setkey(dictionary,word)
    x <- olddictionary[dictionary]
    newwords <- x[is.na(x$wordID)]$word
  } else {
    newwords <- dictionary$word
  }
  
  
  v <- unlist(corp)
  #get indices to recreate list from big vector
  f <- rep(1:length(corp),sapply(corp,length))
  
  counter <- as.integer(ave(v, v, FUN=seq_along))
  v[counter==1 & v %in% newwords] <- "<unk>"

  
  #turn the vector back into a list
  newCorp <- split(v,f)
  names(newCorp) <- NULL      
  
  newCorp
}

replaceOOVWords <- function(corp, dictionary) {
  
  setkey(dictionary,word)
  
  #vector
  v <-(unlist(corp))
  #get indices to recreate list from big vector
  f <- rep(1:length(corp),sapply(corp,length))
  
  dt <- data.table(v)
  setnames(dt,"v","word")
  #setkey(dt,word)
  
  x <- dictionary[dt, on=c(word="word")]
  
  x[is.na(x$wordID)]$word <- "<unk>"
  
  v <- x$word
  
  newCorp <- split(v,f)
  names(newCorp) <- NULL
  
  newCorp
}