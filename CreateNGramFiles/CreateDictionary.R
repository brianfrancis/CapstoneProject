

appendToDictionary <- function(corp, 
                               dict.filename, dict.foldername,
                               initialfile){
  
  unigramfreq <- data.table(make.frequency.list(corp, value=TRUE, relative=FALSE))
  setnames(unigramfreq,"data", "word")
  setnames(unigramfreq, "N", "freq")
  #setnames ??
  
  
  ##check if this is the first file for the dictionary
  ##if not bring in the existing info and get the next word ID
  ##otherwise next word ID = 1
  if (initialfile == FALSE){
    dictionary <- fread(paste(dict.foldername, dict.filename, sep="/"))
    nextindex <- max(dictionary$wordID) + 1
    unigramfreq[,wordID := seq(nextindex, (nextindex-1+nrow(unigramfreq)))]
    combined <- rbind(dictionary, unigramfreq)
    setkey(combined, word)
    dictionary <- combined[, .(min(wordID), sum(freq)), by=.(word)]
    setnames(dictionary,"V1","wordID")
    setnames(dictionary,"V2","freq")
  } else
  { 
    nextindex <- 1
    unigramfreq[,wordID := seq(1, nrow(unigramfreq))]
    dictionary <- unigramfreq
  }
  
  
  write.csv(dictionary, paste(dict.foldername, dict.filename, sep="/"), 
            row.names=FALSE)

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
  new.ngramfreq <- cbind(new.ngramfreq, ids)
  rm(ids, cols)
  new.ngramfreq[,wordID:=NULL]
  
  ##check if this is the first file for the dictionary
  ##if not bring in the existing info and get the next word ID
  ##otherwise next word ID = 1
  if (initialfile == FALSE){
    repo.ngramfreq <- fread(paste(foldername, filename, sep="/"))
    combined <- rbind(repo.ngramfreq, new.ngramfreq)
    rm(repo.ngramfreq,new.ngramfreq)
    setkey(combined, wordID)
    repo.ngramfreq <- combined[, .(sum(freq)), by=.(wordID)]
    rm(combined)
    setnames(repo.ngramfreq,"V1","freq")
  } else
  { 
    
    repo.ngramfreq <- new.ngramfreq
  }
  
  
  write.csv(repo.ngramfreq, paste(foldername, filename, sep="/"), 
            row.names=FALSE)
  
}


replaceFirstWordWithUNK <- function(corp, dictionary){
  
}