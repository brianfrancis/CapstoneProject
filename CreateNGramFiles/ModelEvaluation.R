

getPerplexity <- function(train.folder, test.folder, train.filename, test.filename,
                          key) {
  
  library (data.table)
  train.dt <- fread(paste(train.folder,train.filename, sep="/"))
  setkeyv(train.dt, key)
  
  test.dt <- fread(paste(test.folder,test.filename, sep="/"))
  setkeyv(test.dt, key)
  
  merged <- train.dt[test.dt]
  
  p <- merged[!is.na(merged$p),p]
  freq <- merged[!is.na(merged$p),freq]
  
  
  #perplexity <- (2^sum(log2(p)))^ (-1/length(p))
  perplexity <- exp(-sum(freq*log(p)) / sum(freq))
  #perplexity <- exp(-sum(log(p)) / length(p))
  #perplexity <- (prod(p)) ^ (-1/length(p))

  oov <- length(merged$p[is.na(merged$p)])/length(merged$p)
  
  print(paste("perplexity:",signif(perplexity,4), "| oov:", signif(oov,4), sep=" "))
  

}

getAccuracyPerWord <- function(train.folder, test.folder, dictionary.folder){
  
  ## don't predict start or unk ???
  
  
  library (data.table)

  s <- Sys.time()
  
  dictionary <- fread(paste(dictionary.folder,"dictionary.csv", sep="/"))
  
  removeIDs <- dictionary[word %in% c("<start>", "<unk>", "<profanity>")]$wordID
  
  train.onegram.dt <- fread(paste(train.folder,"onegram.prob.csv", sep="/"))
  train.onegram.dt <- train.onegram.dt[!prediction %in% removeIDs]
  train.onegram.dt[,ngramlevel:=1]
  train.onegram.dt <- train.onegram.dt[order(-p)]
  train.onegram.dt <- train.onegram.dt[1:3,]
  train.onegram.dt[,n:=.N]
  train.onegram.dt[,rank:=cumsum(n)/n]
  setkey(train.onegram.dt,prediction)
  
  train.twogram.dt <- fread(paste(train.folder,"twogram.prob.csv", sep="/"))
  train.twogram.dt <- train.twogram.dt[!prediction %in% removeIDs]
  train.twogram.dt[,ngramlevel:=2]
  setkeyv(train.twogram.dt, c("cond1","p"))
  train.twogram.dt <- train.twogram.dt[,tail(.SD, 3),by=.(cond1)] 
  train.twogram.dt[,n:=.N,by=.(cond1)]
  train.twogram.dt <- train.twogram.dt[order(cond1,-p)]
  train.twogram.dt[,rank:=cumsum(n)/n,by=.(cond1)]

  #train.threegram.dt <- fread(paste(train.folder,"threegram.prob.csv", sep="/"))
  train.threegram.dt <- fread(paste(train.folder,"threegram.trimmed.csv", sep="/"))
  train.threegram.dt <- train.threegram.dt[,.(cond1,cond2,prediction,p)]
  train.threegram.dt <- train.threegram.dt[!prediction %in% removeIDs]
  train.threegram.dt[,ngramlevel:=3]
  setkeyv(train.threegram.dt, c("cond1","cond2", "p"))
  train.threegram.dt <- train.threegram.dt[,tail(.SD, 3),by=.(cond1, cond2)] 
  train.threegram.dt[,n:=.N,by=.(cond1, cond2)]
  train.threegram.dt <- train.threegram.dt[order(cond1,cond2,-p)]
  train.threegram.dt[,rank:=cumsum(n)/n,by=.(cond1,cond2)]

  train.fourgram.dt <- fread(paste(train.folder,"fourgram.trimmed.csv", sep="/"))
  #train.fourgram.dt <- fread(paste(train.folder,"fourgram.prob.csv", sep="/"))
  train.fourgram.dt <- train.fourgram.dt[,.(cond1,cond2,cond3,prediction,p)]
  train.fourgram.dt <- train.fourgram.dt[!prediction %in% removeIDs]
  train.fourgram.dt[,ngramlevel:=4]
  setkeyv(train.fourgram.dt, c("cond1","cond2", "cond3","p"))
  train.fourgram.dt <- train.fourgram.dt[,tail(.SD, 3),by=.(cond1, cond2,cond3)]
  train.fourgram.dt[,n:=.N,by=.(cond1, cond2,cond3)]
  train.fourgram.dt <- train.fourgram.dt[order(cond1,cond2,cond3,-p)]
  train.fourgram.dt[,rank:=cumsum(n)/n,by=.(cond1,cond2,cond3)]
  
  #train.fourgram.dt[,prank:=rank(-p,ties.method="first"), by=.(cond1, cond2,cond3)]
  #train.fourgram.dt <- train.fourgram.dt[prank<=3,]
  
  test.dt <- fread(paste(test.folder,"fourgramfreq.csv", sep="/"))
#  test.dt <- test.dt[!prediction %in% removeIDs]
  setnames(test.dt,"prediction","actual")
  setnames(test.dt,"cond1","actual.cond1")
  setnames(test.dt,"cond2","actual.cond2")
  setnames(test.dt,"cond3","actual.cond3")
  setkey(test.dt,actual.cond1,actual.cond2,actual.cond3)
  
    
  
  e <- Sys.time()
  print(e-s)
  print("Time to get data")
  
  s <- Sys.time()
  
  match.four <- train.fourgram.dt[test.dt,.(actual.cond1,actual.cond2,actual.cond3, actual,
                                              .N,sum(rank*(actual==prediction))), 
                                    on=c(cond1="actual.cond1",cond2="actual.cond2",cond3="actual.cond3"), 
                                    by=.EACHI]
  rm(train.fourgram.dt)
  gc()
  setnames(match.four,"V6","matchrank")
  match.four[is.na(matchrank)]$matchrank <- 0
  match.four[,c("cond1","cond2","cond3"):=NULL]
  match.four[,ngramlevel:=4]
  
  match.three <- train.threegram.dt[test.dt,.(actual.cond1,actual.cond2,actual.cond3, actual,
                                              .N,sum(rank*(actual==prediction))), 
                                    on=c(cond1="actual.cond2",cond2="actual.cond3"), 
                                    by=.EACHI]
  rm(train.threegram.dt)
  gc()
  setnames(match.three,"V6","matchrank")
  match.three[is.na(matchrank)]$matchrank <- 0
  match.three[,c("cond1","cond2"):=NULL]
  match.three[,ngramlevel:=3]
  
  match.two <- train.twogram.dt[test.dt,.(actual.cond1,actual.cond2,actual.cond3, actual,
                                              .N,sum(rank*(actual==prediction))), 
                                    on=c(cond1="actual.cond3"), 
                                    by=.EACHI]
  rm(train.twogram.dt)
  gc()
  setnames(match.two,"V6","matchrank")
  match.two[is.na(matchrank)]$matchrank <- 0
  match.two[,c("cond1"):=NULL]
  match.two[,ngramlevel:=2]
  
  match.one <- train.onegram.dt[test.dt,.(actual.cond1,actual.cond2,actual.cond3, actual,
                                          n,rank), 
                                by=.EACHI, on=c(prediction="actual")]
  rm(train.onegram.dt)
  gc()
  match.one[is.na(n)]$n <- 3
  match.one[is.na(rank)]$rank <- 0
  match.one[,prediction:=NULL]
  match.one[,ngramlevel:=1]
  setnames(match.one,"rank","matchrank")
  setnames(match.one,"n","N")
  
  e <- Sys.time()
  print(e-s)
  print("Time to merge")
  
  s <- Sys.time()
  
  all <- rbind(match.four,match.three,match.two,match.one)
  rm(match.four,match.three,match.two,match.one)
  gc()
  all <- all[order(actual.cond1,actual.cond2,actual.cond3,actual,-ngramlevel)]
  all[,levelrank:=(as.logical(matchrank)*(cumsum(N)-N)+matchrank),by=c("actual.cond1","actual.cond2","actual.cond3","actual")]
  all[levelrank==0]$levelrank <- 100
  
  all <- all[,min(levelrank),by=c("actual.cond1","actual.cond2","actual.cond3","actual")]
  setnames(all,"V1","finalrank")
  setkey(all,actual.cond1,actual.cond2,actual.cond3,actual)
  setkey(test.dt,actual.cond1,actual.cond2,actual.cond3,actual)
  all <- all[test.dt]
  
  print(sum((all$finalrank<=3)*all$freq)/sum(all$freq))
  
}


case <- "case4"

train.folder <- paste(case,"train", sep="/")
test.folder <- paste(case,"test", sep="/")


  getPerplexity(train.folder=train.folder, test.folder=test.folder, train.filename = "twogram.prob.csv", 
                test.filename = "twogramfreq.csv", key=c("cond1", "prediction"))
  
  getPerplexity(train.folder=train.folder, test.folder=test.folder, train.filename = "threegram.prob.csv", 
                test.filename = "threegramfreq.csv", key=c("cond1","cond2", "prediction"))
  
  getPerplexity(train.folder=train.folder, test.folder=test.folder, train.filename = "fourgram.prob.csv", 
              test.filename = "fourgramfreq.csv", key=c("cond1","cond2", "cond3", "prediction"))


  accuracyperword <- getAccuracyPerWord(train.folder=train.folder, test.folder=test.folder)
  sum(accuracyperword$freq*accuracyperword$accuracy)/sum(accuracyperword$freq)
  
  setkey(accuracyperword,prediction)
  
  dictionary <- fread(paste(case,"dictionary/dictionary.csv", sep="/"))
  setkey(dictionary,wordID)
  
  accuracyperword <- dictionary[accuracyperword]
  
  plot(accuracyperword$accuracy ~ log10(accuracyperword$freq))
  

  sub <- accuracyperword[accuracy>0]
  sum(sub$freq*sub$accuracy)/sum(sub$freq)
  
  
x <-  getPerplexity(train.folder=train.folder, test.folder=train.folder, train.filename = "fourgram.prob.csv", 
                test.filename = "fourgramfreq.csv", key=c("cond1","cond2", "cond3", "prediction"))
  