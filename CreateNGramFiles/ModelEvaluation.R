

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

getAccuracyPerWord <- function(train.folder, test.folder){
  library (data.table)

  s <- Sys.time()
  train.onegram.dt <- fread(paste(train.folder,"onegram.prob.csv", sep="/"))
  train.onegram.dt[,ngramlevel:=1]
  train.onegram.dt <- train.onegram.dt[order(-p)]
  train.onegram.dt <- train.onegram.dt[1:3,]
  train.onegram.dt[,n:=.N]
  train.onegram.dt[,rank:=cumsum(n)/n]
  
  train.twogram.dt <- fread(paste(train.folder,"twogram.prob.csv", sep="/"))
  train.twogram.dt[,ngramlevel:=2]
  setkeyv(train.twogram.dt, c("cond1","p"))
  train.twogram.dt <- train.twogram.dt[,tail(.SD, 3),by=.(cond1)] 
  #train.twogram.dt[,n:=.N,by=.(cond1)]
  #train.twogram.dt <- train.twogram.dt[order(cond1,-p)]
  #train.twogram.dt[,rank:=cumsum(n)/n,by=.(cond1)]
  twogramcount <- train.twogram.dt[,.N,by=c("cond1")]
  setnames(twogramcount,"N","twocount")
  
  #train.threegram.dt <- fread(paste(train.folder,"threegram.prob.csv", sep="/"))
  train.threegram.dt <- fread(paste(train.folder,"threegram.trimmed.csv", sep="/"))
  train.threegram.dt <- train.threegram.dt[,.(cond1,cond2,prediction,p)]
  train.threegram.dt[,ngramlevel:=3]
  setkeyv(train.threegram.dt, c("cond1","cond2", "p"))
  train.threegram.dt <- train.threegram.dt[,tail(.SD, 3),by=.(cond1, cond2)] 
  #train.threegram.dt[,n:=.N,by=.(cond1, cond2)]
  #train.threegram.dt <- train.threegram.dt[order(cond1,cond2,-p)]
  #train.threegram.dt[,rank:=cumsum(n)/n,by=.(cond1,cond2)]
  threegramcount <- train.threegram.dt[,.N,by=c("cond1", "cond2")]
  setnames(threegramcount,"N","threecount")
  
  train.fourgram.dt <- fread(paste(train.folder,"fourgram.trimmed.csv", sep="/"))
  #train.fourgram.dt <- fread(paste(train.folder,"fourgram.prob.csv", sep="/"))
  train.fourgram.dt <- train.fourgram.dt[,.(cond1,cond2,cond3,prediction,p)]
  train.fourgram.dt[,ngramlevel:=4]
  setkeyv(train.fourgram.dt, c("cond1","cond2", "cond3","p"))
  train.fourgram.dt <- train.fourgram.dt[,tail(.SD, 3),by=.(cond1, cond2,cond3)]
 
  fourgramcount <- train.fourgram.dt[,.N,by=c("cond1", "cond2","cond3")]
  setnames(fourgramcount,"N","fourcount")
  
#  train.fourgram.dt <- train.fourgram.dt[order(cond1,cond2,cond3,-p)]
#  train.fourgram.dt[,rank:=cumsum(n)/n,by=.(cond1,cond2,cond3)]
  setkey(train.fourgram.dt,cond1,cond2,cond3,prediction)
  #train.fourgram.dt[,prank:=rank(-p,ties.method="first"), by=.(cond1, cond2,cond3)]
  #train.fourgram.dt <- train.fourgram.dt[prank<=3,]
  
  test.dt <- fread(paste(test.folder,"fourgramfreq.csv", sep="/"))
  setkeyv(test.dt, c("cond1","cond2", "cond3", "prediction"))
  
  e <- Sys.time()
  print(e-s)
  print("Time to get data")
  
  s <- Sys.time()

  test.dt <- 
  
  
  four <- merge(test.dt,train.fourgram.dt,all.x=TRUE)
 
  three <- test.dt[train.threegram.dt, on=c(cond2 = "cond1", 
                                            cond3="cond2",
                                            prediction="prediction"),
                   nomatch=0]
  
  two <- test.dt[train.twogram.dt, on=c(cond3 = "cond1", prediction="prediction"),
               nomatch=0]
  
  one <- test.dt[train.onegram.dt, on=c(prediction = "prediction"),
                 nomatch=0]
  
  
  e <- Sys.time()
  print(e-s)
  print("Time to merge")
  
  s <- Sys.time()
  
  # two[,prank:=NULL]
  # three[,prank:=NULL]
  # four[,prank:=NULL]
  
  all <- rbind(four,three,two,one)
  all <- all[order(cond1,cond2,cond3,prediction,-ngramlevel,rank)]
  setkey(all,cond1,cond2,cond3,prediction)
  all <- all[,.(cond1,cond2,cond3,prediction)]
  all <- unique(all)
  all[,match:=TRUE]
  #all <- rbind(one)
  
  #setkey(all,cond1,cond2,cond3, prediction, ngramlevel,p)
  
  #topthree <- all[,tail(.SD, 3), by=.(cond1, cond2,cond3, prediction)]
  
  e <- Sys.time()
  print(e-s)
  print("all top three")
  
  s <- Sys.time()

  x <- all[test.dt]
  x[is.na(match)]$match <- FALSE

  sum(x$match*x$freq)/sum(x$freq)
  
#  topthree[,match:=as.integer(prediction==i.prediction)]
  
#  topthree[,c("i.prediction","p","ngramlevel"):=NULL]
  
  #??
  # x <- topthree[,as.logical(sum(match)),by=.(cond1,cond2,cond3,prediction, freq)]
  # setnames(x,"V1","match")
   accuracyperword <- x[,.(sum(freq),sum(freq*match)/sum(freq)), by=prediction]
   setnames(accuracyperword,"V1","freq")
   setnames(accuracyperword,"V2","accuracy")
  
  
  e <- Sys.time()
  print(e-s)
  print("final processing")
  
  
  accuracyperword 
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
  