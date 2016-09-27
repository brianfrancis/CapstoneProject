

getPerplexity <- function(train.folder, test.folder, train.filename, test.filename,
                          key) {
  
  library (data.table)
  train.dt <- fread(paste(train.folder,train.filename, sep="/"))
  setkeyv(train.dt, key)
  
  test.dt <- fread(paste(test.folder,test.filename, sep="/"))
  setkeyv(test.dt, key)
  
  merged <- train.dt[test.dt]
  
  p <- merged[!is.na(merged$p),p]
  
  #perplexity <- (2^sum(log2(p)))^ (-1/length(p))
  perplexity <- exp(-sum(log(p)) / length(p))
  #perplexity <- (prod(p)) ^ (-1/length(p))

  oov <- length(merged$p[is.na(merged$p)])/length(merged$p)
  
  print(paste("perplexity:",signif(perplexity,4), "| oov:", signif(oov,4), sep=" "))
}

getAccuracyPerWord <- function(train.folder, test.folder){
  library (data.table)
  train.onegram.dt <- fread(paste(train.folder,"onegram.prob.csv", sep="/"))
  train.onegram.dt[,ngramlevel:=1]
  train.onegram.dt <- train.onegram.dt[order(-p)]
  train.onegram.dt <- train.onegram.dt[1:3,]
  
  train.twogram.dt <- fread(paste(train.folder,"twogram.prob.csv", sep="/"))
  train.twogram.dt[,ngramlevel:=2]
  setkeyv(train.twogram.dt, c("cond1"))
  train.twogram.dt[,prank:=rank(-p,ties.method="first"), by=.(cond1)]
  train.twogram.dt <- train.twogram.dt[prank<=3,]
  
  
  train.threegram.dt <- fread(paste(train.folder,"threegram.prob.csv", sep="/"))
  train.threegram.dt[,ngramlevel:=3]
  setkeyv(train.threegram.dt, c("cond1","cond2"))
  train.threegram.dt[,prank:=rank(-p,ties.method="first"), by=.(cond1, cond2)]
  train.threegram.dt <- train.threegram.dt[prank<=3,]
  
  train.fourgram.dt <- fread(paste(train.folder,"fourgram.prob.csv", sep="/"))
  train.fourgram.dt[,ngramlevel:=4]
  setkeyv(train.fourgram.dt, c("cond1","cond2", "cond3"))
  train.fourgram.dt[,prank:=rank(-p,ties.method="first"), by=.(cond1, cond2,cond3)]
  train.fourgram.dt <- train.fourgram.dt[prank<=3,]
  
  test.dt <- fread(paste(test.folder,"fourgramfreq.csv", sep="/"))
  setkeyv(test.dt, c("cond1","cond2", "cond3"))
  
  four <- test.dt[train.fourgram.dt, nomatch=0]
  three <- test.dt[train.threegram.dt, on=c(cond2 = "cond1", 
                                            cond3="cond2"),nomatch=0]
  two <- test.dt[train.twogram.dt, on=c(cond3 = "cond1"),
               nomatch=0, allow.cartesian=TRUE]
  
  
  train.onegram.dt[,d:=1]
  test.dt[,d:=1]
  
  one <- test.dt[train.onegram.dt, on=c(d = "d"),
                 nomatch=0, allow.cartesian=TRUE]
  
  one[,d:=NULL]
  two[,prank:=NULL]
  three[,prank:=NULL]
  four[,prank:=NULL]
  
  all <- rbind(four,three,two,one)
  
  all[,prank:=rank(c(-ngramlevel,-p),ties.method="first"), by=.(cond1, cond2,cond3, prediction)]
  
  topthree <- all[prank<=3,]
  topthree[,match:=as.integer(prediction==i.prediction)]
  
  topthree[,c("i.prediction","p","ngramlevel","prank"):=NULL]
  
  #??
  x <- topthree[,as.logical(sum(match)),by=.(cond1,cond2,cond3,prediction, freq)]
  setnames(x,"V1","match")
  accuracyperword <- x[,.(sum(freq),sum(freq*match)/sum(freq)), by=prediction]
  setnames(accuracyperword,"V1","freq")
  setnames(accuracyperword,"V2","accuracy")
  
  accuracyperword 
}


case <- "case3"

train.folder <- paste(case,"train", sep="/")
test.folder <- paste(case,"test", sep="/")


  getPerplexity(train.folder=train.folder, test.folder=test.folder, train.filename = "twogram.prob.csv", 
                test.filename = "twogramfreq.csv", key=c("cond1", "prediction"))
  
  getPerplexity(train.folder=train.folder, test.folder=test.folder, train.filename = "threegram.prob.csv", 
                test.filename = "threegramfreq.csv", key=c("cond1","cond2", "prediction"))
  
  getPerplexity(train.folder=train.folder, test.folder=test.folder, train.filename = "fourgram.prob.csv", 
              test.filename = "fourgramfreq.csv", key=c("cond1","cond2", "cond3", "prediction"))


  accuracyperword <- getAccuracyPerWord(train.folder=train.folder, test.folder=test.folder)
  plot(accuracyperword$accuracy ~ log10(accuracyperword$freq))
  sum(accuracyperword$freq*accuracyperword$accuracy)/sum(accuracyperword$freq)

  sub <- accuracyperword[log10(freq)<2.5]
  sum(sub$freq*sub$accuracy)/sum(sub$freq)
