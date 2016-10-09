

getPerplexity <- function(train.folder, test.folder, train.filename, test.filename,
                          key) {
  
  library (data.table)
  train.dt <- readRDS(paste(train.folder,train.filename, sep="/"))
  setkeyv(train.dt, key)
  
  test.dt <- readRDS(paste(test.folder,test.filename, sep="/"))
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

getAccuracyPerWord1 <- function(train.folder, test.folder, dictionary.folder){
  
  ## don't predict start or unk ???
  
  
  library (data.table)

  s <- Sys.time()
  
  dictionary <- fread(paste(dictionary.folder,"dictionary.csv", sep="/"))
  
  removeIDs <- dictionary[word %in% c("<start>", "<unk>", "<profanity>,<NUM>")]$wordID
  
  train.onegram.dt <- readRDS(paste(train.folder,"onegram.prob.rds", sep="/"))
  train.onegram.dt <- train.onegram.dt[!prediction %in% removeIDs]
  train.onegram.dt[,ngramlevel:=1]
  train.onegram.dt <- train.onegram.dt[order(-p)]
  train.onegram.dt <- train.onegram.dt[1:3,]
  train.onegram.dt[,n:=.N]
  train.onegram.dt[,rank:=cumsum(n)/n]
  setkey(train.onegram.dt,prediction)
  
  saveRDS(train.onegram.dt,paste(train.folder,"top3.onegram.rds",sep="/"))
  rm(train.onegram.dt)
  
  train.twogram.dt <- readRDS(paste(train.folder,"twogram.prob.rds", sep="/"))
  train.twogram.dt <- train.twogram.dt[!prediction %in% removeIDs]
  train.twogram.dt[,ngramlevel:=2]
  setkeyv(train.twogram.dt, c("cond1","p"))
  train.twogram.dt <- train.twogram.dt[,tail(.SD, 3),by=.(cond1)] 
  train.twogram.dt[,n:=.N,by=.(cond1)]
  train.twogram.dt <- train.twogram.dt[order(cond1,-p)]
  train.twogram.dt[,rank:=cumsum(n)/n,by=.(cond1)]

  saveRDS(train.twogram.dt,paste(train.folder,"top3.twogram.rds",sep="/"))
  rm(train.twogram.dt)
  
  train.threegram.dt <- readRDS(paste(train.folder,"threegram.prob.rds", sep="/"))
  #train.threegram.dt <- readRDS(paste(train.folder,"threegram.trimmed.rds", sep="/"))
  train.threegram.dt <- train.threegram.dt[,.(cond1,cond2,prediction,p)]
  train.threegram.dt <- train.threegram.dt[!prediction %in% removeIDs]
  train.threegram.dt[,ngramlevel:=3]
  setkeyv(train.threegram.dt, c("cond1","cond2", "p"))
  train.threegram.dt <- train.threegram.dt[,tail(.SD, 3),by=.(cond1, cond2)] 
  train.threegram.dt[,n:=.N,by=.(cond1, cond2)]
  train.threegram.dt <- train.threegram.dt[order(cond1,cond2,-p)]
  train.threegram.dt[,rank:=cumsum(n)/n,by=.(cond1,cond2)]
  
  saveRDS(train.threegram.dt,paste(train.folder,"top3.threegram.rds",sep="/"))
  rm(train.threegram.dt)
  
  
  
  #train.fourgram.dt <- readRDS(paste(train.folder,"fourgram.trimmed.rds", sep="/"))
  train.fourgram.dt <- readRDS(paste(train.folder,"fourgram.prob.rds", sep="/"))
  train.fourgram.dt <- train.fourgram.dt[,.(cond1,cond2,cond3,prediction,p)]
  train.fourgram.dt <- train.fourgram.dt[!prediction %in% removeIDs]
  train.fourgram.dt[,ngramlevel:=4]
  setkeyv(train.fourgram.dt, c("cond1","cond2", "cond3","p"))
  train.fourgram.dt <- train.fourgram.dt[,tail(.SD, 3),by=.(cond1, cond2,cond3)]
  train.fourgram.dt[,n:=.N,by=.(cond1, cond2,cond3)]
  train.fourgram.dt <- train.fourgram.dt[order(cond1,cond2,cond3,-p)]
  train.fourgram.dt[,rank:=cumsum(n)/n,by=.(cond1,cond2,cond3)]
  
  saveRDS(train.fourgram.dt,paste(train.folder,"top3.fourgram.rds",sep="/"))
  rm(train.fourgram.dt)
  
  #train.fourgram.dt[,prank:=rank(-p,ties.method="first"), by=.(cond1, cond2,cond3)]
  #train.fourgram.dt <- train.fourgram.dt[prank<=3,]
  
  train.fivegram.dt <- readRDS(paste(train.folder,"fivegram.prob.rds", sep="/"))
  train.fivegram.dt <- train.fivegram.dt[,.(cond1,cond2,cond3,cond4,prediction,p)]
  train.fivegram.dt <- train.fivegram.dt[!prediction %in% removeIDs]
  train.fivegram.dt[,ngramlevel:=5]
  setkeyv(train.fivegram.dt, c("cond1","cond2", "cond3","cond4","p"))
  train.fivegram.dt <- train.fivegram.dt[,tail(.SD, 3),by=.(cond1, cond2,cond3,cond4)]
  train.fivegram.dt[,n:=.N,by=.(cond1, cond2,cond3,cond4)]
  train.fivegram.dt <- train.fivegram.dt[order(cond1,cond2,cond3,cond4,-p)]
  train.fivegram.dt[,rank:=cumsum(n)/n,by=.(cond1,cond2,cond3,cond4)]
  
  saveRDS(train.fivegram.dt,paste(train.folder,"top3.fivegram.rds",sep="/"))
  rm(train.fivegram.dt)
}

getAccuracyPerWord2 <- function(train.folder, test.folder, dictionary.folder){
  s <- Sys.time()
  library(data.table)
    
  test.dt <- readRDS(paste(test.folder,"fivegramfreq.rds", sep="/"))
#  test.dt <- test.dt[!prediction %in% removeIDs]
  setnames(test.dt,"prediction","actual")
  setnames(test.dt,"cond1","actual.cond1")
  setnames(test.dt,"cond2","actual.cond2")
  setnames(test.dt,"cond3","actual.cond3")
  setnames(test.dt,"cond4","actual.cond4")
  setkey(test.dt,actual.cond1,actual.cond2,actual.cond3,actual.cond4)
  
    
  
  e <- Sys.time()
  print(e-s)
  print("Time to get data")
  
  s <- Sys.time()
  
  train.fivegram.dt <- readRDS(paste(train.folder,"top3.fivegram.rds", sep="/"))
  
  match.five <- train.fivegram.dt[test.dt,.(actual.cond1,actual.cond2,actual.cond3, actual.cond4, actual,
                                            .N,sum(rank*(actual==prediction))), 
                                  on=c(cond1="actual.cond1",cond2="actual.cond2",cond3="actual.cond3", cond4="actual.cond4"), 
                                  by=.EACHI]
  rm(train.fivegram.dt)
  gc()
  setnames(match.five,"V7","matchrank")
  match.five[is.na(matchrank)]$matchrank <- 0
  match.five[,c("cond1","cond2","cond3","cond4"):=NULL]
  match.five[,ngramlevel:=5]
  
  train.fourgram.dt <- readRDS(paste(train.folder,"top3.fourgram.rds", sep="/"))
  match.four <- train.fourgram.dt[test.dt,.(actual.cond1,actual.cond2,actual.cond3, actual.cond4, actual,
                                              .N,sum(rank*(actual==prediction))), 
                                    on=c(cond1="actual.cond2",cond2="actual.cond3",cond3="actual.cond4"), 
                                    by=.EACHI]
  rm(train.fourgram.dt)
  gc()
  setnames(match.four,"V7","matchrank")
  match.four[is.na(matchrank)]$matchrank <- 0
  match.four[,c("cond1","cond2","cond3"):=NULL]
  match.four[,ngramlevel:=4]
  
  train.threegram.dt <- readRDS(paste(train.folder,"top3.threegram.rds", sep="/"))
  match.three <- train.threegram.dt[test.dt,.(actual.cond1,actual.cond2,actual.cond3, actual.cond4, actual,
                                              .N,sum(rank*(actual==prediction))), 
                                    on=c(cond1="actual.cond3",cond2="actual.cond4"), 
                                    by=.EACHI]
  rm(train.threegram.dt)
  gc()
  setnames(match.three,"V7","matchrank")
  match.three[is.na(matchrank)]$matchrank <- 0
  match.three[,c("cond1","cond2"):=NULL]
  match.three[,ngramlevel:=3]
  
  train.twogram.dt <- readRDS(paste(train.folder,"top3.twogram.rds", sep="/"))
  match.two <- train.twogram.dt[test.dt,.(actual.cond1,actual.cond2,actual.cond3, actual.cond4, actual,
                                              .N,sum(rank*(actual==prediction))), 
                                    on=c(cond1="actual.cond4"), 
                                    by=.EACHI]
  rm(train.twogram.dt)
  gc()
  setnames(match.two,"V7","matchrank")
  match.two[is.na(matchrank)]$matchrank <- 0
  match.two[,c("cond1"):=NULL]
  match.two[,ngramlevel:=2]
  
  train.onegram.dt <- readRDS(paste(train.folder,"top3.onegram.rds", sep="/"))
  match.one <- train.onegram.dt[test.dt,.(actual.cond1,actual.cond2,actual.cond3, actual.cond4, actual,
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
  
  all <- rbind(match.five,match.four,match.three,match.two,match.one)
  rm(match.five,match.four,match.three,match.two,match.one)
  gc()
  print("all made")
  
  all <- all[order(actual.cond1,actual.cond2,actual.cond3,actual.cond4,actual,-ngramlevel)]
  all[,levelrank:=(as.logical(matchrank)*(cumsum(N)-N)+matchrank),by=c("actual.cond1","actual.cond2","actual.cond3",
                                                                       "actual.cond4","actual")]
  
  print("level rank")
  
  all[levelrank==0,levelrank := 100]
  
  all <- all[,min(levelrank),by=c("actual.cond1","actual.cond2","actual.cond3","actual.cond4","actual")]
  setnames(all,"V1","finalrank")
  setkey(all,actual.cond1,actual.cond2,actual.cond3,actual.cond4,actual)
  setkey(test.dt,actual.cond1,actual.cond2,actual.cond3,actual.cond4,actual)
  all <- all[test.dt]
  
  print("get all")
  
  print(sum((all$finalrank<=3)*all$freq)/sum(all$freq))
  
}


case <- "all_new_prune"

train.folder <- paste(case,"train", sep="/")
test.folder <- paste(case,"validation", sep="/")
dictionary.folder <- paste(case,"dictionary", sep="/")


  getPerplexity(train.folder=train.folder, test.folder=test.folder, train.filename = "twogram.prob.rds", 
                test.filename = "twogramfreq.rds", key=c("cond1", "prediction"))
  
  getPerplexity(train.folder=train.folder, test.folder=test.folder, train.filename = "threegram.prob.rds", 
                test.filename = "threegramfreq.rds", key=c("cond1","cond2", "prediction"))
  
  getPerplexity(train.folder=train.folder, test.folder=test.folder, train.filename = "fourgram.prob.rds", 
              test.filename = "fourgramfreq.rds", key=c("cond1","cond2", "cond3", "prediction"))


  getPerplexity(train.folder=train.folder, test.folder=test.folder, train.filename = "fivegram.prob.rds", 
                test.filename = "fivegramfreq.rds", key=c("cond1","cond2", "cond3", "cond4", "prediction"))
  
  getAccuracyPerWord1(train.folder=train.folder, test.folder=test.folder,
                     dictionary.folder = dictionary.folder)
  
  getAccuracyPerWord2(train.folder=train.folder, test.folder=test.folder,
                      dictionary.folder = dictionary.folder)
  