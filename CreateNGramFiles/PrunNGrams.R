removeRedundant <- function(path) {
  library(data.table)
  four <- fread(paste(path,"fourgramfreq.csv", sep="/"))
  x <- four[,sum(freq),by=.(cond1,cond2,cond3)]
  x[,y:=.N, by=.(cond2,cond3)]
  setkey(x,cond1,cond2,cond3)
  setkey(four,cond1,cond2,cond3)
  four <- four[x]
  four <- four[y>1]
  four[,c("V1","y"):=NULL]
  fwrite(four,paste(path,"fourgramfreq.csv", sep="/"))
  rm(four)
  gc()
  
  three <- fread(paste(path,"threegramfreq.csv", sep="/"))
  x <- three[,sum(freq),by=.(cond1,cond2)]
  x[,y:=.N, by=.(cond2)]
  setkey(x,cond1,cond2)
  setkey(three,cond1,cond2)
  three <- three[x]
  three <- three[y>1]
  three[,c("V1","y"):=NULL]
  fwrite(three,paste(path,"threegramfreq.csv", sep="/"))
  
}



pruneNgram <- function(path, fourpercent =.25, threepercent = .25) {
  
  #get fourgrams
  fourgram.freq <- fread(paste(path,"fourgramfreq.csv", sep="/"))
  setkey(fourgram.freq,cond1,cond2,cond3,prediction)
  fourgram <- fread(paste(path,"fourgram.prob.csv", sep="/"))
  setkey(fourgram,cond1,cond2,cond3,prediction)
  fourgram <- fourgram[fourgram.freq]
  rm(fourgram.freq)
  
  #get threegrams
  threegram.freq <- fread(paste(path,"threegramfreq.csv", sep="/"))
  setkey(threegram.freq,cond1,cond2,prediction)
  threegram <- fread(paste(path,"threegram.prob.csv", sep="/"))
  setkey(threegram,cond1,cond2,prediction)
  threegram <- threegram[threegram.freq]
  rm(threegram.freq)

  
  fourgram <- fourgram[order(1/(p*freq))] 
  fourgram <- head(fourgram,nrow(fourgram)*fourpercent)
  
  setkey(fourgram,cond1,cond2,cond3)
  setkey(threegram, cond1, cond2, prediction)
  
  #get left outer join
  unique.fourgram <- fourgram[,.N,by=c("cond1","cond2","cond3")]
  
  threegram <- merge(threegram, unique.fourgram, 
             by.x=c('cond1','cond2','prediction'), 
             by.y=c('cond1','cond2', 'cond3'), all.x=TRUE)
  
  threegram <- threegram[order(1/(p*freq))] 
  
  threegram.keep <- head(threegram[is.na(N)],
                         nrow(threegram[is.na(N)])*threepercent)
  
  threegram <- rbind(threegram[!is.na(N)], threegram.keep)
  rm(threegram.keep)
  
  fwrite(threegram[,.(cond1,cond2,prediction,p)],paste(path,"threegram.trimmed.csv",sep="/"))
  fwrite(fourgram[,.(cond1,cond2,cond3,prediction,p)],paste(path,"fourgram.trimmed.csv",sep="/"))
  
}
#



simplePrune <- function(folder,freqfile,probfile,key,minfreq=1){
  freq <- readRDS(paste(folder,freqfile,sep="/"))
  setkeyv(freq,key)
  prob <- readRDS(paste(folder,probfile,sep="/"))
  setkeyv(prob,key)
  #saveRDS(prob[freq[freq>1]][,c(key,"p")],probfile)
  join <- prob[freq[freq>1]]
  join[,freq:=NULL]
  saveRDS(join,paste(folder,probfile,sep="/"))
}