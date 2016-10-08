
source("MyFunctions.R")


createProbFiles <- function(foldername, onegramfile="onegramfreq.rds", 
                            twogramfile="twogramfreq.rds",
                            threegramfile="threegramfreq.rds", 
                            fourgramfile="fourgramfreq.rds",
                            fivegramfile="fivegramfreq.rds") 
{

  library(data.table)
  

  onegram.dt <- readRDS(paste(foldername, onegramfile, sep="/"))
  onegram.dt[, p:=freq/sum(freq)]
  
  onegram.dt[,c("p"):= signif(onegram.dt$p,4)]
  
  saveRDS(onegram.dt[,.(prediction,p)], paste(foldername,"onegram.prob.rds",sep="/"))
  

  ####################################################################################
  ##start bigrams processing
  
  twogram.dt <- readRDS(paste(foldername, twogramfile, sep="/"))
  setkey(twogram.dt,cond1)
   
  #get counts from unigram
  onegram.dt[,p:=NULL]
  #setnames(onegram.dt, "ngram", "lookup")
  setnames(onegram.dt, "freq", "onegram.freq")
  setkey(onegram.dt,prediction)
  
  #merge data tables
  twogram.dt <- twogram.dt[onegram.dt, on=c(cond1 = "prediction"),nomatch=0]

  #get cardinalilty of word in bigram
  setkey(twogram.dt, prediction)
  twogram.dt[, cardGivenWord := .(.N), by=.(prediction)]

  #get cardinality of condition (previous word) in bigram
  setkey(twogram.dt, cond1)
  twogram.dt[, cardGivenCondition := .(.N), by=.(cond1)]
  
  #get the continuation probability of the unigram
  twogram.dt[, c("unigram.p") := cardGivenWord/.N]

  
  #calculate probabiliyt of bigrams as if they are the highest order
  twogram.dt[, c("p")
                  := smoothP(x1=freq, x2=onegram.freq,
                             x3=cardGivenCondition, x4=unigram.p)]

  rm(onegram.dt)
  gc()
  
  twogram.dt[,c("p"):= signif(twogram.dt$p,4)]
  
  saveRDS(twogram.dt[,.(cond1,prediction,p)], 
            paste(foldername,"twogram.prob.rds",sep="/"))

  
  ####################################################################################
  ##start trigrams processing
  
  #read in trigram frequencies
  threegram.dt <- readRDS(paste(foldername, threegramfile, sep="/"))
  setkey(threegram.dt,cond1,cond2)
  
  # get need bigram info and merge with trigrams
  twogram.dt[, c("onegram.freq", "cardGivenWord", "p") := NULL]
  setnames(twogram.dt, "freq", "twogram.freq")
  setnames(twogram.dt, "cardGivenCondition", "bigram.cardGivenCondition")
  setkey(twogram.dt,cond1,prediction)
  
  #merge data tables
  threegram.dt <- threegram.dt[twogram.dt, on=c(cond1="cond1", cond2="prediction"),
                               nomatch=0]
  rm(twogram.dt)
  gc()
  
  
  #get cardinalilty of word in trigram
  setkey(threegram.dt, prediction)
  threegram.dt[, threegram.cardGivenWord := .(.N), by=.(prediction)]
  
  
  #get cardinality of condition (previous 2 words) in trigram
  setkey(threegram.dt, cond1, cond2)
  threegram.dt[, threegram.cardGivenCondition := .(.N), by=.(cond1,cond2)]
  
  #get teh contuniation probability for the bigrams
  threegram.dt[, c("bigram.p")
                          := smoothP(x1=threegram.cardGivenWord,  # nbr of trigrams ending in the word
                                     x2=.N,  # nbr of trigrams
                                     x3=bigram.cardGivenCondition,  # nbr of bigrams having the condition (second word in trigramm)
                                     x4=unigram.p)]  # the continuation probability for unigrams previously calculated for bigrams


  #drop columns we don't need anymore
  threegram.dt[,c("threegram.cardGivenWord","bigram.cardGivenCondition", "unigram.p"):=NULL]

  gc()
  
  #get probability for the trigram as if it is the highest order n-gram
  threegram.dt[, c("p")
                       := smoothP(x1=freq, x2=twogram.freq,
                                  x3=threegram.cardGivenCondition,
                                  x4=bigram.p)]
  
  threegram.dt[,c("p"):= signif(threegram.dt$p,4)]
   
   saveRDS(threegram.dt[, .(cond1,cond2,prediction,p)], 
             paste(foldername,"threegram.prob.rds",sep="/"))
  
    
   ####################################################################################
   ##start quad-grams processing
   
   #read in fourgram frequencies
   fourgram.dt <- readRDS(paste(foldername, fourgramfile, sep="/"))
   
   setkey(fourgram.dt,cond1,cond2,cond3)
   
   # get need trigram info and merge with fourgrams
   threegram.dt[, c("twogram.freq", "p") := NULL]
   setnames(threegram.dt, "freq", "threegram.freq")
   setkey(threegram.dt,cond1,cond2,prediction)
   
   #merge data tables
   fourgram.dt <- fourgram.dt[threegram.dt, on=c(cond1 = "cond1", 
                                                 cond2="cond2",
                                                 cond3="prediction"),
                              nomatch=0]
   rm(threegram.dt)
   gc()
   
   #get cardinalilty of word in fourgram
   setkey(fourgram.dt, prediction)
   fourgram.dt[, fourgram.cardGivenWord := .(.N), by=.(prediction)]
   
   #get cardinality of condition (previous 3 words) in fourgram
   setkey(fourgram.dt, cond1,cond2,cond3)
   fourgram.dt[, fourgram.cardGivenCondition := .(.N), by=.(cond1,cond2,cond3)]
   
   #get teh contuniation probability for the trigrams
   fourgram.dt[, c("trigram.p")
                := smoothP(x1=fourgram.cardGivenWord,  # nbr of fourgrams ending in the word
                           x2=.N,  # nbr of fourgrams
                           x3=threegram.cardGivenCondition,  # nbr of trigrams having the condition (second word in trigramm)
                           x4=bigram.p)]  # the continuation probability for bigrams previously calculated for bigrams
   
   #drop columns we don't need anymore
   fourgram.dt[,c("fourgram.cardGivenWord", "threegram.cardGivenCondition", "bigram.p"):=NULL]
   
   #get probability for the trigram as if it is the highest order n-gram
   fourgram.dt[, c("p")
                := smoothP(x1=freq, x2=threegram.freq,
                           x3=fourgram.cardGivenCondition,
                           x4=trigram.p)]
   
   #drop columns we don't need anymore
   #fourgram.dt[,c("fourgram.cardGivenCondition", "trigram.p"):=NULL]
   
   
   fourgram.dt[,c("p"):= signif(fourgram.dt$p,4)]
   
   saveRDS(fourgram.dt[, .(cond1,cond2,cond3,prediction,p)], 
             paste(foldername,"fourgram.prob.rds",sep="/"))
   

   ##start 5-grams processing
   
   #read in fivegram frequencies
   fivegram.dt <- readRDS(paste(foldername, fivegramfile, sep="/"))
   
   setkey(fivegram.dt,cond1,cond2,cond3,cond4)
   
   # get need trigram info and merge with fourgrams
   fourgram.dt[, c("threegram.freq", "p") := NULL]
   setnames(fourgram.dt, "freq", "fourgram.freq")
   setkey(fourgram.dt,cond1,cond2,cond3,prediction)
   
   #merge data tables
   fivegram.dt <- fivegram.dt[fourgram.dt, on=c(cond1 = "cond1", 
                                                 cond2="cond2",
                                                cond3="cond3",
                                                 cond4="prediction"),
                              nomatch=0]
   rm(fourgram.dt)
   gc()
   
   #get cardinalilty of word in fourgram
   setkey(fivegram.dt, prediction)
   fivegram.dt[, fivegram.cardGivenWord := .(.N), by=.(prediction)]
   
   #get cardinality of condition (previous 3 words) in fourgram
   setkey(fivegram.dt, cond1,cond2,cond3,cond4)
   fivegram.dt[, fivegram.cardGivenCondition := .(.N), by=.(cond1,cond2,cond3,cond4)]
   
   #get teh contuniation probability for the trigrams
   fivegram.dt[, c("fourgram.p")
               := smoothP(x1=fivegram.cardGivenWord,  # nbr of fourgrams ending in the word
                          x2=.N,  # nbr of fourgrams
                          x3=fourgram.cardGivenCondition,  # nbr of trigrams having the condition (second word in trigramm)
                          x4=trigram.p)]  # the continuation probability for trigrams previously calculated for bigrams
   
   #drop columns we don't need anymore
   fivegram.dt[,c("fivegram.cardGivenWord", "fourgram.cardGivenCondition", "trigram.p"):=NULL]
   
   #get probability for the trigram as if it is the highest order n-gram
   fivegram.dt[, c("p")
               := smoothP(x1=freq, x2=fourgram.freq,
                          x3=fivegram.cardGivenCondition,
                          x4=fourgram.p)]
   
   #drop columns we don't need anymore
   fivegram.dt[,c("fivegram.cardGivenCondition", "fourgram.p"):=NULL]
   
   
   fivegram.dt[,c("p"):= signif(fivegram.dt$p,4)]
   
   saveRDS(fivegram.dt[, .(cond1,cond2,cond3,cond4,prediction,p)], 
           paste(foldername,"fivegram.prob.rds",sep="/"))
   
}


smoothP <- function(x1, x2,  x3 ,x4, discount=.75){
  
  (pmax((x1 - discount),0)/x2) + (((discount/x2)*x3) *x4)
  
}

mergeProbFiles <- function(folder){
  onegram <- readRDS(paste(folder,"onegram.prob.csv", sep="/"))
  onegram[,freq:=NULL]
  onegram[,ngramlevel:=1]
  onegram[,cond1:=NA]
  onegram[,cond2:=NA]
  onegram[,cond3:=NA]
  setcolorder(onegram, c("cond3", "cond2", "cond1", "prediction", "p", "ngramlevel"))
  
  twogram <- readRDS(paste(folder,"twogram.prob.csv", sep="/"))
  twogram[,ngramlevel:=2]
  twogram[,cond2:=NA]
  twogram[,cond3:=NA]
  setcolorder(twogram, c("cond3", "cond2", "cond1", "prediction", "p", "ngramlevel"))
  
  threegram <- readRDS(paste(folder,"threegram.prob.csv", sep="/"))
  threegram[,ngramlevel:=3]
  names(threegram) <- c("cond2","cond1","prediction", "p", "ngramlevel")
  threegram[,cond3:=NA]
  setcolorder(threegram, c("cond3", "cond2", "cond1", "prediction", "p", "ngramlevel"))
  
  fourgram <- readRDS(paste(folder,"fourgram.prob.csv", sep="/"))
  fourgram[,ngramlevel:=4]
  names(fourgram) <- c("cond3","cond2","cond1","prediction", "p", "ngramlevel")
  
  all <- rbind(fourgram,threegram,twogram,onegram)
  saveRDS(all,paste(folder,"all.prob.rds", sep="/"))
}