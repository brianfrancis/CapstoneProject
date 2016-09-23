
source("MyFunctions.R")


createProbFiles <- function(foldername, onegramfile="onegramfreq.csv", 
                            twogramfile="twogramfreq.csv",
                            threegramfile="threegramfreq.csv", 
                            fourgramfile="fourgramfreq.csv") 
{

  library(data.table)
  
  s <- Sys.time()
  
  onegram.dt <- fread(paste(foldername, onegramfile, sep="/"))
  onegram.dt[, p:=freq/sum(freq)]
  
  onegram.dt[,c("p"):= signif(onegram.dt$p,4)]
  
  fwrite(onegram.dt(,.(prediction,p)), paste(foldername,"onegram.prob.csv",sep="/"))
  
  
  e <- Sys.time()
  print('Time to create and write unigram')
  print(e-s)
  
  ####################################################################################
  ##start bigrams processing
  s <- Sys.time()
  
  twogram.dt <- fread(paste(foldername, twogramfile, sep="/"))
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
  
  fwrite(twogram.dt[,.(cond1,prediction,p)], 
            paste(foldername,"twogram.prob.csv",sep="/"))

  
  e <- Sys.time()
  print('Time to write bigram probs')
  print(e-s)
  
  ####################################################################################
  ##start trigrams processing
  
  s <- Sys.time()
  
  #read in trigram frequencies
  threegram.dt <- fread(paste(foldername, threegramfile, sep="/"))
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
   
   fwrite(threegram.dt[, .(cond1,cond2,prediction,p)], 
             paste(foldername,"threegram.prob.csv",sep="/"))
  
   #time to write trigram probs
   e <- Sys.time()
   print('Time to write trigram probs')
   print(e-s)
    
   ####################################################################################
   ##start quad-grams processing
   
   s <- Sys.time()
  
   #read in fourgram frequencies
   fourgram.dt <- fread(paste(foldername, fourgramfile, sep="/"))
   
   e <- Sys.time()
   print('read 4gram time')
   print(e-s)
   
   s <- Sys.time()
   
   e <- Sys.time()
   print('split ngram')
   print(e-s)
   
   s <- Sys.time()
   
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
   
   
   e <- Sys.time()
   print('merge 3gram and 4gram')
   print(e-s)
   
   s <- Sys.time()
 
   #get cardinalilty of word in fourgram
   setkey(fourgram.dt, prediction)
   fourgram.dt[, fourgram.cardGivenWord := .(.N), by=.(prediction)]
   
   #get cardinality of condition (previous 3 words) in fourgram
   setkey(fourgram.dt, cond1,cond2,cond3)
   fourgram.dt[, fourgram.cardGivenCondition := .(.N), by=.(cond1,cond2,cond3)]
   
   
   e <- Sys.time()
   print('get cardinality')
   print(e-s)
   
   s <- Sys.time()
   
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
   fourgram.dt[,c("fourgram.cardGivenCondition", "trigram.p"):=NULL]
   
   
   e <- Sys.time()
   print('get p')
   print(e-s)
   
   s <- Sys.time()
   
   fourgram.dt[,c("p"):= signif(fourgram.dt$p,4)]
   
   e <- Sys.time()
   print('Time to get signif digits')
   print(e-s)
   
   s <- Sys.time()
   
   fwrite(fourgram.dt[, .(cond1,cond2,cond3,prediction,p)], 
             paste(foldername,"fourgram.prob.csv",sep="/"))
   
   #time to write fourgram probs
   e <- Sys.time()
   print('Time to write quadgram probs')
   print(e-s)
   
}


smoothP <- function(x1, x2,  x3 ,x4, discount=.75){
  
  (pmax((x1 - discount),0)/x2) + (((discount/x2)*x3) *x4)
  
}
