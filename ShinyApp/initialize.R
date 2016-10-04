
options(warn=-1)

library(stylo)
library(stringi)
library(tm)
library(stringr)
library(data.table)

highestngram <- 4

##read in probability tables
dictionary <- fread("dictionary.csv")
onegram.dt <- fread("onegram.prob.csv")
twogram.dt <- fread("twogram.prob.csv")
threegram.dt <- fread("threegram.prob.csv")
fourgram.dt <- fread("fourgram.prob.csv")
profanity <- unlist(read.table("profanity.txt",header=TRUE, stringsAsFactors = FALSE))


setkey(dictionary,wordID)
setkey(onegram.dt, prediction)
setkey(twogram.dt, cond1, prediction)
setkey(threegram.dt, cond1,cond2, prediction)
setkey(fourgram.dt, cond1,cond2,cond3, prediction)

#get IDs for "words" we don't want to predict and remove those from predictions for each ngram table
removeIDs <- dictionary[word %in% c("<start>", "<unk>", "<profanity>")]$wordID
onegram.dt <- onegram.dt[!prediction %in% removeIDs]
twogram.dt <- twogram.dt[!prediction %in% removeIDs]
threegram.dt <- threegram.dt[!prediction %in% removeIDs]
fourgram.dt <- fourgram.dt[!prediction %in% removeIDs]



