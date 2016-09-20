
options(warn=-1)

library(stylo)
library(stringi)
library(tm)
library(stringr)
library(data.table)

highestngram <- 4

##read in probability tables
onegram.dt <- fread("onegram.prob.csv")
twogram.dt <- fread("twogram.prob.csv")
threegram.dt <- fread("threegram.prob.csv")
fourgram.dt <- fread("fourgram.prob.csv")

setnames(onegram.dt,"ngram", "prediction")
setkey(onegram.dt, prediction, p)
setkey(twogram.dt, lookup, prediction)
setkey(threegram.dt, lookup, prediction)
setkey(fourgram.dt, lookup, prediction)
