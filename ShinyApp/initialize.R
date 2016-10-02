
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


setkey(dictionary,wordID)
setkey(onegram.dt, prediction)
setkey(twogram.dt, cond1, prediction)
setkey(threegram.dt, cond1,cond2, prediction)
setkey(fourgram.dt, cond1,cond2,cond3, prediction)

# allgram.dt <- fread("all.prob.csv")
# setkey(allgram.dt, cond3,cond2,cond1, ngramlevel,p)

#drop any words from dictionary that aren't in our training set (will have test and val stuff)
dictionary <- dictionary[onegram.dt[,.(prediction)], on=c(wordID="prediction")]

#setwd("C:/Users/bfrancis/Desktop/Coursera/Capstone/CapstoneProject/ShinyApp")

