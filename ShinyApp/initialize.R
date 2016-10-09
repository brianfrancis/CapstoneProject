
options(warn=-1)

library(stylo)
library(stringi)
library(tm)
library(stringr)
library(data.table)
library(openNLP)
library(NLP)
library(RSQLite)


highestngram <- 5


#fourgram.dt <- readRDS("fourgram.rds")
#threegram.dt <- readRDS("threegram.rds")
#twogram.dt <- readRDS("twogram.rds")
#onegram.dt <- readRDS("onegram.rds")
dictionary.by.id <- readRDS("dictionary.by.id.rds")
dictionary.by.word <- readRDS("dictionary.by.word.rds")
profanity <- readRDS("profanity.rds")

sqlite    <- dbDriver("SQLite")
ngramdb <- dbConnect(sqlite,"ngram.db")



