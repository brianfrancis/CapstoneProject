
options(warn=-1)

library(stylo)
library(stringi)
library(tm)
library(stringr)
library(data.table)
library(openNLP)
library(NLP)


highestngram <- 4


print(system.time(fourgram.dt <- readRDS("fourgram.rds")))
print(system.time(threegram.dt <- readRDS("threegram.rds")))
twogram.dt <- readRDS("twogram.rds")
onegram.dt <- readRDS("onegram.rds")
dictionary.by.id <- readRDS("dictionary.by.id.rds")
dictionary.by.word <- readRDS("dictionary.by.word.rds")
profanity <- readRDS("profanity.rds")


