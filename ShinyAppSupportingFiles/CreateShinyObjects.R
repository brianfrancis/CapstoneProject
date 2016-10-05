

##read in probability tables
dictionary.by.id <- fread("dictionary.csv")
dictionary.by.word <- fread("dictionary.csv")
onegram.dt <- fread("onegram.prob.csv")
twogram.dt <- fread("twogram.prob.csv")
threegram.dt <- fread("threegram.prob.csv")
fourgram.dt <- fread("fourgram.prob.csv")
profanity <- unlist(read.table("profanity.txt",header=TRUE, stringsAsFactors = FALSE))


setkey(dictionary.by.id,wordID)
setkey(dictionary.by.word,word)
setkey(onegram.dt, prediction)
setkey(twogram.dt, cond1)
setkey(threegram.dt, cond1,cond2)
setkey(fourgram.dt, cond1,cond2,cond3)

#get IDs for "words" we don't want to predict and remove those from predictions for each ngram table
removeIDs <- dictionary.by.word[word %in% c("<start>", "<unk>", "<profanity>")]$wordID
onegram.dt <- onegram.dt[!prediction %in% removeIDs]
twogram.dt <- twogram.dt[!prediction %in% removeIDs]
threegram.dt <- threegram.dt[!prediction %in% removeIDs]
fourgram.dt <- fourgram.dt[!prediction %in% removeIDs]

saveRDS(fourgram.dt,"fourgram.rds")
saveRDS(threegram.dt,"threegram.rds")
saveRDS(twogram.dt,"twogram.rds")
saveRDS(onegram.dt,"onegram.rds")
saveRDS(dictionary.by.id,"dictionary.by.id.rds")
saveRDS(dictionary.by.word,"dictionary.by.word.rds")
saveRDS(profanity,"profanity.rds")

