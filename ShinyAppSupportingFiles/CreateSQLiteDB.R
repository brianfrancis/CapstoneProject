library(RSQLite)
library(data.table)

sqlite    <- dbDriver("SQLite")
ngramdb <- dbConnect(sqlite,"ngram.db")

#read in profanity list, dicitonary and unigram probabilities
profanity <- unlist(read.table("profanity.txt",header=TRUE, stringsAsFactors = FALSE))
onegram <- readRDS("onegram.prob.rds")
dictionary.by.id <- fread("dictionary.csv")
dictionary.by.word <- fread("dictionary.csv")

#remove words not in unigrams from dictionary
dictionary.by.id <- dictionary.by.id[onegram[,.(prediction)],on=c(wordID="prediction")]
dictionary.by.word <- dictionary.by.word[onegram[,.(prediction)],on=c(wordID="prediction")]

#stored dictioanry by ID and by Word and profanity as rds files
saveRDS(dictionary.by.id,"dictionary.by.id.rds")
saveRDS(dictionary.by.word,"dictionary.by.word.rds")
saveRDS(profanity,"profanity.rds")

#get list of words we don't want to predict
removeIDs <- dictionary.by.word[is.na(word) | 
                                  word %in% c("<start>", "<unk>", "<profanity>") |
                                  grepl("<NUM>",word)]$wordID



dbSendQuery(conn = ngramdb,
            "CREATE TABLE FIVEGRAM
            (cond1 INTEGER,
            cond2 INTEGER,
            cond3 INTEGER,
            cond4 INTEGER,
            prediction INTEGER,
            p float)")

dbSendQuery(conn = ngramdb,
            "CREATE INDEX FIVEGRAM_IX on FIVEGRAM
            (cond1, cond2, cond3, cond4, p DESC)")

fivegram <- readRDS("fivegram.prob.rds")
fivegram <- fivegram[!prediction %in% removeIDs]

dbWriteTable(conn = ngramdb, name = "FIVEGRAM", value = fivegram,
             append=TRUE)


dbSendQuery(conn = ngramdb,
            "CREATE TABLE FOURGRAM
            (cond1 INTEGER,
            cond2 INTEGER,
            cond3 INTEGER,
            prediction INTEGER,
            p float)")

dbSendQuery(conn = ngramdb,
            "CREATE INDEX FOURGRAM_IX on FOURGRAM
            (cond1, cond2, cond3, p DESC)")

fourgram <- readRDS("fourgram.prob.rds")
fourgram <- fourgram[!prediction %in% removeIDs]

dbWriteTable(conn = ngramdb, name = "FOURGRAM", value = fourgram,
             append=TRUE)

dbSendQuery(conn = ngramdb,
            "CREATE TABLE THREEGRAM
            (cond1 INTEGER,
            cond2 INTEGER,
            prediction INTEGER,
            p float)")

dbSendQuery(conn = ngramdb,
            "CREATE INDEX THREEGRAM_IX on THREEGRAM
            (cond1, cond2, p DESC)")

threegram <- readRDS("threegram.prob.rds")
threegram <- threegram[!prediction %in% removeIDs]

dbWriteTable(conn = ngramdb, name = "THREEGRAM", value = threegram,
             append=TRUE)

dbSendQuery(conn = ngramdb,
            "CREATE TABLE TWOGRAM
            (cond1 INTEGER,
            prediction INTEGER,
            p float)")

dbSendQuery(conn = ngramdb,
            "CREATE INDEX TWOGRAM_IX on TWOGRAM
            (cond1, p DESC)")

twogram <- readRDS("twogram.prob.rds")
twogram <- twogram[!prediction %in% removeIDs]

dbWriteTable(conn = ngramdb, name = "TWOGRAM", value = twogram,
             append=TRUE)

dbSendQuery(conn = ngramdb,
            "CREATE TABLE ONEGRAM
            (prediction INTEGER,
            p float)")

dbSendQuery(conn = ngramdb,
            "CREATE INDEX ONEGRAM_IX on ONEGRAM
            (p DESC)")

onegram <- onegram[!prediction %in% removeIDs]


dbWriteTable(conn = ngramdb, name = "ONEGRAM", value = onegram,
             append=TRUE)

dbSendQuery(conn = ngramdb,
            "CREATE TABLE DICTIONARY
            (word TEXT,
            wordID INTEGER)")

dbSendQuery(conn = ngramdb,
            "CREATE INDEX DICTIONARY_word_IX on DICTIONARY
            (word)")

dbSendQuery(conn = ngramdb,
            "CREATE INDEX DICTIONARY_wordID_IX on DICTIONARY
            (wordID)")

dbWriteTable(conn = ngramdb, name = "DICTIONARY", value = dictionary.by.id,
             append=TRUE)

dbSendQuery(ngramdb,"VACUUM")

dbDisconnect(ngramdb)




