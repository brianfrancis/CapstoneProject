sqlite    <- dbDriver("SQLite")
ngramdb <- dbConnect(sqlite,"ngram.db")

dbSendQuery(conn = ngramdb,
            'CREATE TABLE FOURGRAM
            (cond1 INTEGER,
            cond2 INTEGER,
            cond3 INTEGER,
            prediction INTEGER,
            p float)')

dbSendQuery(conn = ngramdb,
            'CREATE INDEX FOURGRAM_IX on FOURGRAM
            (cond1, cond2, cond3)')

dbWriteTable(conn = ngramdb, name = 'FOURGRAM', value = 'fourgram.prob.csv',
             row.names = FALSE, header = TRUE, append=TRUE)

dbSendQuery(conn = ngramdb,
            'CREATE TABLE THREEGRAM
            (cond1 INTEGER,
            cond2 INTEGER,
            prediction INTEGER,
            p float)')

dbSendQuery(conn = ngramdb,
            'CREATE INDEX THREEGRAM_IX on THREEGRAM
            (cond1, cond2)')

dbWriteTable(conn = ngramdb, name = 'THREEGRAM', value = 'threegram.prob.csv',
             row.names = FALSE, header = TRUE, append=TRUE)


dbSendQuery(conn = ngramdb,
            'CREATE TABLE TWOGRAM
            (cond1 INTEGER,
            prediction INTEGER,
            p float)')

dbSendQuery(conn = ngramdb,
            'CREATE INDEX TWOGRAM_IX on TWOGRAM
            (cond1)')

dbWriteTable(conn = ngramdb, name = 'TWOGRAM', value = 'twogram.prob.csv',
             row.names = FALSE, header = TRUE, append=TRUE)



