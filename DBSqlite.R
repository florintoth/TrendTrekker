library("RSQLite")

#### Create Sqlite DB ####

# db <- dbConnect(SQLite(), dbname = "./data/DB/D.db")
# 
# for (i in 1:nrow(data.loc)){
#         data.Daily[[i]]$Date <- as.character(data.Daily[[i]]$Date)
#         dbWriteTable(db, gsub(".F","",data.loc$Ticker[i], fixed = TRUE), data.Daily[[i]], append = FALSE)
# }
# 
# dbDisconnect(db)
# 
# db <- dbConnect(SQLite(), dbname = "./data/DB/H1.db")
# 
# for (i in 1:nrow(data.loc)){
#         data.Hourly[[i]]$Date <- as.character(data.Hourly[[i]]$Date)
#         dbWriteTable(db, gsub(".F","",data.loc$Ticker[i], fixed = TRUE), data.Hourly[[i]], append = FALSE)
# }
# 
# dbDisconnect(db)
# 
# db <- dbConnect(SQLite(), dbname = "./data/DB/M05.db")
# 
# for (i in 1:nrow(data.loc)){
#         data.FiveMins[[i]]$Date <- as.character(data.FiveMins[[i]]$Date)
#         dbWriteTable(db, gsub(".F","",data.loc$Ticker[i], fixed = TRUE), data.FiveMins[[i]], append = FALSE)
# }
# 
# dbDisconnect(db)

#### Append Sqlite DB ####

if (as.character(weekdays(Sys.Date())) == "Monday") {
        lagSQL <- 3
        } else if ((as.character(weekdays(Sys.Date())) == "Tuesday") |
                (as.character(weekdays(Sys.Date())) == "Wednesday") |
                (as.character(weekdays(Sys.Date())) == "Thursday") |
                (as.character(weekdays(Sys.Date())) == "Friday") |
                (as.character(weekdays(Sys.Date())) == "Saturday")) {
                lagSQL <- 1
        } else if (as.character(weekdays(Sys.Date())) == "Sunday") {
                lagSQL <- 2}

lagSQL <- 2

flist <- list.files("./data/DB/", full.names = TRUE)
file.copy(flist, "./data/DBSafe/", overwrite = TRUE, copy.date = TRUE)
# file.copy(flist, "d:/DBSafe/", overwrite = TRUE, copy.date = TRUE)

db <- dbConnect(SQLite(), dbname = "./data/DB/D.db")
ow <- as.character(data.Daily[["EURUSD"]][substr(data.Daily[["EURUSD"]]$Date,1,10)==as.character(Sys.Date()-lagSQL),]$Date)==last(dbReadTable(db, "EURUSD")$Date)
dbDisconnect(db)

if (ow==FALSE) {
        
db <- dbConnect(SQLite(), dbname = "./data/DB/D.db")

for (i in 1:nrow(data.loc)){
        data.Daily[[i]]$Date <- as.character(data.Daily[[i]]$Date)
        dbWriteTable(db, gsub(".F","",data.loc$Ticker[i], fixed = TRUE), data.Daily[[i]][substr(data.Daily[[i]]$Date,1,10)==as.character(Sys.Date()-lagSQL),], append = TRUE)
}

dbDisconnect(db)

db <- dbConnect(SQLite(), dbname = "./data/DB/H1.db")

for (i in 1:nrow(data.loc)){
        data.Hourly[[i]]$Date <- as.character(data.Hourly[[i]]$Date)
        dbWriteTable(db, gsub(".F","",data.loc$Ticker[i], fixed = TRUE), data.Hourly[[i]][substr(data.Hourly[[i]]$Date,1,10)==as.character(Sys.Date()-lagSQL),], append = TRUE)
}

dbDisconnect(db)

db <- dbConnect(SQLite(), dbname = "./data/DB/M05.db")

for (i in 1:nrow(data.loc)){
        data.FiveMins[[i]]$Date <- as.character(data.FiveMins[[i]]$Date)
        dbWriteTable(db, gsub(".F","",data.loc$Ticker[i], fixed = TRUE), data.FiveMins[[i]][substr(data.FiveMins[[i]]$Date,1,10)==as.character(Sys.Date()-lagSQL),], append = TRUE)
}

dbDisconnect(db)
}
