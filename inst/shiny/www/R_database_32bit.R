library(RODBC) 
db <- file.path("D:/WORK/GUADUAS/PROD/db1.accdb") 
channel <- odbcConnectAccess2007(db) 


# list tables
tables <- sqlTables(channel)


# columns and properties
colnames(sqlColumns(channel, "DAILYPROD"))
 [1] "TABLE_CAT"         "TABLE_SCHEM"       "TABLE_NAME"        "COLUMN_NAME"      
 [5] "DATA_TYPE"         "TYPE_NAME"         "COLUMN_SIZE"       "BUFFER_LENGTH"    
 [9] "DECIMAL_DIGITS"    "NUM_PREC_RADIX"    "NULLABLE"          "REMARKS"          
[13] "COLUMN_DEF"        "SQL_DATA_TYPE"     "SQL_DATETIME_SUB"  "CHAR_OCTET_LENGTH"
[17] "ORDINAL_POSITION"  "IS_NULLABLE"       "ORDINAL"    

sqlColumns(channel, "DAILYPROD")$COLUMN_NAME


# preview data
dataprev <- sqlFetch(channel, "DAILYPROD", max = 10)

# retrieve data SQL
data <- sqlQuery(channel, paste('SELECT "UNIQUE_ID", "VOLUME_DATE", "OIL", "WATER", "GAS" FROM "DAILYPROD" '))


vars <- paste("UNIQUEID", "OIL", "WATER", sep = ",")
table <- "DAILYPROD"
data <- sqlQuery(channel, paste("SELECT", vars, "FROM", table))


cond <- "`UNIQUEID` = `SEGU0002S:CIMARRONA`"
data <- sqlQuery(channel, paste("SELECT", vars, "FROM", table, "WHERE", cond))


data <- sqlQuery(channel, "SELECT `OIL` FROM `DAILYPROD` WHERE `UNIQUEID`= `SEGU0002S:CIMARRONA`")
data <- sqlQuery(channel, paste("SELECT", vars, "FROM", table))


data <- sqlQuery(channel, paste("SELECT", vars, "FROM", table))

, "WHERE", 
paste("UNIQUEID", "=", "SEGU0002S:CIMARRONA", sep = "")))  




data <- sqlQuery(channel, paste("
    SELECT 
    OIL  
    FROM 
    DAILYPROD
    WHERE 
    UNIQUEID = SEGU0002S:CIMARRONA  
    ",  sep=""))



as.character(chstudid),


tables <- DBI::dbListTables(channel)

