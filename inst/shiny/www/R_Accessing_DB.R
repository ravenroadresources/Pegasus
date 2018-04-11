
library(dplyr)
library(ggplot2)
library(RODBC)
db <- file.path("D:/WORK/_DATABASE/OFM/aguasblancas.accdb")
channel <- odbcConnectAccess2007(db)

# list tables
tables <- sqlTables(channel)


# columns and properties
colnames(sqlColumns(channel, "DAILYPROD"))
## [1] "TABLE_CAT"         "TABLE_SCHEM"       "TABLE_NAME"        "COLUMN_NAME"
## [5] "DATA_TYPE"         "TYPE_NAME"         "COLUMN_SIZE"       "BUFFER_LENGTH"
## [9] "DECIMAL_DIGITS"    "NUM_PREC_RADIX"    "NULLABLE"          "REMARKS"
##[13] "COLUMN_DEF"        "SQL_DATA_TYPE"     "SQL_DATETIME_SUB"  "CHAR_OCTET_LENGTH"
##[17] "ORDINAL_POSITION"  "IS_NULLABLE"       "ORDINAL"

sqlColumns(channel, "DAILYPROD")$COLUMN_NAME


# preview data
dataprev <- sqlFetch(channel, "DAILYPROD", max = 10)


# retrieve data SQL
vars <- paste("UNIQUEID", "VOLUME_DATE", "OIL", "WATER", "GAS", sep = ",")
table <- "MONTHLYPROD"
rawdata <- sqlQuery(channel, paste("SELECT", vars, "FROM", table))

odbcClose(channel)




data <- rawdata %>%
  dplyr::mutate(bopd = ifelse(is.na(OIL), 0, OIL),
                bwpd = ifelse(is.na(WATER), 0, WATER),
                mscfd = ifelse(is.na(GAS), 0, GAS),
                well = gsub(":.*", "", UNIQUEID),
                fm = gsub(".*:", "", UNIQUEID),
                wcut = bwpd / ( bwpd + bopd),
                wor = bwpd / bopd,
                gor = mscfd / bopd ,
                dayspermonth = lubridate::days_in_month(VOLUME_DATE)) %>%
  dplyr::group_by(UNIQUEID) %>%
  dplyr::mutate(CumOil = cumsum(bopd) / 1e3,
                CumWat = cumsum(bwpd ) / 1e3,
                CumGas = cumsum(mscfd ) / 1e6,
                bopd = bopd / dayspermonth,
                bwpd = bwpd / dayspermonth,
                mscfd = mscfd / dayspermonth) %>%
  dplyr::ungroup()


  dataCh <- data %>%
  dplyr::filter(!is.na(wor),
    wor > 0) %>%
  dplyr::group_by(UNIQUEID) %>%
  dplyr::mutate(ndays = row_number() *30,
                wor1 = (dplyr::lag(wor, n = 1L) - dplyr::lag(wor, n = 0)) / (dplyr::lag(ndays, n = 1L) - dplyr::lag(ndays, n = 0)),
                gor1 = (dplyr::lag(gor, n = 1L) - dplyr::lag(gor, n = 0)) / (dplyr::lag(ndays, n = 1L) - dplyr::lag(ndays, n = 0))) %>%
  dplyr::select(UNIQUEID, fm, ndays, CumOil, CumWat, wor, wor1, gor, gor1)


dataCh %>%
  ggplot(aes(x = CumOil, y = wor)) +
    geom_line(color = "blue") +
    geom_point(aes(y = wor1), color = "darkred", alpha = 0.1) +
    facet_wrap(~UNIQUEID, scales = "free_x") +
    scale_y_log10() +
    #scale_x_log10() +
    theme_bw() +
    ylab("WOR  -  WOR'") +
    xlab("Time [days]") +
    geom_smooth(aes(y = wor1), color = "black")



#prod data
data %>%
  #dplyr::filter(lubridate::year(VOLUME_DATE) > 2016) %>%
  dplyr::select(UNIQUEID, VOLUME_DATE, bopd, bwpd) %>%
  reshape2::melt(id = c("VOLUME_DATE", "UNIQUEID")) %>%
  dplyr::mutate(fluid = factor(variable, levels = c("bwpd", "bopd"), ordered = TRUE),
                date = lubridate::as_date(VOLUME_DATE)) %>%
  ggplot(aes(x = VOLUME_DATE, y = as.numeric(value), color = fluid)) +
    geom_line(position = "stack", size = 1) +
    facet_wrap(~UNIQUEID, ncol = 4, scales = "free_y") +
    scale_color_manual(values = c("blue", "green")) +
    ylab("BFPD") +
    theme_bw()





################################################################################
################################################################################
library(odbc)
con <- odbc::dbConnect(odbc::odbc(), "test_db")

con <- odbc::dbConnect(odbc::odbc(), .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=gigante.accdb")

con <- odbc::dbConnect()

DBI::dbDriver()

