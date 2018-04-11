library(dplyr)
library(ggplot2)

setwd("D:/WORK/_DATABASE")

## Daily Data
rawdata <- readxl::read_excel("GDY_VAS_GCM_DATABASE_DailyProd.xlsx", sheet = "ABANICO") %>%
  dplyr::mutate(well = gsub(":.*", "", ID),
                fm = gsub(".*:", "", ID),
                wcut = bwpd / ( bwpd + bopd),
                wor = bwpd / bopd,
                gor = mscfd / bopd * 1e3) %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(CumOil = cumsum(bopd) / 1e3,
                CumWat = cumsum(bwpd) / 1e3,
                CumGas = cumsum(mscfd) / 1e3) %>%
  dplyr::ungroup() 



## Monthly Data
rawdata_m <- readxl::read_excel("GDY_VAS_GCM_DATABASE_DailyProd.xlsx", sheet = "ABANICO_Monthly") %>%
  dplyr::mutate(well = gsub(":.*", "", ID),
                fm = gsub(".*:", "", ID),
                wcut = bwpd / ( bwpd + bopd),
                wor = bwpd / bopd,
                gor = mscfd / bopd * 1e3,
                dayspermonth = lubridate::days_in_month(Date)) %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(CumOil = cumsum(bopd) / 1e3,
                CumWat = cumsum(bwpd ) / 1e3,
                CumGas = cumsum(mscfd ) / 1e3,
                bopd = bopd / dayspermonth,
                bwpd = bwpd / dayspermonth,
                mscfd = mscfd / dayspermonth) %>%
  dplyr::ungroup() 

## Stat data
aa <- rawdata_m %>%
  dplyr::group_by(fm, well) %>%
  dplyr::summarize(Np = max(CumOil, na.rm = TRUE),
                   Nw = max(CumWat, na.rm = TRUE),
                   Ng = max(CumGas, na.rm = TRUE)) %>%
  #dplyr::select(-Np, -Nw) %>%
  #tidyr::spread(fm, Ng) %>%
  dplyr::select(Np) %>%
  petroreadr::summary_mod()
  ggplot(aes(x = Np, fill = fm)) + geom_histogram() + theme_bw()
  write.csv("CumProd.csv")


## commingled vs recompletion
aa <- 
rawdata_m %>%
  dplyr::filter(well == "ABAN0007" | well == "ABAN0009" | well == "ABAN0011" | well == "ABAN0015"
    | well == "ABAN0016" | well == "ABAN0018" | well == "ABAN0024" | well == "ABAN0026" | well == "ABAN0033") %>%
  dplyr::select(well, fm, Date, bopd, bwpd) %>%
  reshape2::melt(id = c("Date", "well", "fm")) %>%
  # dplyr::order_by(c("well", "date")) %>%
  dplyr::mutate(fluid = factor(variable, levels = c("bwpd", "bopd"), ordered = TRUE),
                date = lubridate::as_date(Date))  %>%
  ggplot(aes(x = Date, y = as.numeric(value), color = fluid, linetype = fm)) +
    geom_line(position = "stack", size = 1) + 
    #facet_wrap(~well, scales = "free", ncol = 6) + 
    facet_wrap(~well, ncol = 2, scales = "free_y") +
    scale_color_manual(values = c("blue", "green")) + 
    ylab("BFPD") +
    theme_bw()


## ------------------------------------------------------------------------
## PROD

rawdata_m %>%
  #dplyr::filter(fm == "UGA") %>%
  dplyr::filter(well == "ABAN0007") %>%
  dplyr::select(ID, Date, bopd, bwpd) %>%
  reshape2::melt(id = c("Date", "ID")) %>%
  # dplyr::order_by(c("well", "date")) %>%
  dplyr::mutate(fluid = factor(variable, levels = c("bwpd", "bopd"), ordered = TRUE),
                date = lubridate::as_date(Date)) %>%
  ggplot(aes(x = Date, y = as.numeric(value), color = fluid)) +
    geom_line(position = "stack", size = 1) + 
    #facet_wrap(~well, scales = "free", ncol = 6) + 
    facet_wrap(~ID, ncol = 6, scales = "free_y") +
    scale_color_manual(values = c("blue", "green")) + 
    ylab("BFPD") +
    theme_bw()
     +
    scale_x_date(labels = scales::date_format("%d/%b/%y")) 

## ------------------------------------------------------------------------
## WCUT vs Cum Oil

ggplot(rawdata_m, aes(x = CumOil, y = wcut, color = fm)) +
  geom_point()

rawdata %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(ncumoil = CumOil / max(CumOil)) %>%
  ggplot(aes(x = ncumoil, y = wcut, color = well)) +
  geom_line(size = 1.2) +
  theme_bw() +
  xlab("Normalized Np")

rawdata_m %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(ncumoil = CumOil / max(CumOil)) %>%
  ggplot(aes(x = ncumoil, y = wcut, color = fm)) +
  #geom_line(color = "blue") +
  geom_point() +
  #facet_wrap(~ID) +
  theme_bw() +
  xlab("Normalized Np") +
  geom_smooth()

ggplot(rawdata, aes(x = CumOil, y = wcut, color = well)) +
  geom_line(color = "blue") +
  facet_wrap(~ID)

## ------------------------------------------------------------------------
## WOR and GOR

ggplot(rawdata_m, aes(x = CumOil, y = wor, linetype = fm)) +
  geom_line(color = "blue") +
  geom_line(aes(y = gor), color = "orange") +
  facet_wrap(~ID, scales = "free_x") +
  scale_y_log10() +
  theme_bw() +
  ylab("WOR  -  GOR [scf(stb)]") + 
  xlab("CumOil [Mstb]")


ggplot(rawdata_m, aes(x = CumOil, y = CumGas, color = fm)) +
  geom_point() + #color = "orange") +
  facet_wrap(~ID, scales = "free") +
  theme_bw() +
  geom_abline(slope = 0.2, linetype = 2) +
  geom_abline(slope = 0.5, linetype = 2) +
  geom_abline(slope = 1.0, linetype = 2) +
  geom_abline(slope = 2.0, linetype = 2) +
  ylab("Cum Gas [MMcf]") + 
  xlab("CumOil [Mstb]")


ggplot(rawdata_m, aes(x = CumOil, y = CumWat, color = fm)) +
  geom_point() + #color = "orange") +
  #facet_wrap(~ID, scales = "free") +
  theme_bw() +
  geom_abline(slope = 0.1, linetype = 2) +
  geom_abline(slope = 1, linetype = 2) +
  geom_abline(slope = 10, linetype = 2) +
  geom_abline(slope = 19, linetype = 2) +
  ylab("CumWat [Mstb]") + 
  xlab("CumOil [Mstb]") 



ggplot(rawdata_m, aes(x = Date, y = gor)) +
  geom_line(color = "orange") +
  facet_wrap(~ID, scales = "free") +
  theme_bw() 


## ------------------------------------------------------------------------
## CHAN PLOT
data <- rawdata_m %>%
  dplyr::filter(!is.na(wor),
    wor > 0) %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(ndays = row_number(),
                wor1 = (dplyr::lag(wor, n = 1L) - dplyr::lag(wor, n = 0)) / (dplyr::lag(ndays, n = 1L) - dplyr::lag(ndays, n = 0)),
                gor1 = (dplyr::lag(gor, n = 1L) - dplyr::lag(gor, n = 0)) / (dplyr::lag(ndays, n = 1L) - dplyr::lag(ndays, n = 0))) %>%
  dplyr::select(ID, fm, ndays, CumOil, CumWat, wor, wor1, gor, gor1)

datam <- rawdata %>%
  dplyr::filter(!is.na(wor),
    wor > 0) %>%
  dplyr::group_by(ID, paste(lubridate::year(Date), ifelse(lubridate::month(Date) < 10, 
                                                          paste0(0, lubridate::month(Date)),
                                                          lubridate::month(Date)), sep = "_")) %>%
  dplyr::summarize(Oil = sum(bopd, na.rm = TRUE),
                   Wat = sum(bwpd, na.rm = TRUE),
                   wor = Wat / Oil) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(ndays = row_number() * 30,
                wor1 = (dplyr::lag(wor, n = 1L) - dplyr::lag(wor, n = 0)) / (dplyr::lag(ndays, n = 1L) - dplyr::lag(ndays, n = 0)))


data %>%
  dplyr::filter(fm == "UGA") %>%
  ggplot(aes(x = ndays, y = wor)) +
    geom_line(color = "blue") +
    geom_point(aes(y = wor1), color = "darkred", alpha = 0.1) +
    facet_wrap(~ID, scales = "free_x") +
    scale_y_log10() +
    scale_x_log10() +
    theme_bw() +
    ylab("WOR  -  WOR'") + 
    xlab("Time [days]") +
    geom_smooth(aes(y = wor1), color = "black")


ggplot(datam, aes(x = ndays, y = wor)) +
  geom_line(color = "blue") +
  geom_point(aes(y = wor1), color = "darkred", alpha = 0.1) +
  facet_wrap(~ID, scales = "free_x") +
  scale_y_log10() +
  scale_x_log10() +
  theme_bw() +
  ylab("WOR  -  WOR'") + 
  xlab("Time [days]") +
  geom_smooth(aes(y = wor1), color = "black")


ggplot(data, aes(x = ndays, y = gor)) +
  geom_line(color = "orange") +
  geom_point(aes(y = gor1), color = "purple", alpha = 0.1) +
  facet_wrap(~ID, scales = "free_x") +
  scale_y_log10() +
  scale_x_log10() +
  theme_bw() +
  ylab("GOR  -  GOR'") + 
  xlab("Time [days]") +
  geom_smooth(aes(y = gor1), color = "black")


data %>%
  dplyr::filter(ID == "SEGU0001E:CIMARRONA" | ID == "TPAS0001E:CIMARRONA") %>%
  write.csv("aaa.csv")


write.csv(datam, "datam.csv")