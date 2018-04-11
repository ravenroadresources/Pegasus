data_r <- shiny::reactive({
  rawdata_r() %>%
  dplyr::mutate(bopd = ifelse(is.na(OIL), 0, as.numeric(OIL)),
                bwpd = ifelse(is.na(WATER), 0, as.numeric(WATER)),
                mscfd = ifelse(is.na(GAS), 0, as.numeric(GAS)),
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
})

dataChan_r <- shiny::reactive({
  data_r() %>%
  dplyr::filter(!is.na(wor),
    wor > 0) %>%
  dplyr::group_by(UNIQUEID) %>%
  dplyr::mutate(ndays = row_number() * 30,
                CumFluid = CumOil + CumWat,
                wor1_Time = (dplyr::lag(wor, n = 1L) - dplyr::lag(wor, n = 0)) / (dplyr::lag(ndays, n = 1L) - dplyr::lag(ndays, n = 0)),
                gor1_Time = (dplyr::lag(gor, n = 1L) - dplyr::lag(gor, n = 0)) / (dplyr::lag(ndays, n = 1L) - dplyr::lag(ndays, n = 0)),
                wor1_CumOil = (dplyr::lag(wor, n = 1L) - dplyr::lag(wor, n = 0)) / (dplyr::lag(CumOil, n = 1L) - dplyr::lag(CumOil, n = 0)),
                gor1_CumOil = (dplyr::lag(gor, n = 1L) - dplyr::lag(gor, n = 0)) / (dplyr::lag(CumOil, n = 1L) - dplyr::lag(CumOil, n = 0)),
                wor1_CumFluid = (dplyr::lag(wor, n = 1L) - dplyr::lag(wor, n = 0)) / (dplyr::lag(CumFluid, n = 1L) - dplyr::lag(CumFluid, n = 0)),
                gor1_CumFluid = (dplyr::lag(gor, n = 1L) - dplyr::lag(gor, n = 0)) / (dplyr::lag(CumFluid, n = 1L) - dplyr::lag(CumFluid, n = 0))) %>%
  dplyr::rename(Time = ndays) %>%
  dplyr::select(UNIQUEID, fm, Time, CumOil, CumFluid, wor, wor1_Time, wor1_CumOil, wor1_CumFluid, gor, gor1_Time, gor1_CumOil, gor1_CumFluid)
})

###################################################################################

output$data_summary <- shiny::renderTable({
  data <- data_r()
  head(data)
})


output$plot_rates <- shiny::renderPlot({
data_r() %>%
  #dplyr::filter(lubridate::year(VOLUME_DATE) > 2016) %>%
  dplyr::select(UNIQUEID, VOLUME_DATE, bopd, bwpd) %>%
  reshape2::melt(id = c("VOLUME_DATE", "UNIQUEID")) %>%
  dplyr::mutate(fluid = factor(variable, levels = c("bwpd", "bopd"), ordered = TRUE),
                date = lubridate::as_date(VOLUME_DATE)) %>%
  ggplot2::ggplot(ggplot2::aes(x = VOLUME_DATE, y = as.numeric(value), color = fluid)) +
    ggplot2::geom_line(position = "stack", size = 1) +
    ggplot2::facet_wrap(~UNIQUEID, ncol = 4, scales = "free_y") +
    ggplot2::scale_color_manual(values = c("blue", "green")) +
    ggplot2::ylab("BFPD") +
    ggplot2::theme_bw()
})


wcut_color_r <- reactive({
  input$plot_wcut1_color
})

### WCUT
output$plot_wcut1 <- shiny::renderPlot({
  color_ <- wcut_color_r()
  data <- data_r()
  ggplot2::ggplot(data, ggplot2::aes(x = CumOil, y = wcut, color = data[[color_]])) +
    ggplot2::geom_point()
})

output$plot_wcut2 <- shiny::renderPlot({
  data_r()  %>%
  dplyr::group_by(UNIQUEID) %>%
  dplyr::mutate(ncumoil = CumOil / max(CumOil)) %>%
  ggplot2::ggplot(ggplot2::aes(x = ncumoil, y = wcut, color = fm)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~UNIQUEID) +
    ggplot2::theme_bw() +
    ggplot2::xlab("Normalized Np")
})

### WOR and GOR
output$plot_worgor <- shiny::renderPlot({
  ggplot2::ggplot(data_r(), ggplot2::aes(x = CumOil, y = wor, linetype = fm)) +
    ggplot2::geom_line(color = "blue") +
    ggplot2::geom_line(ggplot2::aes(y = gor), color = "orange") +
    ggplot2::facet_wrap(~UNIQUEID, scales = "free_x") +
    ggplot2::scale_y_log10() +
    ggplot2::theme_bw() +
    ggplot2::ylab("WOR  -  GOR [scf/stb]") +
    ggplot2::xlab("CumOil [Mstb]")
})

output$plot_cumgor <- shiny::renderPlot({
ggplot2::ggplot(data_r(), ggplot2::aes(x = CumOil, y = CumGas, color = fm)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~UNIQUEID, scales = "free") +
  ggplot2::theme_bw() +
  ggplot2::geom_abline(slope = input$gor1 / 1e6, linetype = 2) +
  ggplot2::geom_abline(slope = input$gor2 / 1e6, linetype = 2) +
  ggplot2::geom_abline(slope = input$gor3 / 1e6, linetype = 2) +
  ggplot2::geom_abline(slope = input$gor4 / 1e6, linetype = 2) +
  ggplot2::ylab("Cum Gas [MMcf]") +
  ggplot2::xlab("CumOil [Mstb]")
})

output$plot_gortime <- shiny::renderPlot({
ggplot2::ggplot(data_r(), ggplot2::aes(x = VOLUME_DATE, y = gor)) +
  ggplot2::geom_line(color = "orange") +
  ggplot2::facet_wrap(~UNIQUEID, scales = "free") +
  ggplot2::theme_bw()
})


output$plot_chan <- shiny::renderPlot({
  data <- dataChan_r()

  if (input$chan_var == "WOR") {
    data %>%
    ggplot2::ggplot(ggplot2::aes(x = data[[input$chan_xaxis]], y = wor)) +
      ggplot2::geom_line(color = "blue") +
      ggplot2::geom_point(ggplot2::aes(y = data[[paste0("wor1_", input$chan_xaxis)]]), color = "darkred", alpha = 0.1) +
      ggplot2::facet_wrap(~UNIQUEID, scales = "free_x") +
      ggplot2::scale_y_log10() +
      ggplot2::scale_x_log10() +
      ggplot2::theme_bw() +
      ggplot2::ylab("WOR  -  WOR'") +
      ggplot2::xlab(input$chan_xaxis) +
      ggplot2::geom_smooth(ggplot2::aes(y = data[[paste0("wor1_", input$chan_xaxis)]]), color = "black")
  }
  else if (input$chan_var == "GOR") {
    ggplot2::ggplot(data, ggplot2::aes(x = data[[input$chan_xaxis]], y = gor)) +
    ggplot2::geom_line(color = "orange") +
    ggplot2::geom_point(ggplot2::aes(y = data[[paste0("gor1_", input$chan_xaxis)]]), color = "purple", alpha = 0.1) +
    ggplot2::facet_wrap(~UNIQUEID, scales = "free_x") +
    ggplot2::scale_y_log10() +
    ggplot2::scale_x_log10() +
    ggplot2::theme_bw() +
    ggplot2::ylab("GOR  -  GOR'") +
    ggplot2::xlab(input$chan_xaxis) +
    ggplot2::geom_smooth(ggplot2::aes(y = data[[paste0("gor1_", input$chan_xaxis)]]), color = "black")
  }

})
