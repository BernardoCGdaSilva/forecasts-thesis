# TCC forecast: helper functions
# Bernardo Cainelli Gomes da Silva
# Nov - 2022

# _____________________________________________________________________________
# __________________________________ PACKAGES __________________________________
# _____________________________________________________________________________
{
  pacotes_necessarios <- list(
    "lubridate", "tidyverse", "httr",
    "jsonlite", "tsibble", "fable", "feasts", "sidrar",
    "ipeadatar", "neverhpfilter", "caret", "doParallel",
    "fredr", "tempdisagg", "imfr"
  )

  for (i in pacotes_necessarios) {
    eval(bquote(
      if (!require(.(i))) {
        install.packages(.(i))
        library(.(i))
      }
    ))
  }

  rm(pacotes_necessarios, i)
}

# _______________________________________________________________________________
# __________________________________ FUNCTIONS __________________________________
# _______________________________________________________________________________

# fuction to download time series from BACEN API
get_sgs <- function(api_url, nome_coluna) { # ,nome_variavel){
  x <- httr::GET(api_url)
  y <- jsonlite::fromJSON(rawToChar(x$content))
  y$data <- lubridate::dmy(y$data)
  y$valor <- as.double(y$valor)
  colnames(y) <- c("date", nome_coluna)
  # y <- mutate(y, variavel = nome_variavel)
  # z <- tsibble::as_tsibble(y, index = data, key = variavel)
  # z <- arrange(z, data)
  # z <- z %>% fill_gaps(.full = FALSE) %>% fill(valor)
  return(y)
}

# Transform to timeseries
make_ts_month <- function(base) {
  x <- as.matrix(base$valor)
  ano <- min(lubridate::year(base$data))
  mes <- lubridate::month(min(base$data))
  y <- ts(x, frequency = 12, start = c(ano, mes))
  return(y)
}

# Convert from dail to monthly and keeps last daily value
day_to_month <- function(base) {
  nome_coluna <- colnames(base)[2]
  x <- base %>%
    dplyr::mutate(mes = lubridate::floor_date(date, "month")) %>%
    group_by(mes) %>%
    summarise(valor = last(!!rlang::ensym(nome_coluna)))
  # colnames(x) <- c("data", "valor")
  return(x)
}

# Calculate output gap via Hamilton Filter from IMF GDP Data panel, quarterly data
output_gap_q <- function(data, filtro = "BR", colname = "br_gap") {
  gdp <- data %>%
    dplyr::filter(ref_area == filtro) %>%
    dplyr::select(c("date", "value"))
  gdp$value <- as.double(gdp$value)
  gdp$date <- stringr::str_remove_all(gdp$date, "Q") %>%
    zoo::as.yearqtr() %>%
    as.Date(frac = 1)
  hf <- neverhpfilter::yth_filter(xts::as.xts(gdp), h = 8, p = 4)
  gap <- gdp$value / hf$value.trend
  gap <- as.data.frame(gap) %>%
    rownames_to_column() %>%
    `colnames<-`(c("date", colname))
  gap$date <- lubridate::ymd(gap$date)
  return(gap)
}

# Calculate output gap via Hamilton Filter, monthly data
output_gap_m <- function(data, colname = "br_gap") {
  hf <- neverhpfilter::yth_filter(xts::as.xts(data[,1:2]), h = 24, p = 12)
  gap <- data$value / hf$value.trend
  gap <- as.data.frame(gap) %>%
    rownames_to_column() %>%
    `colnames<-`(c("date", colname))
  gap$date <- lubridate::ymd(gap$date)
  return(gap)
}

# disaggregate data from lower frequency to higher (quarterly to monthly)
quartet_to_monthly <- function(data_gdp, data_ip, filtro = "BR", colname = "br_gdp", gdp_ano_0, gdp_quarter_0, ip_ano_0, ip_mes_0) {
  gdp <- data_gdp %>%
    filter(ref_area == filtro) %>%
    select(c(date, value))
  gdp$value <- as.double(gdp$value)
  gdp$date <- stringr::str_remove_all(gdp$date, "Q") %>%
    zoo::as.yearqtr() %>%
    as.Date()
  gdp_ts <- ts(gdp$value, start = c(gdp_ano_0, gdp_quarter_0), frequency = 4)
  ip_ts <- ts(data_ip$value, start = c(ip_ano_0, ip_mes_0), frequency = 12) %>% forecast::na.interp()

  gdp_monthly_fit <- tempdisagg::td(gdp_ts ~ ip_ts, conversion = "first", to = "month") %>% predict()
  gdp_monthly <- data.frame(date = as.Date(time(gdp_monthly_fit)), value = as.matrix(gdp_monthly_fit))

  gdp_monthly <- gdp_monthly %>% filter(date >= min(gdp$date) & date <= max(gdp$date))

  plot <- ggplot() +
    geom_line(gdp_monthly, mapping = aes(x = date, y = value, color = "monthly")) +
    geom_line(gdp, mapping = aes(x = date, y = value, color = "quartely")) +
    # tidyquant::coord_x_date(xlim = c("2018-01-01", max(gdp$date)))+
    scale_color_manual(name = "", values = c("monthly" = "blue", "quartely" = "red")) +
    labs(title = filtro)

  # colnames(gdp_monthly) <- c("date", colname)

  print(plot)
  return(gdp_monthly)
}

# filter IFS exchage rate panel by country
filter_ifs <- function(data, filter = "BR", colname = "br_exchange") {
  x <- filter(data, ref_area == filter) %>% select(c(date, value))
  x$date <- lubridate::ym(x$date)
  x$value <- as.double(x$value)
  colnames(x) <- c("date", colname)
  return(x)
}

# Get RMSE from random walk
rmse_rw <- function(cty) {
  data <- read_csv2(paste0("outputs/", cty, "_panel_data.csv"), col_types = "Dddddddddddd") %>% arrange(date)

  rw <- data[, c(1, 3, 4)] %>%
    `colnames<-`(c("data", "var_h1", "var_h12")) %>%
    # mutate(
    #  rw1 = lag(var_h1, 1),
    #  rw12 = lag(var_h12, 12)
    # ) %>%
    slice_tail(n = -120) # %>%
  # mutate(
  #  se1 = (rw1)**2,
  #  se12 = (rw12)**2
  # )

  # rmse_rw_h1 <- rw$se1 %>%
  #  mean() %>%
  #  sqrt()
  # rmse_rw_h12 <- rw$se12 %>%
  #  mean() %>%
  #  sqrt()

  # rmse_rw_h1 <- RMSE(pred = rw$rw1, obs = rw$var_h1)
  # rmse_rw_h12 <- RMSE(pred = rw$rw12, obs = rw$var_h12)

  error_rw_h1 <- rw$var_h1**2
  error_rw_h12 <- rw$var_h12**2

  rmse_rw_h1 <- error_rw_h1 %>%
    mean() %>%
    sqrt()
  rmse_rw_h12 <- error_rw_h12 %>%
    mean() %>%
    sqrt()

  return(list(
    "rmse_rw" = c(rmse_rw_h1, rmse_rw_h12),
    "error_rw_h1" = error_rw_h1,
    "error_rw_h12" = error_rw_h12
  ))
}
