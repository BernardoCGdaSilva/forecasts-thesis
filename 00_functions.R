# TCC forecast: helper functions
# Bernardo Cainelli Gomes da Silva
# Nov - 2022

# _____________________________________________________________________________
# __________________________________ PACKAGES __________________________________
# _____________________________________________________________________________
{
  pacotes_necessarios <- list("lubridate", "tidyverse", "httr",
                              "jsonlite", "tsibble", "fable", "feasts", "sidrar")

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
get_sgs <- function(api_url, nome_coluna){#,nome_variavel){
  x <- httr::GET(api_url)
  y <- jsonlite::fromJSON(rawToChar(x$content))
  y$data <- lubridate::dmy(y$data)
  y$valor <- as.double(y$valor)
  colnames(y) <- c("data", nome_coluna)
  #y <- mutate(y, variavel = nome_variavel)
  #z <- tsibble::as_tsibble(y, index = data, key = variavel)
  #z <- arrange(z, data)
  #z <- z %>% fill_gaps(.full = FALSE) %>% fill(valor)
  return(y)
}

# Transform to timeseries
make_ts_month <- function(base){
  x <- as.matrix(base$valor)
  ano <- min(lubridate::year(base$data))
  mes <- lubridate::month(min(base$data))
  y <- ts(x, frequency = 12, start = c(ano, mes))
  return(y)
}

# Convert from dail to monthly and keeps last daily value
day_to_month <- function(base){
  nome_coluna <- colnames(base)[2]
  x <- base %>%
    mutate(mes = lubridate::floor_date(data, "month")) %>%
    group_by(mes) %>%
    summarise(valor = last(!! rlang::ensym(nome_coluna)))
  #colnames(x) <- c("data", "valor")
  return(x)
}
