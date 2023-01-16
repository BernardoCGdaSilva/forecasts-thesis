# TCC forecast: helper functions
# Bernardo Cainelli Gomes da Silva
# Nov - 2022

# _____________________________________________________________________________
# __________________________________ PACKAGES __________________________________
# _____________________________________________________________________________
{
  pacotes_necessarios <- list("lubridate", "tidyverse", "httr",
                              "jsonlite", "tsibble", "fable", "feasts")

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
get_sgs <- function(api_url,nome_variavel){
  x <- httr::GET(api_url)
  y <- jsonlite::fromJSON(rawToChar(x$content))
  y$data <- lubridate::dmy(y$data)
  y$valor <- as.double(y$valor)
  colnames(y) <- c("data", "valor")
  y <- mutate(y, variavel = nome_variavel)
  z <- tsibble::as_tsibble(y, index = data, key = variavel)
  z <- arrange(z, data)
  z <- z %>% fill_gaps(.full = FALSE) %>% fill(valor)
  return(z)
}
