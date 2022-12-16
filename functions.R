# TCC forecast: helper functions
# Bernardo Cainelli Gomes da Silva
# Nov - 2022

# _____________________________________________________________________________
# __________________________________ PACKAGES __________________________________
# _____________________________________________________________________________
{
  pacotes_necessarios <- list("lubridate", "tidyverse", "httr", "jsonlite")
  
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

# fuction to download series from BACEN API
get_sgs <- function(api_url){
  x <- GET(api_url)
  y <- fromJSON(rawToChar(x$content))
  y$data <- dmy(y$data)
  y$valor <- as.double(y$valor)
  colnames(y) <- c("data", "valor")
  return(y)
}
