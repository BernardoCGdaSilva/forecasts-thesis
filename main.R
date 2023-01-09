# TCC forecast
# Bernardo Cainelli Gomes da Silva
# Nov - 2022

# _________________________________________________________________________________
# _____________________________ HELPER FUNCTIONS __________________________________
# _________________________________________________________________________________

source("functions.R")

# _________________________________________________________________________________
# __________________________________ DATA _________________________________________
# _________________________________________________________________________________

# get data

dolar_compra <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.10813/dados?formato=json")
dolar_venda <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.1/dados?formato=json")



snowy <- tourism %>%
  filter(
    Region == "Snowy Mountains",
    Purpose == "Holiday"
  )
snowy

fit <- snowy %>%
  model(
    #snaive = SNAIVE(Trips ~ lag("year")),
    #ets = ETS(Trips),
    arima = ARIMA(Trips)
  )
fit

fc <- fit %>%
  forecast(h = 12)
fc

fc %>%
  autoplot(snowy) +
  ggtitle("Forecasts for Snowy Mountains holidays") +
  xlab("Year") +
  guides(colour = guide_legend(title = "Forecast"))

