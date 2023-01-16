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

dolar_compra <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.10813/dados?formato=json", "dolar_compra")
dolar_venda <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.1/dados?formato=json", "dolar_venda")


fit <- fabletools::model(filter(dolar_compra, data >= lubridate::ymd("2000/01/01")),
                         arima = ARIMA(valor),
                         ets = ETS(valor),
                         theta = THETA(valor),
                         RW = SNAIVE(valor)
                         )

fit %>% accuracy()

fc <- fit %>% forecast(h = "30 days")
fc
fc %>%  autoplot()
