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

#dolar_compra_diario <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.10813/dados?formato=json","dolar_compra_diario")#, "dolar_compra")
#dolar_venda_diario <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.1/dados?formato=json","dolar_venda_diario")#, "dolar_venda")
dolar_compra_mensal <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.3695/dados?formato=json", "dolar_compra_mensal")
dolar_venda_mensal <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.3695/dados?formato=json", "dolar_venda_mensal")
#M1_diario <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.27785/dados?formato=json","M1_diario")#, "base_monetaria_m1")
M1_mensal <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.27791/dados?formato=json", "M1_mensal")
#M1_defasado_diario <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.1821/dados?formato=json", "base_monetaria_m1")
inflacao_mensal <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.433/dados?formato=json", "inflacao_mensal")#, "ipca")
selic_diario <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.432/dados?formato=json", "selic_diario")
pib_mensal <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.4380/dados?formato=json", "pib_mensal")

# Daily data to monthly data: last value

selic_mensal <- selic_diario %>%
  mutate(mes = lubridate::floor_date(data, "month")) %>%
  group_by(data) %>%
  summarise(selic_mensal = last(selic_diario))

#####a <- day_to_month(selic_diario)

# Build panel

painel <- reduce(list(dolar_compra_mensal, dolar_venda_mensal, M1_mensal,inflacao_mensal, selic_mensal, pib_mensal),full_join, by = "data")
painel <- painel[rowSums(is.na(painel)) == 0,]
#teste <- painel[painel$dolar_compra_mensal != painel$dolar_venda_mensal,] # Para averiguar se há diferença entre os valores de compra e venda do dólar.

dolar_ts <- make_ts_month(dolar_compra_mensal)
M1_ts <- make_ts_month(M1_mensal)
inflacao_ts <- make_ts_month(inflacao_mensal)
pib_ts <- make_ts_month(pib_mensal)

a <- lm(data = log(painel[,-c(1,2)]), formula = dolar_venda_mensal  ~.)


fit <- fabletools::model(filter(dolar_compra, data >= lubridate::ymd("2000/01/01")),
                         arima = ARIMA(valor),
                         ets = ETS(valor),
                         theta = THETA(valor),
                         RW = NAIVE(valor),
                         RWs = SNAIVE(valor)
                         )

fit %>% accuracy()

fc <- fit %>% forecast(h = "30 days")
fc
fc %>%  autoplot()
