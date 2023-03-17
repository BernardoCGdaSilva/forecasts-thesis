# TCC forecast: get data and build panel
# Bernardo Cainelli Gomes da Silva
# Nov - 2022

# _________________________________________________________________________________
# _____________________________ HELPER FUNCTIONS __________________________________
# _________________________________________________________________________________

source("00_functions.R")

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

selic_mensal <- day_to_month(selic_diario) %>% `colnames<-`(c("data", "selic_mensal")) ### TEM ALGO DE ERRADO. VERIFICAR ULTIMOS VALORES

# +n to inflation series to allow for log-diff

#inflacao_mensal$inflacao_mensal <- inflacao_mensal$inflacao_mensal+1

# Build panel

painel <- reduce(list(dolar_compra_mensal, dolar_venda_mensal, M1_mensal,inflacao_mensal, selic_mensal, pib_mensal),full_join, by = "data")
painel <- painel[rowSums(is.na(painel)) == 0,]
rownames(painel) = seq(length=nrow(painel))
#teste <- painel[painel$dolar_compra_mensal != painel$dolar_venda_mensal,] # Para averiguar se há diferença entre os valores de compra e venda do dólar.


View(log(painel[,-1]))


write_csv2(painel, "Painel de dados.csv")

rm(list=setdiff(ls(), "painel"))
