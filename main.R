# TCC forecast
# Bernardo Cainelli Gomes da Silva
# Nov - 2022

source("functions.R")

# Dados

dolar_compra <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.10813/dados?formato=json")
dolar_venda <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.1/dados?formato=json")
