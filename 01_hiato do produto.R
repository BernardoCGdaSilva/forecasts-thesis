# TCC forecast: output gap
# Bernardo Cainelli Gomes da Silva
# Abr - 2023

# _________________________________________________________________________________
# _____________________________ HELPER FUNCTIONS __________________________________
# _________________________________________________________________________________

source("00_functions.R")

# _________________________________________________________________________________
# ___________________________________ DATA ________________________________________
# _________________________________________________________________________________

pib_mensal <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.4380/dados?formato=json", "pib_mensal")
# k_estoque <- ipeadata(code="DIMAC_ECFLIQTOT12", language = "br") %>% select(2,3)
# attributes(k_estoque$date) <- NULL
# names(k_estoque$value) <- NULL
# k_utilizado <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.28561/dados?formato=json", "UCI_transformacao")

# _________________________________________________________________________________
# _______________________________ HAMILTON FILTER _________________________________
# _________________________________________________________________________________

library(neverhpfilter)

hf <- yth_filter(as.xts(pib_mensal), h = 24, p = 12)
hiato <- (pib_mensal$pib_mensal / hf$pib_mensal.trend) - 1


# c <- pib_mensal %>%
#  column_to_rownames("data") %>%
#  bind_cols(., lag(., 24), lag(., 25), lag(., 26), lag(., 27), lag(., 28), lag(., 29), lag(., 30), lag(., 31), lag(., 32), lag(., 33), lag(., 34), lag(., 35))

# b <- lm(c, formula = `pib_mensal...1` ~ ., na.action = na.omit)
# summary(b)
