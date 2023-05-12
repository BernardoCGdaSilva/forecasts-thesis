# TCC forecast: get data and build panel
# Bernardo Cainelli Gomes da Silva
# Nov - 2022

# _________________________________________________________________________________
# _____________________________ HELPER FUNCTIONS __________________________________
# _________________________________________________________________________________

source("00_functions.R")
fredr_set_key("97f79f87ad234e695b64f0cc576fb23f")

# _________________________________________________________________________________
# __________________________________ DATA _________________________________________
# _________________________________________________________________________________

# Exchange Rate

br_exchange <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.3695/dados?formato=json", "br_exchange")
#dolar_venda_mensal <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.3695/dados?formato=json", "dolar_venda_mensal")

# Brazilian Inflation

br_inflation <- sidrar::get_sidra(x = 1737, variable = 2266, period = "all", geo = "Brazil")
br_inflation <- br_inflation %>%
  dplyr::mutate(data = ym(`Mês (Código)`)) %>%
  dplyr::select(data, Valor)
colnames(br_inflation) <- c("date", "br_inflation")

# US Inflation

us_inflation <- fredr::fredr(series_id = "CPIAUCNS") %>% select(1,3)
colnames(us_inflation) <- c("date", "us_inflation")

# Brazilian GDP

br_gdp <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.4380/dados?formato=json", "br_gdp")
br_gdp$br_gdp <- br_gdp$br_gdp * 1000000

# US GDP: quarterly data turned monthly with Industrial Production as indicator (Riberio, 2017, p. 899)

us_gdp <- fredr::fredr(series_id = "NA000334Q")
us_gdp$value <- us_gdp$value*1000000
us_gdp <- ts(us_gdp$value, start = c(1947,1), frequency = 4)

us_ip <- fredr::fredr(series_id = "IPB50001N")
us_ip <- ts(us_ip$value, start = c(1919,1), frequency = 12)

us_gdp_monthly_fit <- tempdisagg::td(us_gdp ~ us_ip, conversion = "first", to = "month") %>% predict()
us_gdp_monthly <- data.frame(date=as.Date(time(us_gdp_monthly_fit)), us_gdp=as.matrix(us_gdp_monthly_fit))

#test
ggplot() +
  geom_line(data = us_gdp_monthly, mapping = aes (x = date, y = us_gdp))+
  geom_line(data = data.frame(date=as.Date(time(us_gdp)), us_gdp=as.matrix(us_gdp)), mapping = aes (x = date, y = us_gdp)) #+
  #tidyquant::coord_x_date(xlim = c("2016-01-01", "2023-05-11"))

tail(data.frame(date=as.Date(time(us_gdp)), usa_gdp=as.matrix(us_gdp)))
tail(us_gdp_monthly)

# BR Interest Rate

br_interest <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.432/dados?formato=json", "br_interest")
br_interest <- day_to_month(br_interest) %>% `colnames<-`(c("date", "br_interest"))

# US Interest Rate

us_interest <- fredr(series_id = "FEDFUNDS") %>% select(1,3)
colnames(us_interest) <- c("date", "us_interest")

# BR monetary suply

br_m1 <- get_sgs("http://api.bcb.gov.br/dados/serie/bcdata.sgs.27791/dados?formato=json", "br_m1")
br_m1$br_m1 <- br_m1$br_m1 * 1000

# US monetary suply

us_m1 <- fredr(series_id = "M1NS")%>% select(1,3)
colnames(us_m1) <- c("date", "us_m1")
us_m1$us_m1 <- us_m1$us_m1 * 1000000000

# BR output gap via Hamilton Filter

br_hf <- neverhpfilter::yth_filter(xts::as.xts(br_gdp), h = 24, p = 12)
br_gap <- br_gdp$br_gdp / br_hf$br_gdp.trend
br_gap <- as.data.frame(br_gap) %>%
  rownames_to_column() %>%
  `colnames<-`(c("date", "br_gap"))
br_gap$date <- lubridate::ymd(br_gap$date)

# US output gap via Hamilton Filter

us_hf <- neverhpfilter::yth_filter(xts::as.xts(us_gdp_monthly), h = 24, p = 12)
us_gap <- us_gdp_monthly$us_gdp / us_hf$us_gdp.trend
us_gap <- as.data.frame(us_gap) %>%
  rownames_to_column() %>%
  `colnames<-`(c("date", "us_gap"))
us_gap$date <- lubridate::ymd(us_gap$date)

# _________________________________________________________________________________
# _______________________________ BUILD PANEL _____________________________________
# _________________________________________________________________________________

data_panel <- reduce(list(br_exchange, br_inflation, us_inflation, br_gdp, us_gdp_monthly, br_interest, us_interest, br_m1, us_m1, br_gap, us_gap), full_join, by = "date")
data_panel <- data_panel[rowSums(is.na(data_panel)) == 0, ]
rownames(data_panel) <- seq(length = nrow(data_panel))

# Todo:
# real exchange rate
# diffs

write_csv2(data_panel, "Painel de dados.csv")

rm(list = setdiff(ls(), "data_panel"))
