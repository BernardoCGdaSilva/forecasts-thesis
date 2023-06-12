# TCC forecast: Get and organise data
# Bernardo Cainelli Gomes da Silva
# Jun - 2023

# _____________________________________________________________________________
# _______________________________ HELPER FUNCTIONS ____________________________
# _____________________________________________________________________________
source("00_functions.R")
fredr_set_key("97f79f87ad234e695b64f0cc576fb23f")

# View(imf_databases())
# View(imf_parameters(database_id = "MFS"))
# View(imf_parameters(database_id = "MFS")[[3]])

# _____________________________________________________________________________
# ________________________________ GET DATA ___________________________________
# _____________________________________________________________________________

# nominal exchange rate, NSA
nominal_exchange <- imf_dataset(
  database_id = "IFS",
  indicator = "ENDE_XDC_USD_RATE",
  freq = "M",
  ref_area = c("BR", "RU", "IN", "CN", "ZA")
)

br_exchange <- filter_ifs(nominal_exchange, filter = "BR", colname = "br_exchange")
ru_exchange <- filter_ifs(nominal_exchange, filter = "RU", colname = "ru_exchange")
in_exchange <- filter_ifs(nominal_exchange, filter = "IN", colname = "in_exchange")
cn_exchange <- filter_ifs(nominal_exchange, filter = "CN", colname = "cn_exchange")
za_exchange <- filter_ifs(nominal_exchange, filter = "ZA", colname = "za_exchange")

# interest rate, NSA
# br_interest <- fredr(series_id = "BRALOCOSTORSTM") %>% select(1, 3) # selic
br_interest <- fredr(series_id = "IRSTCI01BRM156N") %>% select(1, 3) # interbank <24h
ru_interest <- fredr(series_id = "IRSTCI01RUM156N") %>% select(1, 3)
# in_interest <- fredr(series_id = "INDLOCOSTORSTM") %>% select(1, 3) # selic india
in_interest <- fredr(series_id = "IRSTCI01INM156N") %>% select(1, 3) # interbank <24h
cn_interest <- fredr(series_id = "IRSTCI01CNM156N") %>% select(1, 3) # interbank <24h
za_interest <- fredr(series_id = "IRSTCI01ZAM156N") %>% select(1, 3) # interbank <24h
# us_interest <- fredr(series_id = "FEDFUNDS") %>% select(1, 3) # fed funds
us_interest <- fredr(series_id = "IR3TIB01USM156N") %>% select(1, 3) # interbank <24h
colnames(br_interest) <- c("date", "br_interest")
colnames(ru_interest) <- c("date", "ru_interest")
colnames(in_interest) <- c("date", "in_interest")
colnames(cn_interest) <- c("date", "cn_interest")
colnames(za_interest) <- c("date", "za_interest")
colnames(us_interest) <- c("date", "us_interest")



# inflation, NSA
inflation <- imf_dataset(
  database_id = "CPI",
  indicator = "PCPI_IX",
  freq = "M",
  ref_area = c("BR", "RU", "IN", "CN", "ZA", "US")
)
br_inflation <- filter_ifs(inflation, filter = "BR", colname = "br_inflation")
ru_inflation <- filter_ifs(inflation, filter = "RU", colname = "ru_inflation")
in_inflation <- filter_ifs(inflation, filter = "IN", colname = "in_inflation")
cn_inflation <- filter_ifs(inflation, filter = "CN", colname = "cn_inflation")
za_inflation <- filter_ifs(inflation, filter = "ZA", colname = "za_inflation")
us_inflation <- filter_ifs(inflation, filter = "US", colname = "us_inflation")

# M1, NSA
br_m1 <- fredr(series_id = "MANMM101BRM189N") %>% select(1, 3)
colnames(br_m1) <- c("date", "br_m1")
ru_m1 <- fredr(series_id = "MANMM101RUM189N") %>% select(1, 3)
colnames(ru_m1) <- c("date", "ru_m1")
in_m1 <- fredr(series_id = "MANMM101INM189N") %>% select(1, 3)
colnames(in_m1) <- c("date", "in_m1")
cn_m1 <- fredr(series_id = "MANMM101CNM189N") %>% select(1, 3)
colnames(cn_m1) <- c("date", "cn_m1")
za_m1 <- fredr(series_id = "MANMM101ZAM189N") %>% select(1, 3)
colnames(za_m1) <- c("date", "za_m1")
us_m1 <- fredr(series_id = "MANMM101USM189N") %>% select(1, 3)
colnames(us_m1) <- c("date", "us_m1")

# Industrial production: GDP proxy, monthly, NSA

br_ip <- fredr(series_id = "BRAPRMNTO01IXOBM") %>% select(1, 3)
ru_ip <- fredr(series_id = "RUSPRMNTO01IXOBM") %>% select(1, 3)
# ru_ip <- fredr(series_id = "RUSPRINTO01IXOBM") %>% select(1, 3)
# ru_ip <- fredr(series_id = "RUSSLRTTO02MLM") %>% select(1, 3)
in_ip <- fredr(series_id = "INDPRMNTO01IXOBM") %>% select(1, 3)
cn_ip <- fredr(series_id = "CHNPRINTO01IXPYM") %>% select(1, 3)
za_ip <- fredr(series_id = "ZAFPRMNTO01IXOBM") %>% select(1, 3)
us_ip <- fredr(series_id = "USAPRMNTO01IXOBM") %>% select(1, 3)
us_ip <- us_ip[rowSums(is.na(us_ip)) == 0, ]

# nominal gdp, quarterly, NSA
nominal_gdp <- imf_dataset(
  database_id = "IFS",
  indicator = "NGDP_NSA_XDC",
  freq = "Q",
  ref_area = c("BR", "RU", "IN", "CN", "ZA", "US")
)

# Output gap via Hamilton Filter from quarterly series

# br_gap <- output_gap_q(data = nominal_gdp, filtro = "BR", colname = "br_gap")
# ru_gap <- output_gap_q(data = nominal_gdp, filtro = "RU", colname = "ru_gap")
# in_gap <- output_gap_q(data = nominal_gdp, filtro = "IN", colname = "in_gap")
# cn_gap <- output_gap_q(data = nominal_gdp, filtro = "CN", colname = "cn_gap")
# za_gap <- output_gap_q(data = nominal_gdp, filtro = "ZA", colname = "za_gap")
# us_gap <- output_gap_q(data = nominal_gdp, filtro = "US", colname = "us_gap")

# gdp: Quarterly to Monthly

br_gdp <- quartet_to_monthly(
  data_gdp = nominal_gdp,
  data_ip = br_ip,
  filtro = "BR",
  # colname = "br_gdp",
  gdp_ano_0 = 1996,
  gdp_quarter_0 = 1,
  ip_ano_0 = 1975,
  ip_mes_0 = 1
)

ru_gdp <- quartet_to_monthly(
  data_gdp = nominal_gdp,
  data_ip = ru_ip,
  filtro = "RU",
  # colname = "ru_gdp",
  gdp_ano_0 = 2003,
  gdp_quarter_0 = 1,
  ip_ano_0 = 1999,
  ip_mes_0 = 1
)

in_gdp <- quartet_to_monthly(
  data_gdp = nominal_gdp,
  data_ip = in_ip,
  filtro = "IN",
  # colname = "in_gdp",
  gdp_ano_0 = 2004,
  gdp_quarter_0 = 2,
  ip_ano_0 = 1994,
  ip_mes_0 = 4
)

cn_gdp <- quartet_to_monthly(
  data_gdp = nominal_gdp,
  data_ip = cn_ip,
  filtro = "CN",
  # colname = "cn_gdp",
  gdp_ano_0 = 1992,
  gdp_quarter_0 = 1,
  ip_ano_0 = 1999,
  ip_mes_0 = 1
)

za_gdp <- quartet_to_monthly(
  data_gdp = nominal_gdp,
  data_ip = za_ip,
  filtro = "ZA",
  # colname = "za_gdp",
  gdp_ano_0 = 1993,
  gdp_quarter_0 = 1,
  ip_ano_0 = 1990,
  ip_mes_0 = 1
)

us_gdp <- quartet_to_monthly(
  data_gdp = nominal_gdp,
  data_ip = us_ip,
  filtro = "US",
  # colname = "us_gdp",
  gdp_ano_0 = 1950,
  gdp_quarter_0 = 1,
  ip_ano_0 = 1960,
  ip_mes_0 = 1
)

# Output gap via Hamilton Filter from monthly series

br_gap <- output_gap_m(data = br_gdp, colname = "br_gap")
ru_gap <- output_gap_m(data = ru_gdp, colname = "ru_gap")
in_gap <- output_gap_m(data = in_gdp, colname = "in_gap")
cn_gap <- output_gap_m(data = cn_gdp, colname = "cn_gap")
za_gap <- output_gap_m(data = za_gdp, colname = "za_gap")
us_gap <- output_gap_m(data = us_gdp, colname = "us_gap")

# Rename colnames of gdp here because if done before would break de functions
colnames(br_gdp) <- c("date", "br_gdp")
colnames(ru_gdp) <- c("date", "ru_gdp")
colnames(in_gdp) <- c("date", "in_gdp")
colnames(cn_gdp) <- c("date", "cn_gdp")
colnames(za_gdp) <- c("date", "za_gdp")
colnames(us_gdp) <- c("date", "us_gdp")

# _____________________________________________________________________________
# ______________________________ BUILD PANEL __________________________________
# _____________________________________________________________________________

# All the data
panel_data <- reduce(
  list(
    br_exchange, br_gap, br_gdp, br_inflation, br_interest, br_m1,
    ru_exchange, ru_gap, ru_gdp, ru_inflation, ru_interest, ru_m1,
    in_exchange, in_gap, in_gdp, in_inflation, in_interest, in_m1,
    cn_exchange, cn_gap, cn_gdp, cn_inflation, cn_interest, cn_m1,
    za_exchange, za_gap, za_gdp, za_inflation, za_interest, za_m1,
    us_gap, us_gdp, us_inflation, us_interest, us_m1
  ),
  full_join,
  by = "date"
)
panel_data <- panel_data[rowSums(is.na(panel_data)) == 0, ]
rownames(panel_data) <- seq(length = nrow(panel_data))

# Brazilian data
br_panel_data <- reduce(
  list(
    br_exchange, br_gap, br_gdp, br_inflation, br_interest, br_m1,
    us_gap, us_gdp, us_inflation, us_interest, us_m1
  ),
  full_join,
  by = "date"
)
br_panel_data <- br_panel_data[rowSums(is.na(br_panel_data)) == 0, ]
rownames(br_panel_data) <- seq(length = nrow(br_panel_data))

# Russian data
ru_panel_data <- reduce(
  list(
    ru_exchange, ru_gap, ru_gdp, ru_inflation, ru_interest, ru_m1,
    us_gap, us_gdp, us_inflation, us_interest, us_m1
  ),
  full_join,
  by = "date"
)
ru_panel_data <- ru_panel_data[rowSums(is.na(ru_panel_data)) == 0, ]
rownames(ru_panel_data) <- seq(length = nrow(ru_panel_data))

# Indian data
in_panel_data <- reduce(
  list(
    in_exchange, in_gap, in_gdp, in_inflation, in_interest, in_m1,
    us_gap, us_gdp, us_inflation, us_interest, us_m1
  ),
  full_join,
  by = "date"
)
in_panel_data <- in_panel_data[rowSums(is.na(in_panel_data)) == 0, ]
rownames(in_panel_data) <- seq(length = nrow(in_panel_data))

# Chinese data
cn_panel_data <- reduce(
  list(
    cn_exchange, cn_gap, cn_gdp, cn_inflation, cn_interest, cn_m1,
    us_gap, us_gdp, us_inflation, us_interest, us_m1
  ),
  full_join,
  by = "date"
)
cn_panel_data <- cn_panel_data[rowSums(is.na(cn_panel_data)) == 0, ]
rownames(cn_panel_data) <- seq(length = nrow(cn_panel_data))

# South african data
za_panel_data <- reduce(
  list(
    za_exchange, za_gap, za_gdp, za_inflation, za_interest, za_m1,
    us_gap, us_gdp, us_inflation, us_interest, us_m1
  ),
  full_join,
  by = "date"
)
za_panel_data <- za_panel_data[rowSums(is.na(za_panel_data)) == 0, ]
rownames(za_panel_data) <- seq(length = nrow(za_panel_data))

# Export
write_csv2(panel_data, "outputs/panel_data.csv")
write_csv2(br_panel_data, "outputs/br_panel_data.csv")
write_csv2(ru_panel_data, "outputs/ru_panel_datas.csv")
write_csv2(in_panel_data, "outputs/in_panel_data.csv")
write_csv2(cn_panel_data, "outputs/cn_panel_data.csv")
write_csv2(za_panel_data, "outputs/za_panel_data.csv")
