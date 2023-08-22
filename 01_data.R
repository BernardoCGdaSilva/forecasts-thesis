# TCC forecast: Get and organise data
# Bernardo Cainelli Gomes da Silva
# Jun - 2023

# _____________________________________________________________________________
# _______________________________ HELPER FUNCTIONS ____________________________
# _____________________________________________________________________________
source("00_functions.R")
fredr_set_key(pull(read_csv("fred_key.txt", col_names = F)))

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

# a <- br_exchange %>% mutate(
#  lead1 = lead(br_exchange, 1),
#  var1 = (lead(br_exchange, 1) / br_exchange) - 1
# )
# b <- br_exchange %>% mutate(
#  lag1 = lag(br_exchange, 1),
#  var1 = (br_exchange / lag(br_exchange, 1)) - 1
# )
# c <- br_exchange %>% mutate(
#  log1 = log(br_exchange),
#  loglead = lead(log(br_exchange),1),
#  var1 = lead(log(br_exchange),1) - log(br_exchange)
#    )

br_exchange_rate <- br_exchange %>%
  mutate(
    br_exchange_rate_h1 = lead(log(br_exchange), 1) - log(br_exchange),
    br_exchange_rate_h12 = lead(log(br_exchange), 12) - log(br_exchange)
  ) %>%
  select(-br_exchange)
ru_exchange_rate <- ru_exchange %>%
  mutate(
    ru_exchange_rate_h1 = lead(log(ru_exchange), 1) - log(ru_exchange),
    ru_exchange_rate_h12 = lead(log(ru_exchange), 12) - log(ru_exchange)
  ) %>%
  select(-ru_exchange)
in_exchange_rate <- in_exchange %>%
  mutate(
    in_exchange_rate_h1 = lead(log(in_exchange), 1) - log(in_exchange),
    in_exchange_rate_h12 = lead(log(in_exchange), 12) - log(in_exchange)
  ) %>%
  select(-in_exchange)
cn_exchange_rate <- cn_exchange %>%
  mutate(
    cn_exchange_rate_h1 = lead(log(cn_exchange), 1) - log(cn_exchange),
    cn_exchange_rate_h12 = lead(log(cn_exchange), 12) - log(cn_exchange)
  ) %>%
  select(-cn_exchange)
za_exchange_rate <- za_exchange %>%
  mutate(
    za_exchange_rate_h1 = lead(log(za_exchange), 1) - log(za_exchange),
    za_exchange_rate_h12 = lead(log(za_exchange), 12) - log(za_exchange)
  ) %>%
  select(-za_exchange)

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

br_inflation_rate <- br_inflation %>%
  mutate(br_inflation_rate = (br_inflation / lag(br_inflation)) - 1) %>%
  select(-br_inflation)
ru_inflation_rate <- ru_inflation %>%
  mutate(ru_inflation_rate = (ru_inflation / lag(ru_inflation)) - 1) %>%
  select(-ru_inflation)
in_inflation_rate <- in_inflation %>%
  mutate(in_inflation_rate = (in_inflation / lag(in_inflation)) - 1) %>%
  select(-in_inflation)
cn_inflation_rate <- cn_inflation %>%
  mutate(cn_inflation_rate = (cn_inflation / lag(cn_inflation)) - 1) %>%
  select(-cn_inflation)
za_inflation_rate <- za_inflation %>%
  mutate(za_inflation_rate = (za_inflation / lag(za_inflation)) - 1) %>%
  select(-za_inflation)
us_inflation_rate <- us_inflation %>%
  mutate(us_inflation_rate = (us_inflation / lag(us_inflation)) - 1) %>%
  select(-us_inflation)

# M1, NSA
br_m1 <- fredr(series_id = "MANMM101BRM189N") %>% select(1, 3)
colnames(br_m1) <- c("date", "br_m1")
br_m1 <- mutate(br_m1, br_m1_log = log(br_m1))

ru_m1 <- fredr(series_id = "MANMM101RUM189N") %>% select(1, 3)
colnames(ru_m1) <- c("date", "ru_m1")
ru_m1 <- mutate(ru_m1, ru_m1_log = log(ru_m1))

in_m1 <- fredr(series_id = "MANMM101INM189N") %>% select(1, 3)
colnames(in_m1) <- c("date", "in_m1")
in_m1 <- mutate(in_m1, in_m1_log = log(in_m1))

cn_m1 <- fredr(series_id = "MANMM101CNM189N") %>% select(1, 3)
colnames(cn_m1) <- c("date", "cn_m1")
cn_m1 <- mutate(cn_m1, cn_m1_log = log(cn_m1))

za_m1 <- fredr(series_id = "MANMM101ZAM189N") %>% select(1, 3)
colnames(za_m1) <- c("date", "za_m1")
za_m1 <- mutate(za_m1, za_m1_log = log(za_m1))

us_m1 <- fredr(series_id = "MANMM101USM189N") %>% select(1, 3)
colnames(us_m1) <- c("date", "us_m1")
us_m1 <- mutate(us_m1, us_m1_log = log(us_m1))

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
) %>% mutate(gdp_log = log(value))

ru_gdp <- quartet_to_monthly(
  data_gdp = nominal_gdp,
  data_ip = ru_ip,
  filtro = "RU",
  # colname = "ru_gdp",
  gdp_ano_0 = 2003,
  gdp_quarter_0 = 1,
  ip_ano_0 = 1999,
  ip_mes_0 = 1
) %>% mutate(gdp_log = log(value))

in_gdp <- quartet_to_monthly(
  data_gdp = nominal_gdp,
  data_ip = in_ip,
  filtro = "IN",
  # colname = "in_gdp",
  gdp_ano_0 = 2004,
  gdp_quarter_0 = 2,
  ip_ano_0 = 1994,
  ip_mes_0 = 4
) %>% mutate(gdp_log = log(value))

cn_gdp <- quartet_to_monthly(
  data_gdp = nominal_gdp,
  data_ip = cn_ip,
  filtro = "CN",
  # colname = "cn_gdp",
  gdp_ano_0 = 1992,
  gdp_quarter_0 = 1,
  ip_ano_0 = 1999,
  ip_mes_0 = 1
) %>% mutate(gdp_log = log(value))

za_gdp <- quartet_to_monthly(
  data_gdp = nominal_gdp,
  data_ip = za_ip,
  filtro = "ZA",
  # colname = "za_gdp",
  gdp_ano_0 = 1993,
  gdp_quarter_0 = 1,
  ip_ano_0 = 1990,
  ip_mes_0 = 1
) %>% mutate(gdp_log = log(value))

us_gdp <- quartet_to_monthly(
  data_gdp = nominal_gdp,
  data_ip = us_ip,
  filtro = "US",
  # colname = "us_gdp",
  gdp_ano_0 = 1950,
  gdp_quarter_0 = 1,
  ip_ano_0 = 1960,
  ip_mes_0 = 1
) %>% mutate(gdp_log = log(value))

# Output gap via Hamilton Filter from monthly series

br_gap <- output_gap_m(data = br_gdp, colname = "br_gap")
ru_gap <- output_gap_m(data = ru_gdp, colname = "ru_gap")
in_gap <- output_gap_m(data = in_gdp, colname = "in_gap")
cn_gap <- output_gap_m(data = cn_gdp, colname = "cn_gap")
za_gap <- output_gap_m(data = za_gdp, colname = "za_gap")
us_gap <- output_gap_m(data = us_gdp, colname = "us_gap")

# VIX

vix <- fredr(series_id = "VIXCLS") %>%
  select(1, 3) %>%
  dplyr::mutate(date = lubridate::floor_date(date, "month")) %>%
  group_by(date) %>%
  summarise(vix = dplyr::last(value))

# Rename colnames of gdp here because if done before would break the functions
colnames(br_gdp) <- c("date", "br_gdp", "br_gdp_log")
colnames(ru_gdp) <- c("date", "ru_gdp", "ru_gdp_log")
colnames(in_gdp) <- c("date", "in_gdp", "in_gdp_log")
colnames(cn_gdp) <- c("date", "cn_gdp", "cn_gdp_log")
colnames(za_gdp) <- c("date", "za_gdp", "za_gdp_log")
colnames(us_gdp) <- c("date", "us_gdp", "us_gdp_log")

# _____________________________________________________________________________
# ______________________________ BUILD PANEL __________________________________
# _____________________________________________________________________________

# All the data
panel_data <- reduce(
  list(
    br_exchange, br_exchange_rate, br_gap, br_gdp, br_inflation, br_inflation_rate, br_interest, br_m1,
    ru_exchange, ru_exchange_rate, ru_gap, ru_gdp, ru_inflation, ru_inflation_rate, ru_interest, ru_m1,
    in_exchange, in_exchange_rate, in_gap, in_gdp, in_inflation, in_inflation_rate, in_interest, in_m1,
    cn_exchange, cn_exchange_rate, cn_gap, cn_gdp, cn_inflation, cn_inflation_rate, cn_interest, cn_m1,
    za_exchange, za_exchange_rate, za_gap, za_gdp, za_inflation, za_inflation_rate, za_interest, za_m1,
    us_gap, us_gdp, us_inflation, us_inflation_rate, us_interest, us_m1, vix
  ),
  full_join,
  by = "date"
) %>%
  mutate(
    br_interest_lag = dplyr::lag(br_interest, 1),
    ru_interest_lag = dplyr::lag(ru_interest, 1),
    in_interest_lag = dplyr::lag(in_interest, 1),
    cn_interest_lag = dplyr::lag(cn_interest, 1),
    za_interest_lag = dplyr::lag(za_interest, 1),
    us_interest_lag = dplyr::lag(us_interest, 1),
    us_inflation_log = log(us_inflation),
    br_exchange_log = log(br_exchange),
    br_inflation_log = log(br_inflation),
    ru_exchange_log = log(ru_exchange),
    ru_inflation_log = log(ru_inflation),
    in_exchange_log = log(in_exchange),
    in_inflation_log = log(in_inflation),
    cn_exchange_log = log(cn_exchange),
    cn_inflation_log = log(cn_inflation),
    za_exchange_log = log(za_exchange),
    za_inflation_log = log(za_inflation)
  ) %>%
  arrange(date)
panel_data <- panel_data[rowSums(is.na(panel_data)) == 0, ]
rownames(panel_data) <- seq(length = nrow(panel_data))

# Brazilian data
br_panel_data <- reduce(
  list(
    br_exchange, br_exchange_rate, br_gap, br_gdp, br_inflation, br_interest, br_m1, br_inflation_rate,
    us_gap, us_gdp, us_inflation, us_interest, us_m1, us_inflation_rate, vix
  ),
  full_join,
  by = "date"
) %>%
  mutate(
    br_interest_lag = dplyr::lag(br_interest, 1),
    us_interest_lag = dplyr::lag(us_interest, 1),
    br_exchange_log = log(br_exchange),
    br_inflation_log = log(br_inflation),
    us_inflation_log = log(us_inflation),
    diff_inflation_rate = br_inflation_rate - us_inflation_rate,
    diff_gap = br_gap - us_gap,
    real_exchange_rate = br_exchange_log + us_inflation_log - br_inflation_log,
    diff_interest_lag = br_interest_lag - us_interest_lag,
    diff_interest = br_interest - us_interest,
    diff_m1 = br_m1_log - us_m1_log,
    diff_gdp = br_gdp_log - us_gdp_log
  ) %>%
  arrange(date)
br_panel_data <- br_panel_data[rowSums(is.na(br_panel_data)) == 0, ]
rownames(br_panel_data) <- seq(length = nrow(br_panel_data))

# Russian data
ru_panel_data <- reduce(
  list(
    ru_exchange, ru_exchange_rate, ru_gap, ru_gdp, ru_inflation, ru_interest, ru_m1, ru_inflation_rate,
    us_gap, us_gdp, us_inflation, us_interest, us_m1, us_inflation_rate, vix
  ),
  full_join,
  by = "date"
) %>%
  mutate(
    ru_interest_lag = dplyr::lag(ru_interest, 1),
    us_interest_lag = dplyr::lag(us_interest, 1),
    ru_exchange_log = log(ru_exchange),
    ru_inflation_log = log(ru_inflation),
    us_inflation_log = log(us_inflation),
    diff_inflation_rate = ru_inflation_rate - us_inflation_rate,
    diff_gap = ru_gap - us_gap,
    real_exchange_rate = ru_exchange_log + us_inflation_log - ru_inflation_log,
    diff_interest_lag = ru_interest_lag - us_interest_lag,
    diff_interest = ru_interest - us_interest,
    diff_m1 = ru_m1_log - us_m1_log,
    diff_gdp = ru_gdp_log - us_gdp_log
  ) %>%
  arrange(date)
ru_panel_data <- ru_panel_data[rowSums(is.na(ru_panel_data)) == 0, ]
rownames(ru_panel_data) <- seq(length = nrow(ru_panel_data))

# Indian data
in_panel_data <- reduce(
  list(
    in_exchange, in_exchange_rate, in_gap, in_gdp, in_inflation, in_interest, in_m1, in_inflation_rate,
    us_gap, us_gdp, us_inflation, us_interest, us_m1, us_inflation_rate, vix
  ),
  full_join,
  by = "date"
) %>%
  mutate(
    in_interest_lag = dplyr::lag(in_interest, 1),
    us_interest_lag = dplyr::lag(us_interest, 1),
    in_exchange_log = log(in_exchange),
    in_inflation_log = log(in_inflation),
    us_inflation_log = log(us_inflation),
    diff_inflation_rate = in_inflation_rate - us_inflation_rate,
    diff_gap = in_gap - us_gap,
    real_exchange_rate = in_exchange_log + us_inflation_log - in_inflation_log,
    diff_interest_lag = in_interest_lag - us_interest_lag,
    diff_interest = in_interest - us_interest,
    diff_m1 = in_m1_log - us_m1_log,
    diff_gdp = in_gdp_log - us_gdp_log
  ) %>%
  arrange(date)
in_panel_data <- in_panel_data[rowSums(is.na(in_panel_data)) == 0, ]
rownames(in_panel_data) <- seq(length = nrow(in_panel_data))

# Chinese data
cn_panel_data <- reduce(
  list(
    cn_exchange, cn_exchange_rate, cn_gap, cn_gdp, cn_inflation, cn_interest, cn_m1, cn_inflation_rate,
    us_gap, us_gdp, us_inflation, us_interest, us_m1, us_inflation_rate, vix
  ),
  full_join,
  by = "date"
) %>%
  mutate(
    cn_interest_lag = dplyr::lag(cn_interest, 1),
    us_interest_lag = dplyr::lag(us_interest, 1),
    cn_exchange_log = log(cn_exchange),
    cn_inflation_log = log(cn_inflation),
    us_inflation_log = log(us_inflation),
    diff_inflation_rate = cn_inflation_rate - us_inflation_rate,
    diff_gap = cn_gap - us_gap,
    real_exchange_rate = cn_exchange_log + us_inflation_log - cn_inflation_log,
    diff_interest_lag = cn_interest_lag - us_interest_lag,
    diff_interest = cn_interest - us_interest,
    diff_m1 = cn_m1_log - us_m1_log,
    diff_gdp = cn_gdp_log - us_gdp_log
  ) %>%
  arrange(date)
cn_panel_data <- cn_panel_data[rowSums(is.na(cn_panel_data)) == 0, ]
rownames(cn_panel_data) <- seq(length = nrow(cn_panel_data))

# South african data
za_panel_data <- reduce(
  list(
    za_exchange, za_exchange_rate, za_gap, za_gdp, za_inflation, za_interest, za_m1, za_inflation_rate,
    us_gap, us_gdp, us_inflation, us_interest, us_m1, us_inflation_rate, vix
  ),
  full_join,
  by = "date"
) %>%
  mutate(
    za_interest_lag = dplyr::lag(za_interest, 1),
    us_interest_lag = dplyr::lag(us_interest, 1),
    za_exchange_log = log(za_exchange),
    za_inflation_log = log(za_inflation),
    us_inflation_log = log(us_inflation),
    diff_inflation_rate = za_inflation_rate - us_inflation_rate,
    diff_gap = za_gap - us_gap,
    real_exchange_rate = za_exchange_log + us_inflation_log - za_inflation_log,
    diff_interest_lag = za_interest_lag - us_interest_lag,
    diff_interest = za_interest - us_interest,
    diff_m1 = za_m1_log - us_m1_log,
    diff_gdp = za_gdp_log - us_gdp_log
  ) %>%
  arrange(date)
za_panel_data <- za_panel_data[rowSums(is.na(za_panel_data)) == 0, ]
rownames(za_panel_data) <- seq(length = nrow(za_panel_data))

# Export
write_csv2(panel_data, "outputs/panel_data.csv")
write_csv2(br_panel_data, "outputs/br_panel_data.csv")
write_csv2(ru_panel_data, "outputs/ru_panel_data.csv")
write_csv2(in_panel_data, "outputs/in_panel_data.csv")
write_csv2(cn_panel_data, "outputs/cn_panel_data.csv")
write_csv2(za_panel_data, "outputs/za_panel_data.csv")

