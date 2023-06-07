# TCC forecast: train models
# Bernardo Cainelli Gomes da Silva
# Fev - 2023

# _________________________________________________________________________________
# _____________________________ HELPER FUNCTIONS __________________________________
# _________________________________________________________________________________

source("00_functions.R")
# source("01_data.R")
# source("02_models.R")

# _________________________________________________________________________________
# _________________________________ GET MODELS ____________________________________
# _________________________________________________________________________________

get_models <- c(list.files("outputs", pattern = "rds"))

for (i in get_models) {
  assign(paste0(str_remove(i, ".rds")), readRDS(paste0("outputs/", i)))
}

# __________________________________________________________________________________
# _________________________________ GET RMSE H1 ____________________________________
# __________________________________________________________________________________

rmse_lm_h1 <- model_lm$pred %>% as.data.frame()
rmse_lm_h1$training <- str_sub(rmse_lm_h1$Resample, -3) %>% as.double()
rmse_lm_h1 <- filter(rmse_lm_h1, training + 1 == rowIndex)%>% mutate(mse = (pred - obs)**2/ nrow(.))
rmse_lm_h1 <- sqrt(sum(rmse_lm_h1$mse))

# ___________________________________________________________________________________
# _________________________________ GET RMSE H12 ____________________________________
# ___________________________________________________________________________________

rmse_lm_h12 <- model_lm$pred %>% as.data.frame()
rmse_lm_h12$training <- str_sub(rmse_lm_h12$Resample, -3) %>% as.double()
rmse_lm_h12 <- filter(rmse_lm_h12, training + 12 == rowIndex)%>% mutate(mse = (pred - obs)**2/ nrow(.))
rmse_lm_h12 <- sqrt(sum(rmse_lm_h12$mse))
