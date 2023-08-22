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

models <- readRDS("outputs/output_models.rds")

# ___________________________________________________________________________________
# ___________________________________ RANDOM WALK ___________________________________
# ___________________________________________________________________________________

# Random walk RMSE
br_rmse_rw <- rmse_rw("br")
ru_rmse_rw <- rmse_rw("ru")
in_rmse_rw <- rmse_rw("in")
cn_rmse_rw <- rmse_rw("cn")
za_rmse_rw <- rmse_rw("za")

# ___________________________________________________________________________________
# _____________________________________ GET RMSE ____________________________________
# ___________________________________________________________________________________

rmse_lm <- list()
rmse_svm_r <- list()
rmse_svm_l <- list()
#rmse_mars <- list()
rmse_tree <- list()
rmse_forest <- list()
rmse_gam <- list()
dm_lm <- list()
dm_svm_r <- list()
dm_svm_l <- list()
#dm_mars <- list()
dm_tree <- list()
dm_forest <- list()
dm_gam <- list()
country_list <- c("br", "ru", "in", "cn", "za")
model_list <- c(
  "taylor", "taylor_ppp", "taylor_ppp_smoothing", "taylor_smoothing",
  "ppp", "foward_premium", "monetary", "monetary_sticky", "monetary_risk"
)
window_list <- c("rolling", "expanding")
h_list <- c("h1", "h12")


for (cty in country_list) {
  for (wdw in window_list) {
    for (h in h_list) {
      for (mdl in model_list) {
        # Linear models

        # Set up for Diebold-Mariano test
        h_num <- ifelse(h == "h1", 1, 12)
        e1_dm <- get(paste0(cty, "_rmse_rw"))[[paste0("error_rw_", h)]]
        e2_dm_lm <- (models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["lm"]]$pred[, "pred"] -
          models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["lm"]]$pred[, "obs"])**2

        # Get RMSE
        rmse_lm[[paste0(cty, "_", wdw, "_", h, "_", mdl, "_lm")]] <- c(RMSE(
          pred = models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["lm"]]$pred[, "pred"],
          obs = models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["lm"]]$pred[, "obs"]
        ), cty, h, wdw, mdl, "lm")

        # Get DM test
        dm_lm[[paste0(cty, "_", wdw, "_", h, "_", mdl, "_lm")]] <- forecast::dm.test(
          e1 = e1_dm, e2 = e2_dm_lm, h = h_num, power = 2, alternative = "greater", varestimator = "bartlett"
        )[["p.value"]]

        # SVM radial

        var_svm_r <- filter(
          models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["svm_r"]]$pred,
          sigma == models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["svm_r"]]$bestTune[[1]] &
            C == models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["svm_r"]]$bestTune[[2]]
        ) %>% mutate(error = (pred - obs)**2)

        rmse_svm_r[[paste0(cty, "_", wdw, "_", h, "_", mdl, "_svm_r")]] <- c(RMSE(
          pred = var_svm_r$pred,
          obs = var_svm_r$obs
        ), cty, h, wdw, mdl, "svm_r")

        dm_svm_r[[paste0(cty, "_", wdw, "_", h, "_", mdl, "_svm_r")]] <- forecast::dm.test(
          e1 = e1_dm, e2 = var_svm_r$error, h = h_num, power = 2, alternative = "greater", varestimator = "bartlett"
        )[["p.value"]]

        # SVM linear

        var_svm_l <- filter(
          models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["svm_l"]]$pred,
          C == models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["svm_l"]]$bestTune[[1]]
        ) %>% mutate(error = (pred - obs)**2)

        rmse_svm_l[[paste0(cty, "_", wdw, "_", h, "_", mdl, "_svm_l")]] <- c(RMSE(
          pred = var_svm_l$pred,
          obs = var_svm_l$obs
        ), cty, h, wdw, mdl, "svm_l")

        dm_svm_l[[paste0(cty, "_", wdw, "_", h, "_", mdl, "_svm_l")]] <- forecast::dm.test(
          e1 = e1_dm, e2 = var_svm_l$error, h = h_num, power = 2, alternative = "greater", varestimator = "bartlett"
        )[["p.value"]]

        # MARS

        #var_mars <- filter(
        #  models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["mars"]]$pred,
        #  nprune == models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["mars"]]$bestTune[[1]] &
        #    degree == models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["mars"]]$bestTune[[2]]
        #) %>% mutate(error = (pred - obs)**2)

        #rmse_mars[[paste0(cty, "_", wdw, "_", h, "_", mdl, "_mars")]] <- c(RMSE(
        #  pred = var_mars$pred,
       #   obs = var_mars$obs
       # ), cty, h, wdw, mdl, "mars")

        #dm_mars[[paste0(cty, "_", wdw, "_", h, "_", mdl, "_mars")]] <- forecast::dm.test(
        #  e1 = e1_dm, e2 = var_mars$error, h = h_num, power = 2, alternative = "greater", varestimator = "bartlett"
        #)[["p.value"]]

        # Tree

        var_tree <- filter(
          models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["tree"]]$pred,
          cp == models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["tree"]]$bestTune[[1]]
        ) %>% mutate(error = (pred - obs)**2)

        rmse_tree[[paste0(cty, "_", wdw, "_", h, "_", mdl, "_tree")]] <- c(RMSE(
          pred = var_tree$pred,
          obs = var_tree$obs
        ), cty, h, wdw, mdl, "tree")

        dm_tree[[paste0(cty, "_", wdw, "_", h, "_", mdl, "_tree")]] <- forecast::dm.test(
          e1 = e1_dm, e2 = var_tree$error, h = h_num, power = 2, alternative = "greater", varestimator = "bartlett"
        )[["p.value"]]

        # Random Forest

        var_forest <- filter(
          models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["forest"]]$pred,
          mtry == models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["forest"]]$bestTune[[1]]
        ) %>% mutate(error = (pred - obs)**2)

        rmse_forest[[paste0(cty, "_", wdw, "_", h, "_", mdl, "_forest")]] <- c(RMSE(
          pred = var_forest$pred,
          obs = var_forest$obs
        ), cty, h, wdw, mdl, "forest")

        dm_forest[[paste0(cty, "_", wdw, "_", h, "_", mdl, "_forest")]] <- forecast::dm.test(
          e1 = e1_dm, e2 = var_forest$error, h = h_num, power = 2, alternative = "greater", varestimator = "bartlett"
        )[["p.value"]]
        
        # GAM
        
        var_gam <-  models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["gam"]]$pred %>% mutate(error = (pred - obs)**2)
        
        rmse_gam[[paste0(cty, "_", wdw, "_", h, "_", mdl, "_gam")]] <- c(RMSE(
          pred = var_gam$pred,
          obs = var_gam$obs
        ), cty, h, wdw, mdl, "gam")
        
        dm_gam[[paste0(cty, "_", wdw, "_", h, "_", mdl, "_gam")]] <- forecast::dm.test(
          e1 = e1_dm, e2 = var_gam$error, h = h_num, power = 2, alternative = "greater", varestimator = "bartlett"
        )[["p.value"]]
      }
    }
  }
}



# ___________________________________________________________________________________
# _________________________________ RELATIVE RMSE ___________________________________
# ___________________________________________________________________________________

rmse_list <- list("rmse_lm", "rmse_svm_r", "rmse_svm_l", "rmse_tree", "rmse_forest", "rmse_gam")

for (rmse in rmse_list) {
  print(rmse)
  assign(x = paste0("relative_", rmse), value = get(rmse) %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame() %>% `colnames<-`(c("rmse", "country", "horizon", "window", "model", "method")) %>%
    mutate(
      rmse = as.double(rmse),
      relative_rmse = case_when(
        country == "br" & horizon == "h1" ~ rmse / br_rmse_rw$rmse_rw[1],
        country == "br" & horizon == "h12" ~ rmse / br_rmse_rw$rmse_rw[2],
        country == "ru" & horizon == "h1" ~ rmse / ru_rmse_rw$rmse_rw[1],
        country == "ru" & horizon == "h12" ~ rmse / ru_rmse_rw$rmse_rw[2],
        country == "in" & horizon == "h1" ~ rmse / in_rmse_rw$rmse_rw[1],
        country == "in" & horizon == "h12" ~ rmse / in_rmse_rw$rmse_rw[2],
        country == "cn" & horizon == "h1" ~ rmse / cn_rmse_rw$rmse_rw[1],
        country == "cn" & horizon == "h12" ~ rmse / cn_rmse_rw$rmse_rw[2],
        country == "za" & horizon == "h1" ~ rmse / za_rmse_rw$rmse_rw[1],
        country == "za" & horizon == "h12" ~ rmse / za_rmse_rw$rmse_rw[2]
      )
    ))
}

# ___________________________________________________________________________________
# _________________________________ DIEBOLD MARIANO TEST ____________________________
# ___________________________________________________________________________________

dm_list <- list("dm_lm", "dm_svm_r", "dm_svm_l", "dm_tree", "dm_forest", "dm_gam")

for (dm in dm_list) {
  print(dm)
  assign(x = paste0("table_", dm), value = get(dm) %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame() %>%
    `colnames<-`(c("dm_test_p_value")))
}

# ___________________________________________________________________________________
# ____________________________________ EXPORT RESULTS _______________________________
# ___________________________________________________________________________________

results_lm <- merge(relative_rmse_lm, table_dm_lm, by = 0)
results_svm_l <- merge(relative_rmse_svm_l, table_dm_svm_l, by = 0)
results_svm_r <- merge(relative_rmse_svm_r, table_dm_svm_r, by = 0)
#results_mars <- merge(relative_rmse_mars, table_dm_mars, by = 0)
results_tree <- merge(relative_rmse_tree, table_dm_tree, by = 0)
results_forest <- merge(relative_rmse_forest, table_dm_forest, by = 0)
results_gam <- merge(relative_rmse_gam, table_dm_gam, by = 0)


rw_uni <- bind_rows(
  "br" = br_rmse_rw$rmse_rw,
  "ru" = ru_rmse_rw$rmse_rw,
  "in" = in_rmse_rw$rmse_rw,
  "cn" = cn_rmse_rw$rmse_rw,
  "za" = za_rmse_rw$rmse_rw
) %>%
  `rownames<-`(c("h1", "h12")) %>%
  rownames_to_column("horizonte")
results_br <- bind_rows(
  filter(results_lm, country == "br"),
  filter(results_svm_l, country == "br"),
  filter(results_svm_r, country == "br"),
  #filter(results_mars, country == "br"),
  filter(results_tree, country == "br"),
  filter(results_forest, country == "br"),
  filter(results_gam, country == "br"),
)
results_ru <- bind_rows(
  filter(results_lm, country == "ru"),
  filter(results_svm_l, country == "ru"),
  filter(results_svm_r, country == "ru"),
  #filter(results_mars, country == "ru"),
  filter(results_tree, country == "ru"),
  filter(results_forest, country == "ru"),
  filter(results_gam, country == "ru"),
)
results_in <- bind_rows(
  filter(results_lm, country == "in"),
  filter(results_svm_l, country == "in"),
  filter(results_svm_r, country == "in"),
  #filter(results_mars, country == "in"),
  filter(results_tree, country == "in"),
  filter(results_forest, country == "in"),
  filter(results_gam, country == "in"),
)
results_cn <- bind_rows(
  filter(results_lm, country == "cn"),
  filter(results_svm_l, country == "cn"),
  filter(results_svm_r, country == "cn"),
  #filter(results_mars, country == "cn"),
  filter(results_tree, country == "cn"),
  filter(results_forest, country == "cn"),
  filter(results_gam, country == "cn"),
)
results_za <- bind_rows(
  filter(results_lm, country == "za"),
  filter(results_svm_l, country == "za"),
  filter(results_svm_r, country == "za"),
  #filter(results_mars, country == "za"),
  filter(results_tree, country == "za"),
  filter(results_forest, country == "za"),
  filter(results_gam, country == "za"),
)
results_all <- bind_rows(
  results_lm,
  results_svm_l,
  results_svm_r,
  #results_mars,
  results_tree,
  results_forest,
  results_gam
)

writexl::write_xlsx(x = list(
  "RMSE Random Walk" = rw_uni,
  "Brasil" = results_br,
  "Índia" = results_in,
  "Rússia" = results_ru,
  "China" = results_cn,
  "África do Sul" = results_za,
  "Resultados" = results_all
), path = "outputs/Resultados.xlsx")
