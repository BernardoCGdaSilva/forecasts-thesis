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
rmse_mars <- list()
rmse_ctree <- list()
country_list <- c("br", "ru", "in", "cn", "za")
model_list <- c("taylor", "taylor_ppp", "taylor_ppp_smoothing", "taylor_smoothing", "ppp", "monetary", "foward_premium")
window_list <- c("rolling", "expanding")
h_list <- c("h1", "h12")

# Linear models
for (cty in country_list) {
  for (wdw in window_list) {
    for (h in h_list) {
      for (mdl in model_list) {
        rmse_lm[[paste0("rmse_", cty, "_", wdw, "_", h, "_", mdl, "_lm")]] <- c(RMSE(
          pred = models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["lm"]]$pred[, "pred"],
          obs = models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["lm"]]$pred[, "obs"]
        ), cty, h)
      }
    }
  }
}

# SVM radial
for (cty in country_list) {
  for (wdw in window_list) {
    for (h in h_list) {
      for (mdl in model_list) {
        var <- filter(
          models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["svm_r"]]$pred,
          sigma == models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["svm_r"]]$bestTune[[1]] &
            C == models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["svm_r"]]$bestTune[[2]]
        )

        rmse_svm_r[[paste0("rmse_", cty, "_", wdw, "_", h, "_", mdl, "_svm_r")]] <- c(RMSE(
          pred = var$pred,
          obs = var$obs
        ), cty, h)
      }
    }
  }
}

# SVM linear
for (cty in country_list) {
  for (wdw in window_list) {
    for (h in h_list) {
      for (mdl in model_list) {
        var <- filter(
          models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["svm_l"]]$pred,
          C == models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["svm_l"]]$bestTune[[1]]
        )

        rmse_svm_l[[paste0("rmse_", cty, "_", wdw, "_", h, "_", mdl, "_svm_l")]] <- c(RMSE(
          pred = var$pred,
          obs = var$obs
        ), cty, h)
      }
    }
  }
}

# MARS
for (cty in country_list) {
  for (wdw in window_list) {
    for (h in h_list) {
      for (mdl in model_list) {
        var <- filter(
          models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["mars"]]$pred,
          nprune == models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["mars"]]$bestTune[[1]] &
            degree == models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["mars"]]$bestTune[[2]]
        )

        rmse_mars[[paste0("rmse_", cty, "_", wdw, "_", h, "_", mdl, "_mars")]] <- c(RMSE(
          pred = var$pred,
          obs = var$obs
        ), cty, h)
      }
    }
  }
}

# CTree
for (cty in country_list) {
  for (wdw in window_list) {
    for (h in h_list) {
      for (mdl in model_list) {
        var <- filter(
          models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["ctree"]]$pred,
          mincriterion == models[[paste0(cty, "_", wdw, "_", h, "_form_", mdl)]][["ctree"]]$bestTune[[1]]
        )

        rmse_ctree[[paste0("rmse_", cty, "_", wdw, "_", h, "_", mdl, "_ctree")]] <- c(RMSE(
          pred = var$pred,
          obs = var$obs
        ), cty, h)
      }
    }
  }
}

# ___________________________________________________________________________________
# _________________________________ RELATIVE RMSE ___________________________________
# ___________________________________________________________________________________

method_list <- list("rmse_lm", "rmse_svm_r", "rmse_svm_l", "rmse_mars", "rmse_ctree")

for (mtd in method_list) {
  print(mtd)
  assign(x = paste0("relative_", mtd), value = get(mtd) %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame() %>% `colnames<-`(c("rmse", "country", "horizon")) %>%
    mutate(
      rmse = as.double(rmse),
      relative_rmse = case_when(
        country == "br" & horizon == "h1" ~ rmse / br_rmse_rw[1],
        country == "br" & horizon == "h12" ~ rmse / br_rmse_rw[2],
        country == "ru" & horizon == "h1" ~ rmse / ru_rmse_rw[1],
        country == "ru" & horizon == "h12" ~ rmse / ru_rmse_rw[2],
        country == "in" & horizon == "h1" ~ rmse / in_rmse_rw[1],
        country == "in" & horizon == "h12" ~ rmse / in_rmse_rw[2],
        country == "cn" & horizon == "h1" ~ rmse / cn_rmse_rw[1],
        country == "cn" & horizon == "h12" ~ rmse / cn_rmse_rw[2],
        country == "za" & horizon == "h1" ~ rmse / za_rmse_rw[1],
        country == "za" & horizon == "h12" ~ rmse / za_rmse_rw[2]
      )
    ))
}







# PRovisório

rw_unificado <- bind_rows(
  "br" = br_rmse_rw,
  "ru" = ru_rmse_rw,
  "in" = in_rmse_rw,
  "cn" = cn_rmse_rw,
  "za" = za_rmse_rw
) %>%
  `rownames<-`(c("h1", "h12"))%>% rownames_to_column("horizonte")
rmse_br <- bind_rows(
  filter(relative_rmse_lm, country == "br"),
  filter(relative_rmse_mars, country == "br"),
  filter(relative_rmse_svm_l, country == "br"),
  filter(relative_rmse_svm_r, country == "br"),
  filter(relative_rmse_ctree, country == "br")
)%>% rownames_to_column("especificação")
rmse_ru <- bind_rows(
  filter(relative_rmse_lm, country == "ru"),
  filter(relative_rmse_mars, country == "ru"),
  filter(relative_rmse_svm_l, country == "ru"),
  filter(relative_rmse_svm_r, country == "ru"),
  filter(relative_rmse_ctree, country == "ru")
)%>% rownames_to_column("especificação")
rmse_in <- bind_rows(
  filter(relative_rmse_lm, country == "in"),
  filter(relative_rmse_mars, country == "in"),
  filter(relative_rmse_svm_l, country == "in"),
  filter(relative_rmse_svm_r, country == "in"),
  filter(relative_rmse_ctree, country == "in")
)%>% rownames_to_column("especificação")
rmse_cn <- bind_rows(
  filter(relative_rmse_lm, country == "cn"),
  filter(relative_rmse_mars, country == "cn"),
  filter(relative_rmse_svm_l, country == "cn"),
  filter(relative_rmse_svm_r, country == "cn"),
  filter(relative_rmse_ctree, country == "cn")
)%>% rownames_to_column("especificação")
rmse_za <- bind_rows(
  filter(relative_rmse_lm, country == "za"),
  filter(relative_rmse_mars, country == "za"),
  filter(relative_rmse_svm_l, country == "za"),
  filter(relative_rmse_svm_r, country == "za"),
  filter(relative_rmse_ctree, country == "za")
) %>% rownames_to_column("especificação")

writexl::write_xlsx(x = list(
  "RMSE Random Walk" = rw_unificado,
  "RMSE Brasil" = rmse_br,
  "RMSE Índia" = rmse_in,
  "RMSE Rússia" = rmse_ru,
  "RMSE China" = rmse_cn,
  "RMSE África do Sul" = rmse_za
), path = "outputs/rmse.xlsx")
