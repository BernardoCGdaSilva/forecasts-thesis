# TCC forecast: train models
# Bernardo Cainelli Gomes da Silva
# Fev - 2023

t0 <- Sys.time()
# _________________________________________________________________________________
# _____________________________ HELPER FUNCTIONS __________________________________
# _________________________________________________________________________________

source("00_functions.R")
# source("01_data.R")

# _________________________________________________________________________________
# ________________________________ COUNTRY LOOP ___________________________________
# _________________________________________________________________________________

country_list <- c("br", "ru", "in", "cn", "za")
# country_list <- c("br")

output_models <- list()

for (cty in country_list) {
  print(cty)

  # get data
  data <- read_csv2(paste0("outputs/", cty, "_panel_data.csv"), col_types = "Dddddddddddd") %>% arrange(date)

  # Set seeds for reproducibility
  set.seed(123)
  seed_lenght <- nrow(data) - 120
  seeds <- vector(mode = "list", length = seed_lenght)
  for (i in 1:seed_lenght) seeds[[i]] <- sample.int(1000, 675)
  seeds[[seed_lenght + 1]] <- sample.int(1000, 1)

  # set number o cores for parallel processing
  registerDoParallel(cores = 8)

  # set the number of alternativa to metaparameters
  n_tuneLength <- 15

  # _________________________________________________________________________________
  # ________________________________ WINDOW LOOP ____________________________________
  # _________________________________________________________________________________

  for (wdw in c("rolling", "expanding")) {
    print(paste0(cty, "_", wdw))

    # set estimation window
    windown <- (wdw == "rolling")

    # Adjustments for cross validation
    cv_control <- trainControl(
      method = "timeslice",
      initialWindow = 120,
      horizon = 1,
      fixedWindow = windown,
      allowParallel = TRUE,
      savePredictions = TRUE,
      seeds = seeds,
      returnResamp = "all"
    )

    # _________________________________________________________________________________
    # ____________________ HORIZON LOOP AND MODELS (FORMULAS) _________________________
    # _________________________________________________________________________________

    for (hzn in c("h1", "h12")) {
      print(paste0(cty, "_", wdw, "_", hzn))

      # set train formula
      {
        # taylor
        form_taylor <- paste0(
          cty, "_exchange_rate_", hzn, " ~ diff_inflation_rate + diff_gap + real_exchange_rate"
        )
        # taylor ppp
        form_taylor_ppp <- paste0(
          cty, "_exchange_rate_", hzn, " ~ diff_inflation_rate + diff_gap"
        )
        # taylor ppp smoothing
        form_taylor_ppp_smoothing <- paste0(
          cty, "_exchange_rate_", hzn, " ~ diff_inflation_rate + diff_gap + diff_interest_lag"
        )
        # taylor smoothing
        form_taylor_smoothing <- paste0(
          cty, "_exchange_rate_", hzn, " ~ diff_inflation_rate + diff_gap + diff_interest_lag + real_exchange_rate"
        )
        # ppp
        form_ppp <- paste0(
          cty, "_exchange_rate_", hzn, " ~ real_exchange_rate"
        )
        # foward premium model
        form_foward_premium <- paste0(
          cty, "_exchange_rate_", hzn, " ~ diff_interest"
        )
        # monetary
        form_monetary <- paste0(
          cty, "_exchange_rate_", hzn, " ~ diff_m1 + diff_gdp"
        )
        # monetary with sticky prices
        form_monetary_sticky <- paste0(
          cty, "_exchange_rate_", hzn, " ~ diff_m1 + diff_gdp + diff_interest + diff_inflation_rate"
        )
        # monetary with sticky prices augmented by risk
        form_monetary_risk <- paste0(
          cty, "_exchange_rate_", hzn, " ~ diff_m1 + diff_gdp + diff_interest + diff_inflation_rate + vix"
        )
      }

      formula_list <- list(
        "form_taylor", "form_taylor_ppp", "form_taylor_ppp_smoothing",
        "form_taylor_smoothing", "form_ppp", "form_foward_premium", "form_monetary",
        "form_monetary_sticky", "form_monetary_risk"
      )

      # _________________________________________________________________________________
      # ___________________________ FORMULA LOOP AND METHODS ____________________________
      # _________________________________________________________________________________

      for (form in formula_list) {
        print(paste0(cty, "_", wdw, "_", hzn, "_", form))

        train_formula <- as.formula(get(form))

        # View(model.matrix(data = data, as.formula(train_formula)))
        # View(model.frame(data = data, as.formula(train_formula)))

        print(paste0(cty, "_", wdw, "_", hzn, "_", form, "_lm"))
        model_lm <- train(train_formula,
          data = data,
          method = "lm",
          trControl = cv_control,
          tuneLength = n_tuneLength,
          metric = "RMSE"
        )

        # print(paste0(cty, "_", wdw, "_", hzn, "_", form, "_ridge"))
        # model_ridge <- train(train_formula,
        #  data = data,
        #  method = "ridge",
        #  tuneLength = n_tuneLength,
        #  trControl = cv_control,
        #  metric = "RMSE"
        # )

        #  print("lasso")
        #  model_lasso <- train(train_formula,
        #    data = data,
        #    method = "lasso",
        #    tuneLength = n_tuneLength,
        #    trControl = cv_control,
        #    metric = "RMSE"
        #  )

        print(paste0(cty, "_", wdw, "_", hzn, "_", form, "_svm_r"))
        model_svm_r <- train(train_formula,
          data = data,
          method = "svmRadial",
          trControl = cv_control,
          tuneLength = n_tuneLength,
          metric = "RMSE"
        )

        print(paste0(cty, "_", wdw, "_", hzn, "_", form, "_svm_l"))
        model_svm_l <- train(train_formula,
          data = data,
          method = "svmLinear",
          trControl = cv_control,
          tuneLength = n_tuneLength,
          metric = "RMSE"
        )

        # print("svm_p")
        # model_svm_p <- train(train_formula,
        #  data = data,
        #  method = "svmPoly",
        #  trControl = cv_control,
        #  tuneLength = n_tuneLength,
        #  metric = "RMSE"
        # )

        print(paste0(cty, "_", wdw, "_", hzn, "_", form, "_tree"))
        model_tree <- train(train_formula,
          data = data,
          method = "rpart",
          tuneLength = n_tuneLength,
          trControl = cv_control,
          metric = "RMSE"
        )

        # print(paste0(cty, "_", wdw, "_", hzn, "_", form, "_ctree"))
        # model_conditional_tree <- train(train_formula,
        #  data = data,
        #  method = "ctree",
        #  tuneLength = n_tuneLength,
        #  trControl = cv_control,
        #  metric = "RMSE"
        # )

        # print(paste0(cty, "_", wdw, "_", hzn, "_", form, "_mars"))
        # model_mars <- train(train_formula,
        #  data = data,
        #  method = "earth",
        # tuneLength = n_tuneLength,
        #  tuneGrid = expand.grid(
        #    .degree = 1,
        #    .nprune = 2:25
        #  ),
        #  trControl = cv_control
        # )

        print(paste0(cty, "_", wdw, "_", hzn, "_", form, "_forest"))
        model_rf <- train(train_formula,
          data = data,
          method = "rf",
          tuneLength = n_tuneLength,
          trControl = cv_control,
          ntrees = 1000,
          metric = "RMSE"
        )

        # print(paste0(cty, "_", wdw, "_", hzn, "_", form, "_cforest"))
        # model_conditional_rf <- train(train_formula,
        #  data = data,
        #  method = "cforest",
        #  tuneLength = n_tuneLength,
        #  trControl = cv_control,
        #  metric = "RMSE"
        # )

        print(paste0(cty, "_", wdw, "_", hzn, "_", form, "_gam"))
        model_gam <- train(train_formula,
          data = data,
          method = "gam",
          # tuneLength = n_tuneLength,
          tuneGrid = data.frame(
            select = F,
            method = "GCV.Cp"
          ),
          trControl = cv_control,
          metric = "RMSE"
        )

        loop <- paste0(cty, "_", wdw, "_", hzn, "_", form)
        output_models[[loop]] <- list(
          "lm" = model_lm,
          # "ridge" = model_ridge,
          # "lasso" = model_lasso,
          "svm_r" = model_svm_r,
          "svm_l" = model_svm_l,
          # "ctree" = model_tree,
          # "mars" = model_mars,
          "tree" = model_tree,
          "forest" = model_rf,
          "gam" = model_gam
        )
      }
    }
  }
}

# export trained models
saveRDS(output_models, "outputs/output_models.rds")

tf <- Sys.time()
tf - t0
