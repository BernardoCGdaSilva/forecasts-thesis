# TCC forecast: train models
# Bernardo Cainelli Gomes da Silva
# Fev - 2023

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
  for (i in 1:seed_lenght) seeds[[i]] <- sample.int(1000, 15)
  seeds[[seed_lenght + 1]] <- sample.int(1000, 1)

  # set number o cores for parallel processing
  registerDoParallel(cores = 4)

  # set the number of alternativa to metaparameters
  n_tuneLength <- 15

  # _________________________________________________________________________________
  # ________________________________ WINDOW LOOP ____________________________________
  # _________________________________________________________________________________

  for (wdw in c("rolling", "expanding")) {
    print(wdw)

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
    # ________________________________ HORIZON LOOP ___________________________________
    # _________________________________________________________________________________

    for (hzn in c("h1", "h12")) {
      print(hzn)

      # set train formula
      {
        # taylor
        form_taylor <- paste0(
          cty, "_exchange_rate_", hzn, " ~ I(", cty, "_inflation_rate - us_inflation_rate) + I(", cty,
          "_gap - us_gap) + I(log(", cty, "_exchange) + log(us_inflation) - log(", cty, "_inflation))"
        )
        # taylor ppp
        form_taylor_ppp <- paste0(
          cty, "_exchange_rate_", hzn, " ~ I(", cty, "_inflation_rate - us_inflation_rate) + I(", cty, "_gap - us_gap)"
        )
        # taylor ppp smoothing
        form_taylor_ppp_smoothing <- paste0(
          cty, "_exchange_rate_", hzn, " ~ I(", cty, "_inflation - us_inflation) + I(", cty,
          "_gap - us_gap) + I(", cty, "_interest - us_interest)"
        )
        # taylor smoothing
        form_taylor_smoothing <- paste0(
          cty, "_exchange_rate_", hzn, " ~ I(", cty, "_inflation - us_inflation) + I(", cty,
          "_gap - us_gap) + I(", cty, "_interest - us_interest) + I(log(", cty,
          "_exchange) + log(us_inflation) - log(", cty, "_inflation))"
        )
        # ppp
        form_ppp <- paste0(
          cty, "_exchange_rate_", hzn, " ~ I(log(", cty, "_exchange) + log(us_inflation) - log(", cty, "_inflation))"
        )
        # monetary
        form_monetary <- paste0(
          cty, "_exchange_rate_", hzn, " ~ I(log(", cty, "_exchange) -((", cty, "_m1 - us_m1)-(", cty, "_gdp - us_gdp)))"
        )
        # foward premium model
        form_foward_premium <- paste0(
          cty, "_exchange_rate_", hzn, " ~ I(", cty, "_interest - us_interest)"
        )
      }

      formula_list <- list(
        "form_taylor", "form_taylor_ppp", "form_taylor_ppp_smoothing",
        "form_taylor_smoothing", "form_ppp", "form_monetary", "form_foward_premium"
      )

      # _________________________________________________________________________________
      # ___________________________ FORMULA LOOP AND TRAIN_______________________________
      # _________________________________________________________________________________

      for (form in formula_list) {
        print(form)

        train_formula <- as.formula(get(form))

        # View(model.matrix(data = data, as.formula(train_formula)))
        # View(model.frame(data = data, as.formula(train_formula)))

        print("lm")
        model_lm <- train(train_formula,
          data = data,
          method = "lm",
          trControl = cv_control,
          tuneLength = n_tuneLength,
          metric = "RMSE"
        )

        # print("ridge")
        # model_ridge <- train(train_formula,
        #  data = data,
        #  method = "ridge",
        #  tuneLength = n_tuneLength,
        #  trControl = cv_control,
        #  metric = "RMSE"
        # )

        # print("lasso")
        # model_lasso <- train(train_formula,
        #  data = data,
        # method = "lasso",
        #  tuneLength = n_tuneLength,
        #  trControl = cv_control,
        #  metric = "RMSE"
        # )

        print("svm_r")
        model_svm_r <- train(train_formula,
          data = data,
          method = "svmRadial",
          trControl = cv_control,
          tuneLength = n_tuneLength,
          metric = "RMSE"
        )

        print("svm_l")
        model_svm_l <- train(train_formula,
          data = data,
          method = "svmLinear",
          trControl = cv_control,
          tuneLength = n_tuneLength,
          metric = "RMSE"
        )

        print("tree")
        model_tree <- train(train_formula,
          data = data,
          method = "ctree",
          tuneLength = n_tuneLength,
          trControl = cv_control,
          metric = "RMSE"
        )

        print("mars")
        model_mars <- train(train_formula,
          data = data,
          method = "earth",
          tuneGrid = expand.grid(
            .degree = 1, # Preciso entender o que são esses dois parâmetros e porque eles não podem ser estimados com CV
            .nprune = 2:25
          ),
          trControl = cv_control
        )

        # model_rf <- train(train_formula,
        # data = data,
        # method = "rf",
        # tuneLength = n_tuneLength,
        # trControl = cv_control,
        # metric = "RMSE"
        # )

        loop <- paste0(cty, "_", wdw, "_", hzn, "_", form)
        output_models[[loop]] <- list(
          "lm" = model_lm,
          # "ridge" = model_ridge, "lasso" = model_lasso,
          "svm_r" = model_svm_r, "svm_l" = model_svm_l,
          "ctree" = model_tree, "mars" = model_mars
        )
      }
    }
  }
}

# export trained models
saveRDS(output_models, "outputs/output_models.rds")
