# TCC forecast: train models
# Bernardo Cainelli Gomes da Silva
# Fev - 2023

# _________________________________________________________________________________
# _____________________________ HELPER FUNCTIONS __________________________________
# _________________________________________________________________________________

source("00_functions.R")
#source("01_data.R")

# _________________________________________________________________________________
# _____________________________ PREPROCESS DATA ___________________________________
# _________________________________________________________________________________

# First difference
data_diff <- data_panel[, -1]
data_diff <- diff(as.matrix(log(data_diff)))
rownames(data_diff) = seq(length=nrow(data_diff))

formula_diff_lin <- as.formula(paste0("br_exchange ~ ."))
formula_diff_nlin <- as.formula(paste0("br_exchange ~ (.)^2 +",
                                       "I(br_inflation^2) + I(us_inflation^2) +",
                                       "I(br_gdp^2) + I(us_gdp^2) + I(br_interest^2) +",
                                       "I(us_interest^2) + I(br_m1^2) + I(us_m1^2) +",
                                       "I(br_gap^2) + I(us_gap^2)"))
formula_international_diff <- as.formula(paste0("br_exchange ~ I(br_inflation - us_inflation) + ",
                                                "I(br_gdp - us_gdp) +",
                                                "I(br_interest - us_interest) + ",
                                                "I(br_m1 - us_m1) + I(br_gap - us_gap)"))

#model.frame(formula_international_diff, data = data_panel)
#terms(formula_international_diff)

# Error Correction



data_model_training <- as.data.frame(data_diff)
formula_model_training <- formula_diff_nlin

# _________________________________________________________________________________
# __________________________________ MODEL _________________________________________
# _________________________________________________________________________________

#### creating sampling seeds ####
# Preciso utilizar o argumento seeds de trainControl para garantir reprodutibilidade utilizando computação paralela.
set.seed(123)
seeds <- vector(mode = "list", length = 126)
for (i in 1:125) seeds[[i]] <- sample.int(1000, 15)

## For the last model:
seeds[[126]] <- sample.int(1000, 1)




registerDoParallel(cores = 4) # número de núcleos de processamento



# Ajustes da validação cruzada
myTimeControl <- trainControl(
  method = "timeslice",
  initialWindow = 120,
  horizon = 12,
  fixedWindow = T,
  allowParallel = TRUE,
  savePredictions = TRUE,
  seeds = seeds, # seeds para garantir reprodutibilidade
  returnResamp = "all"
) # Este argumento armazena as samples da validação cruzada

tuneLength.num <- 15 # para o ajuste de parâmetros, peço para o modelo testar quinze alternativas para cada parâmetro.



model_lm <- train(formula_model_training,
  data = data_model_training,
  method = "lm",
  trControl = myTimeControl,
  tuneLength = tuneLength.num,
  metric = "RMSE"
)
model_lm
model_lm$finalModel
model_lm$results
model_lm$control$index
model_lm$control$indexOut

model_svm_r <- train(formula_model_training,
  data = data_model_training,
  method = "svmRadial",
  trControl = myTimeControl,
  tuneLength = tuneLength.num,
  metric = "RMSE"
)

model_svm_l <- train(formula_model_training,
                   data = data_model_training,
                   method = "svmLinear",
                   trControl = myTimeControl,
                   tuneLength = tuneLength.num,
                   metric = "RMSE"
)

#model_svm_p <- train(dolar_venda_mensal ~ .,
#                     data = data_diff,
#                     method = "svmPoly",
#                     trControl = myTimeControl,
#                     tuneLength = tuneLength.num,
#                     metric = "RMSE"
#)

# svm.mod
# svm.mod$finalModel
# svm.mod$results
# svm.mod$control$index

model_tree <- train(formula_model_training,
  data = data_model_training,
  method = "ctree",
  tuneLength = tuneLength.num,
  trControl = myTimeControl,
  metric = "RMSE"
)

model_rf <- train(formula_model_training,
                    data = data_model_training,
                    method = "rf",
                    tuneLength = tuneLength.num,
                    trControl = myTimeControl,
                    metric = "RMSE"
)
#model_rf$results
#model_rf$pred %>% filter(mincriterion == 0.64) %>% str

model_MARS <- train(formula_model_training,
  data = data_model_training,
  method = "earth",
  tuneGrid = expand.grid(
    .degree = 1, # Preciso entender o que são esses dois parâmetros e porque eles não podem ser estimados com CV
    .nprune = 2:25
  ),
  trControl = myTimeControl
)

model_lasso <- train(formula_model_training,
                  data = data_model_training,
                  method = "lasso",
                  tuneLength = tuneLength.num,
                  trControl = myTimeControl,
                  metric = "RMSE"
)
#predictors(model_lasso)

model_ridge <- train(formula_model_training,
                     data = data_model_training,
                     method = "ridge",
                     tuneLength = tuneLength.num,
                     trControl = myTimeControl,
                     metric = "RMSE"
)


modelos <- resamples(list(
  "Linear Regression" = model_lm,
  "Support Vector Machine Kernel Radial" = model_svm_r,
  "Support Vector Machine Kernel Linear" = model_svm_l,
  #"Support Vector Machine Kernel Polinomial" = model_svm_p,
  "Tree" = model_tree,
  "Random Forest" = model_rf,
  "Adaptative Splines" = model_MARS,
  "Lasso" = model_lasso,
  "Ridge Regression" = model_ridge
))
parallelplot(modelos, metric = "RMSE")
summary(modelos)

trellis.par.set(caretTheme())
dotplot(modelos, metric = "RMSE")


#plot(varImp(model_svm_r))

#Prev_lm <- predict(model_lm, tail(data_diff, 1))
#Prev_lm
