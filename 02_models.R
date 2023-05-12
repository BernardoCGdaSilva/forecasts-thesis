# TCC forecast: train models
# Bernardo Cainelli Gomes da Silva
# Fev - 2023

# _________________________________________________________________________________
# _____________________________ HELPER FUNCTIONS __________________________________
# _________________________________________________________________________________

source("00_functions.R")
source("01_data.R")

# _________________________________________________________________________________
# _____________________________ PREPROCESS DATA ___________________________________
# _________________________________________________________________________________

# First difference
data_diff <- painel[, -c(1:2)]
data_diff <- diff(as.matrix(log(data_diff)))
rownames(data_diff) = seq(length=nrow(data_diff))

formula_diff_lin <- as.formula(paste0("dolar_venda_mensal ~ ."))
formula_diff_nlin <- as.formula(paste0("dolar_venda_mensal ~ (.)^2 +",
                                       "I(M1_mensal^2) + I(inflacao_mensal^2) +",
                                       "I(selic_mensal^2) + I(pib_mensal^2) + I(hiato^2)"))

#model.frame(formula_diff_nlin, data = painel)
#terms(formula_diff_nlin)

# Error Correction


# _________________________________________________________________________________
# __________________________________ MODEL _________________________________________
# _________________________________________________________________________________

#### creating sampling seeds ####
# Preciso utilizar o argumento seeds de trainControl para garantir reprodutibilidade utilizando computação paralela.
set.seed(123)
seeds <- vector(mode = "list", length = 125)
for (i in 1:124) seeds[[i]] <- sample.int(1000, 15)

## For the last model:
seeds[[125]] <- sample.int(1000, 1)




registerDoParallel(cores = 4) # número de núcleos de processamento



# Ajustes da validação cruzada
myTimeControl <- trainControl(
  method = "timeslice",
  initialWindow = 120,
  horizon = 12,
  fixedWindow = TRUE,
  allowParallel = TRUE,
  savePredictions = TRUE,
  seeds = seeds, # seeds para garantir reprodutibilidade
  returnResamp = "all"
) # Este argumento armazena as samples da validação cruzada

tuneLength.num <- 15 # para o ajuste de parâmetros, peço para o modelo testar quinze alternativas para cada parâmetro.



model_lm <- train(formula_diff_nlin,
  data = data_diff,
  method = "lm",
  trControl = myTimeControl,
  tuneLength = tuneLength.num,
  metric = "RMSE"
)
#model_lm
#model_lm$finalModel
#model_lm$results
#model_lm$control$index

model_svm_r <- train(formula_diff_nlin,
  data = data_diff,
  method = "svmRadial",
  trControl = myTimeControl,
  tuneLength = tuneLength.num,
  metric = "RMSE"
)

model_svm_l <- train(formula_diff_nlin,
                   data = data_diff,
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

model_rf <- train(formula_diff_nlin,
  data = data_diff,
  method = "ctree",
  tuneLength = tuneLength.num,
  trControl = myTimeControl,
  metric = "RMSE"
)

model_MARS <- train(formula_diff_nlin,
  data = data_diff,
  method = "earth",
  tuneGrid = expand.grid(
    .degree = 1, # Preciso entender o que são esses dois parâmetros e porque eles não podem ser estimados com CV
    .nprune = 2:25
  ),
  trControl = myTimeControl
)

model_lasso <- train(formula_diff_nlin,
                  data = data_diff,
                  method = "lasso",
                  tuneLength = tuneLength.num,
                  trControl = myTimeControl,
                  metric = "RMSE"
)
#predictors(model_lasso)

model_ridge <- train(formula_diff_nlin,
                     data = data_diff,
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
  "Random Forest" = model_rf,
  "Adaptative Splines" = model_MARS,
  "Lasso" = model_lasso,
  "Ridge Regression" = model_ridge
))
parallelplot(modelos, metric = "RMSE")
summary(modelos)

trellis.par.set(caretTheme())
dotplot(modelos, metric = "RMSE")

#Prev_lm <- predict(model_lm, tail(data_diff, 1))
#Prev_lm
