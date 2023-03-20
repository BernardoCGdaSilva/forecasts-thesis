# TCC forecast: train models
# Bernardo Cainelli Gomes da Silva
# Fev - 2023

# _________________________________________________________________________________
# _____________________________ HELPER FUNCTIONS __________________________________
# _________________________________________________________________________________

source("00_functions.R")
source("01_data.R")
library(caret)
library(doParallel)

# _________________________________________________________________________________
# _____________________________ PREPROCESS DATA ___________________________________
# _________________________________________________________________________________


data <- painel[, -c(1:2)]

# +n to inflation series to allow for log-diff

# data$inflacao_mensal <- data$inflacao_mensal+1
# data <- diff(as.matrix(log(data)))

# sample <- createTimeSlices(y = as.matrix(data), initialWindow = 120, horizon = 12, fixedWindow = TRUE)

# data_train <- data[sample$train[[1]],]
# data_test <- data[sample$test[[1]],]


# _________________________________________________________________________________
# __________________________________ MODEL _________________________________________
# _________________________________________________________________________________

#### creating sampling seeds ####
# Preciso utilizar o argumento seeds de trainControl para garantir reprodutibilidade utilizando computação paralela.
set.seed(123)
seeds <- vector(mode = "list", length = 124)
for (i in 1:123) seeds[[i]] <- sample.int(1000, 15)

## For the last model:
seeds[[124]] <- sample.int(1000, 1)




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



model_lm <- train(dolar_venda_mensal ~ .,
  data = data,
  method = "lm",
  trControl = myTimeControl,
  tuneLength = tuneLength.num,
  metric = "RMSE"
)
# lm.mod
# lm.mod$finalModel
# lm.mod$results
# lm.mod$control$index

model_svm <- train(dolar_venda_mensal ~ .,
  data = data,
  method = "svmRadial",
  trControl = myTimeControl,
  tuneLength = tuneLength.num,
  metric = "RMSE"
)

# svm.mod
# svm.mod$finalModel
# svm.mod$results
# svm.mod$control$index

model_rf <- train(dolar_venda_mensal ~ .,
  data = data,
  method = "ctree",
  tuneLength = tuneLength.num,
  trControl = myTimeControl,
  metric = "RMSE"
)

model_MARS <- train(dolar_venda_mensal ~ .,
  data = data,
  method = "earth",
  tuneGrid = expand.grid(
    .degree = 1, # Preciso entender o que são esses dois parâmetros e porque eles não podem ser estimados com CV
    .nprune = 2:25
  ),
  trControl = myTimeControl
)


modelos <- resamples(list(
  "Linear Regression" = model_lm,
  "Support Vector Machine" = model_svm,
  "Random Forest" = model_rf,
  "Splines adaptativos" = model_MARS
))
parallelplot(modelos, metric = "RMSE")
summary(modelos)

trellis.par.set(caretTheme())
dotplot(modelos, metric = "RMSE")

Prev_lm <- predict(model_lm, tail(data, 1))
Prev_lm
