source("00_functions.R")
source("01_data.R")

rw_h1 <- data_panel[1:2] %>%
  mutate(
    rw1 = lag(br_exchange, 1)
  ) %>%
  slice_tail(n = -120) %>%
  mutate(
    mse1 = ((rw1 - br_exchange)**2) / nrow(.)
  )
rmse_h1 <- sqrt(sum(rw_h1$mse1))


rw_h1 <- data_panel[1:2] %>%
  mutate(
    rw1 = lag(br_exchange, 1)
  ) %>%
  slice_tail(n = -120) %>%
  mutate(
    diff = (rw1 - br_exchange),
    quad = (rw1 - br_exchange)**2
  )
rmse <- rw_h1$quad %>% mean() %>% sqrt()
rmse



rw_h12 <- data_panel[1:2] %>%
  mutate(
    rw1 = lag(br_exchange, 1),
    rw2 = lag(br_exchange, 2),
    rw3 = lag(br_exchange, 3),
    rw4 = lag(br_exchange, 4),
    rw5 = lag(br_exchange, 5),
    rw6 = lag(br_exchange, 6),
    rw7 = lag(br_exchange, 7),
    rw8 = lag(br_exchange, 8),
    rw9 = lag(br_exchange, 9),
    rw10 = lag(br_exchange, 10),
    rw11 = lag(br_exchange, 11),
    rw12 = lag(br_exchange, 12)
  ) %>%
  slice_tail(n = -120) %>%
  mutate(
    mse1 = ((rw1 - br_exchange)**2) / nrow(.),
    mse2 = ((rw2 - br_exchange)**2) / nrow(.),
    mse3 = ((rw3 - br_exchange)**2) / nrow(.),
    mse4 = ((rw4 - br_exchange)**2) / nrow(.),
    mse5 = ((rw5 - br_exchange)**2) / nrow(.),
    mse6 = ((rw6 - br_exchange)**2) / nrow(.),
    mse7 = ((rw7 - br_exchange)**2) / nrow(.),
    mse8 = ((rw8 - br_exchange)**2) / nrow(.),
    mse9 = ((rw9 - br_exchange)**2) / nrow(.),
    mse10 = ((rw10 - br_exchange)**2) / nrow(.),
    mse11 = ((rw11 - br_exchange)**2) / nrow(.),
    mse12 = ((rw12 - br_exchange)**2) / nrow(.)
  )
rmse_h12 <- sqrt(sum(rw_h12[, 26]))

model_rw <- data_panel[1:2] %>%
  mutate(
    rw1 = lag(br_exchange, 1),
    rw12 = lag(br_exchange, 12)
  ) %>%
  slice_tail(n = -120) %>%
  mutate(
    mse1 = ((rw1 - br_exchange)**2) / nrow(.),
    mse12 = ((rw12 - br_exchange)**2) / nrow(.)
  )
rmse_rw_h1 <- sqrt(sum(model_rw$mse1))
rmse_rw_h12 <- sqrt(sum(model_rw$mse12))



library(caret)

# a <- arima(x = painel$dolar_compra_mensal, order = c(0, 1, 0))
# b <- predict(a, n.ahead = 12)
# b$pred


#### Modelo no CARET

# RW_driftless <- list(
#  type = "Regression",
#  library = "stats",
#  loop = NULL
# )

# c <- forecast::auto.arima(painel$dolar_compra_mensal)
# c$x




arima_parameters <- function() {
  return(data.frame(
    parameter = c("p", "d", "q", "intercept"),
    class = c("integer", "integer", "integer", "logical"),
    label = c("Order AR", "Degree differencing", "Order MA", "Intercept"),
    stringsAsFactors = FALSE
  ))
}

arima_grid <- function(p = 5, d = 5, q = 5, intercept = TRUE) {
  return(function(x, y, len = NULL, search = "grid") {
    out <- data.frame(
      p = p, d = d, q = q, intercept = intercept,
      stringsAsFactors = FALSE
    )
    return(out)
  })
}

arima_fit <- function(...) {
  return(function(x, y, wts, param, lev, last, weights, classProbs) {
    if (is.null(y)) {
      y <- x
    }

    if (ncol(x) == 0) {
      m <- forecast::Arima(y = y, order = c(param$p, param$d, param$q), ...)
    } else {
      m <- forecast::Arima(y = y, xreg = x, order = c(param$p, param$d, param$q), ...)
    }

    return(m)
  })
}

auto_arima_fit <- function(...) {
  return(function(x, y, wts, param, lev, last, weights, classProbs) {
    # cat("ARIMA\n")
    if (ncol(x) == 0) {
      m <- forecast::auto.arima(y = y, max.p = param$p, max.d = param$d, max.q = param$q, start.p = 0, start.q = 0, seasonal = FALSE, ...)
    } else {
      m <- forecast::auto.arima(y = y, xreg = x, max.p = param$p, max.d = param$d, max.q = param$q, start.p = 0, start.q = 0, seasonal = FALSE, ...)
    }

    return(m)
  })
}

arima_predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
  if ("ts" %in% class(newdata)) {
    newdata <- zoo::coredata(newdata)
  }

  if (is.vector(newdata)) {
    modelFit <- forecast::Arima(newdata, model = modelFit)
    pred <- forecast::forecast(modelFit, h = length(newdata)) # , xreg = matrix(newdata, ncol = 1)
  } else if (ncol(newdata) == 0) {
    pred <- forecast::forecast(modelFit, h = nrow(newdata))
  } else {
    pred <- forecast::forecast(modelFit, xreg = newdata)
  }

  return(as.numeric(pred$mean))
}

arima_sort <- function(x) {
  return(x)
}

arima_varimp <- function(object, ...) {
  values <- abs(object$coef[object$xNames])

  out <- data.frame(values)
  colnames(out) <- "Overall"

  if (!is.null(names(values))) {
    rownames(out) <- names(values)
  }

  return(out)
}

arima_model <- function(p, d, q, intercept = TRUE, ...) {
  return(list(
    label = "ARIMA",
    type = "Regression",
    library = "forecast",
    loop = NULL,
    prob = NULL,
    parameters = arima_parameters(),
    grid = arima_grid(p, d, q, intercept),
    fit = arima_fit(...),
    predict = arima_predict,
    sort = arima_sort,
    varImp = arima_varimp
  ))
}

auto_arima_model <- function(p = 5, d = 2, q = 5, intercept = TRUE, ...) {
  return(list(
    label = "ARIMA",
    type = "Regression",
    library = "forecast",
    loop = NULL,
    prob = NULL,
    parameters = arima_parameters(),
    grid = arima_grid(p, d, q, intercept),
    fit = auto_arima_fit(...),
    predict = arima_predict,
    sort = arima_sort,
    varImp = arima_varimp
  ))
}



trainDirectFit <- function(...) {
  return(caret::trainControl(method = "none", number = 1, repeats = 1, ...))
}

#################################

library(forecast)

data(WWWusage) # from package "forecast"
df <- data.frame(y = as.numeric(WWWusage))

library(caret)

# ARIMA model of order (1, 1, 1)

arima1 <- caret::train(y ~ 1, data = df, method = arima_model(1, 1, 1), trControl = caret::trainControl(method = "none", number = 1, repeats = 1, ...))

summary(arima1)


# https://github.com/tungntdhtl/caret.ts/blob/master/R/trainControl.R
# teste <- train(method = "none", number = 1, repeats = 1,)
