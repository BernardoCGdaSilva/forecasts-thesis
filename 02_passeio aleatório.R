source("00_functions.R")
source("01_data.R")

library(caret)

#a <- arima(x = painel$dolar_compra_mensal, order = c(0, 1, 0))
#b <- predict(a, n.ahead = 12)
#b$pred


#### Modelo no CARET

#RW_driftless <- list(
#  type = "Regression",
#  library = "stats",
#  loop = NULL
#)

#c <- forecast::auto.arima(painel$dolar_compra_mensal)
#c$x




arima_parameters <- function() {
  return(data.frame(parameter = c("p", "d", "q", "intercept"),
                    class = c("integer", "integer", "integer", "logical"),
                    label = c("Order AR", "Degree differencing", "Order MA", "Intercept"),
                    stringsAsFactors = FALSE))
}

arima_grid <- function(p = 5, d = 5, q = 5, intercept = TRUE) {
  return(function(x, y, len = NULL, search = "grid") {
    out <- data.frame(p = p, d = d, q = q, intercept = intercept,
                      stringsAsFactors = FALSE)
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
  return(list(label = "ARIMA",
              type = "Regression",
              library = "forecast",
              loop = NULL,
              prob = NULL, 
              parameters = arima_parameters(),
              grid = arima_grid(p, d, q, intercept),
              fit = arima_fit(...),
              predict = arima_predict,
              sort = arima_sort,
              varImp = arima_varimp))
}

#https://github.com/tungntdhtl/caret.ts/blob/master/R/trainControl.R
#teste <- train(method = "none", number = 1, repeats = 1,)