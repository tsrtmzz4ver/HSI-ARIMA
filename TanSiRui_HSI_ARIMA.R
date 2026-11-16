knitr::opts_chunk$set(echo = FALSE, comment = "", fig.height = 3.5)
library(quantmod)
library(tseries)
library(forecast)

cat('<style>table { page-break-inside: avoid; }</style>')

getSymbols("^HSI", from = "2022-01-01", to = "2026-11-13")
hsi_price <- Cl(HSI)
log_return <- na.omit(diff(log(hsi_price)))



mu   <- mean(log_return)
sig  <- sd(log_return)
skew <- mean((log_return - mu)^3) / sig^3
kurt <- mean((log_return - mu)^4) / sig^4 - 3

desc <- data.frame(
  Statistic = c("Observations", "Mean", "Standard deviation", "Median",
                "Minimum", "Maximum", "Skewness", "Excess kurtosis"),
  Value = c(format(length(log_return), big.mark = ","),
            sprintf("%.5f", mu),
            sprintf("%.4f", sig),
            sprintf("%.5f", median(log_return)),
            sprintf("%.4f", min(log_return)),
            sprintf("%.4f", max(log_return)),
            sprintf("%.3f", skew),
            sprintf("%.3f", kurt))
)

knitr::kable(desc, row.names = FALSE, col.names = c("Statistic", "Value"),
      caption = "Summary statistics of the daily log returns")

plot(log_return, main = "HSI Daily Log Returns", ylab = "Log Return")

adf_res  <- adf.test(as.numeric(log_return))
kpss_res <- kpss.test(as.numeric(log_return))

knitr::kable(
  data.frame(
    Test        = c("ADF", "KPSS"),
    Null        = c("Unit root (non-stationary)", "Stationary"),
    Alternative = c("Stationary", "Non-stationary"),
    p_value     = c(round(adf_res$p.value, 4), round(kpss_res$p.value, 4))
  ),
  caption = "Summary of the stationarity tests"
)


{
par(mfrow = c(1, 2), mar = c(4.5, 4.2, 3.8, 1), mgp = c(2.6, 0.8, 0))
acf(log_return, main = "ACF of HSI Log Returns")
pacf(log_return, main = "PACF of HSI Log Returns")
}

fit2 <- Arima(log_return, order = c(1, 0, 2))
checkresiduals(fit2)


co <- coef(fit2)

knitr::kable(
  data.frame(
    Parameter = names(co),
    Estimate  = round(as.numeric(co), 4),
    Std.Error = round(sqrt(diag(vcov(fit2))), 4)
  ),
  row.names = FALSE,
  caption = "Estimated coefficients of the ARIMA(1,0,2) model"
)


b <- coef(fit2)
cat(sprintf("r_t = %+.4f r_(t-1) + a_t %+.4f a_(t-1) %+.4f a_(t-2),   sigma^2 = %.6f\n",
            b["ar1"], b["ma1"], b["ma2"], fit2$sigma2))


models <- list(
  "ARIMA(0,0,0) without constant" = Arima(log_return, order = c(0, 0, 0), include.mean = FALSE),
  "ARIMA(0,0,0) with constant"    = Arima(log_return, order = c(0, 0, 0), include.mean = TRUE),
  "ARIMA(1,0,0)"                  = Arima(log_return, order = c(1, 0, 0)),
  "ARIMA(0,0,1)"                  = Arima(log_return, order = c(0, 0, 1)),
  "ARIMA(1,0,1)"                  = Arima(log_return, order = c(1, 0, 1)),
  "ARIMA(1,0,2)"                  = Arima(log_return, order = c(1, 0, 2))
)

auto_fit <- auto.arima(log_return, seasonal = FALSE)

comparison <- data.frame(
  Model = names(models),
  AIC  = round(sapply(models, AIC), 2),
  AICc = round(sapply(models, function(m) m$aicc), 2),
  BIC  = round(sapply(models, BIC), 2)
)

comparison <- comparison[order(comparison$AICc), ]



knitr::kable(comparison, row.names = FALSE,
             caption = "Comparison of candidate ARIMA models")


m0 <- Arima(log_return, order = c(0, 0, 0))

lr_stat <- 2 * (as.numeric(logLik(fit2)) - as.numeric(logLik(m0)))
lr_p    <- pchisq(lr_stat, df = 3, lower.tail = FALSE)

cat(sprintf("LR statistic = %.2f, df = 3, p-value = %.3f\n", lr_stat, lr_p))


res <- as.numeric(residuals(fit2))

qqnorm(res, main = "Normal Q-Q Plot of Residuals")
qqline(res, col = "red")
tseries::jarque.bera.test(res)

tail_n <- 100

act_vals <- tail(as.numeric(log_return), tail_n)
fit_vals <- tail(as.numeric(fitted(fit2)), tail_n)

plot(act_vals, type = "l", col = "grey55",
     main = "Actual versus Fitted Log Returns (last 100 days)",
     xlab = "Trading days", ylab = "Log return")
lines(seq_along(fit_vals), fit_vals, col = "red", lwd = 2)
legend("topright", legend = c("Actual", "Fitted"),
       col = c("grey55", "red"), lty = 1, bty = "n")

fit_final   <- Arima(log_return, order = c(0, 0, 0), include.mean = FALSE)
fore_result <- forecast(fit_final, h = 20)

fore_tab <- as.data.frame(fore_result)
knitr::kable(fore_tab[1:6, ], digits = 6,
             caption = "First six horizons of the 20-step forecast")

plot(fore_result, include = 0,
     main = "20-Step Ahead Forecast of HSI Log Returns")


n <- length(log_return)

train_ts <- ts(as.numeric(log_return[1:floor(0.8 * n)]))
test_ts  <- ts(as.numeric(log_return[(floor(0.8 * n) + 1):n]),
               start = length(train_ts) + 1, frequency = 1)

fit_train <- Arima(train_ts, order = c(0, 0, 0), include.mean = FALSE)
fc_test   <- forecast(fit_train, h = length(test_ts))

knitr::kable(accuracy(fc_test, test_ts), digits = 4,
             caption = "Out-of-sample forecast accuracy")


actual <- as.numeric(test_ts)
pred   <- as.numeric(fc_test$mean)

plot(actual, type = "l", col = "black",
     main = "Out-of-sample Forecast versus Actual Returns",
     xlab = "Test set observations", ylab = "Log return")
lines(pred, col = "red", lty = 2)
legend("topright", legend = c("Actual", "Forecast"),
       col = c("black", "red"), lty = c(1, 2), bty = "n")

