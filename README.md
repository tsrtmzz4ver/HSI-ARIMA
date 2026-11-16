# Time Series Analysis of the Hang Seng Index

**Tan SiRui** ｜ Course: MATH7970

Daily log returns of the Hang Seng Index are modelled with an ARIMA specification and forecast twenty steps ahead. The sample runs from 4 January 2022 to 12 November 2026, giving 1,193 trading-day observations.

## Main findings

- Log returns are stationary by both the ADF and the KPSS tests, so the differencing order is set to zero.
- The ACF and PACF show no clear cut-off, so the model orders are settled by comparing six candidates on AIC, AICc and BIC.
- The zero-mean white noise specification ARIMA(0,0,0) attains the lowest AICc and BIC and is also the model returned by `auto.arima()`. All specifications with lag structure perform worse on both criteria and none of their coefficients is individually significant.
- The 20-step forecast is therefore flat at zero with constant-width intervals, and the out-of-sample evaluation shows accuracy no better than a naive benchmark.

## Files

| File | Description |
| --- | --- |
| `TanSiRui_HSI_ARIMA.Rmd` | Source file. Knit with the Knit button in RStudio or `rmarkdown::render()`. |
| `TanSiRui_HSI_ARIMA_Report.pdf` | Knitted output, nine pages. |

## Reproducing

The R packages `quantmod`, `tseries` and `forecast` are required. An internet connection is needed on the first run, because the data are downloaded from Yahoo Finance with `getSymbols("^HSI")`. The sample end date is fixed at 2026-11-13 in the data chunk, so that the reported figures stay reproducible after the submission date.
