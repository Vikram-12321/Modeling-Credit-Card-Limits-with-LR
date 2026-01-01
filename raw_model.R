###############################################################################
##  Title: STA302 – Final Project (Raw-Scale Version)
##  Authors: Vikram Bhojanala, Ellie Clarke
##  Description: OLS on LIMIT_BAL (no transforms) + full diagnostic grids
###############################################################################

## ---------------------------------------------------------------------------
##  0.  SET-UP  –  load data and encode categorical variables
## ---------------------------------------------------------------------------

setwd("/Users/vikrambhojanala/Library/CloudStorage/OneDrive-Personal/Desktop/Classes/STA302/Final_Project")
df <- read.csv("default_of_credit_card_clients.csv", skip = 1)

library(MASS)

## Variables
n =  30000# Number of observations
p =  22# Number of predictors

df$SEX       <- as.factor(df$SEX)
df$EDUCATION <- as.factor(df$EDUCATION)
df$MARRIAGE  <- as.factor(df$MARRIAGE)

repayment_levels <- as.character(-1:9)
for (v in paste0("PAY_", c(0, 2:6))) {
  df[[v]] <- factor(df[[v]], levels = repayment_levels, ordered = TRUE)
}

## ---------------------------------------------------------------------------
##  1.  BASE MODEL  –  raw response
## ---------------------------------------------------------------------------

model_raw <- lm(
  LIMIT_BAL ~ SEX + EDUCATION + MARRIAGE + AGE +
    PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
    BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 +
    PAY_AMT1  + PAY_AMT2  + PAY_AMT3  + PAY_AMT4  + PAY_AMT5  + PAY_AMT6,
  data = df,
  y    = TRUE
)

bc   <- boxcox(model_raw, plot = FALSE)
λ <- bc$x[which.max(bc$y)] # λmax = 0.2
geom_mean <- exp(mean(log(df$LIMIT_BAL)))
LIMIT_BAL_transformed <- geom_mean^(1-λ) * (df$LIMIT_BAL ^ λ - 1)/λ


signed_log <- function(x) sign(x) * log(abs(x) + 1)

for (i in 1:6) {
  df[[paste0("",  "BILL_AMT", i)]] <- signed_log(df[[paste0("BILL_AMT", i)]])
  df[[paste0("",  "PAY_AMT",  i)]] <- signed_log(df[[paste0("PAY_AMT",  i)]])
}


model_transformed <- lm(
  LIMIT_BAL_transformed ~ SEX + EDUCATION + MARRIAGE + AGE +
    PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
    BILL_AMT1 + BILL_AMT2 + BILL_AMT3 +
    BILL_AMT4 + BILL_AMT5 + BILL_AMT6 +
    PAY_AMT1  + PAY_AMT2  + PAY_AMT3 +
    PAY_AMT4  + PAY_AMT5  + PAY_AMT6,
  data = df,
  y    = TRUE,
)

cat("=====  OLS Summary on Raw LIMIT_BAL  =====\n")
print(summary(model_transformed))

## ---------------------------------------------------------------------------
##  2.  DIAGNOSTIC PLOTS
## ---------------------------------------------------------------------------

# Convenience objects (all same length)
mf_trans   <- model.frame(model_transformed)
raw_res  <- resid(model_transformed)
std_res  <- rstandard(model_transformed)
fitted_v <- fitted(model_transformed)
response <- mf_trans$LIMIT_BAL_transformed

length(fitted_v)
length(raw_res)

# 2·1  Scatter-matrix (quick look at a few numerics)
pairs(
  df[, c("LIMIT_BAL", "AGE", "BILL_AMT1", "BILL_AMT2", "PAY_AMT1")],
  pch  = 18,
  main = "Scatterplot Matrix: LIMIT_BAL & Numeric Predictors"
)

# 2·2  Six-panel residual diagnostics (2 × 3 grid)
 par(mfrow = c(2, 3))

 plot(fitted_v, raw_res,
      main = "Residuals vs Fitted",
      xlab = "Fitted (LIMIT_BAL)", ylab = "Raw Residuals (e)")
 abline(h = 0, col = "red", lty = 2); 
 #lines(lowess(fitted_v, raw_res), col = "blue")

 plot(mf_trans$AGE, raw_res,
      main = "Residuals vs AGE",
      xlab = "AGE", ylab = "Raw Residuals (e)")
 abline(h = 0, col = "red", lty = 2); lines(lowess(mf_trans$AGE, raw_res), col = "blue")

 plot(mf_trans$BILL_AMT1, raw_res,
      main = "Residuals vs BILL_AMT1",
      xlab = "BILL_AMT1", ylab = "Raw Residuals (e)")
 abline(h = 0, col = "red", lty = 2); lines(lowess(mf_trans$BILL_AMT1, raw_res), col = "blue")

 hist(std_res,
      main = "Histogram of Std Residuals",
      xlab = "Standardized Residuals (r)", breaks = 30)

 qqnorm(std_res, main = "Q–Q Plot Std Residuals")
 qqline(std_res, col = "blue", lwd = 2)

 plot(fitted_v, response,
      main = "Response vs Fitted",
      xlab = "Fitted (LIMIT_BAL)", ylab = "Observed (LIMIT_BAL)")
 abline(0, 1, col = "blue", lwd = 2)

par(mfrow = c(1, 1))   # reset

# 2·3  Response-vs-Predictor grid  ──────────────────────────────────────────
resp_pred <- c("SEX", "EDUCATION", "MARRIAGE", "AGE")
               # "PAY_0", "PAY_2", "PAY_3", "PAY_4", "PAY_5","PAY_6", 
               # "BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6",
               # "PAY_AMT1", "PAY_AMT2", "PAY_AMT3", "PAY_AMT4", "PAY_AMT5", "PAY_AMT6")  

# Compute grid size automatically
n  <- length(resp_pred)
nc <- ceiling(sqrt(n))
nr <- ceiling(n / nc)
par(mfrow = c(nr, nc))

for (v in resp_pred) {
  plot(mf_trans[[v]], response,
       main = paste("Response vs", v),
       xlab = v, ylab = "Observed (LIMIT_BAL)")
  lines(lowess(mf_trans[[v]], response), col = "darkgreen")
}

par(mfrow = c(1, 1))   # final reset
###############################################################################
#  End of script
###############################################################################
