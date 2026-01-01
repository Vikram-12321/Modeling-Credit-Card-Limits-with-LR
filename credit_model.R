
## Preliminaries
setwd("/Users/vikrambhojanala/Library/CloudStorage/OneDrive-Personal/Desktop/Classes/STA302/Final_Project")
df <- read.csv("default_of_credit_card_clients.csv", skip = 1)

## Libraries 
library(MASS)
library(broom)
library(psych)

## Variables
n <-  30000 # Number of observations
p <-  22 # Number of predictors

## Factorize Demographic Categorical Predictors
df$SEX       <- as.factor(df$SEX)
df$EDUCATION <- as.factor(df$EDUCATION)
df$MARRIAGE  <- as.factor(df$MARRIAGE)

## Factorize Payement Categorical Predictors
pay_vars <- paste0("PAY_", c(0,2:6))

stopifnot(all(pay_vars %in% names(df)))

df[pay_vars] <- lapply(df[pay_vars], function(x) {
  factor(x, ordered = FALSE)
})

colSums(is.na(df[pay_vars]))

## ---------------------------------------------------------------------------
##  0.  BASE MODEL  –  raw response and transformed 
## ---------------------------------------------------------------------------

## Raw model
model_raw <- lm(
  LIMIT_BAL ~ SEX + EDUCATION + MARRIAGE + AGE +
    PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
    BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 +
    PAY_AMT1  + PAY_AMT2  + PAY_AMT3  + PAY_AMT4  + PAY_AMT5  + PAY_AMT6,
  data = df,
  y    = TRUE
)

## LIMIT_BAL Transformation
bc   <- boxcox(model_raw, plot = FALSE)
λ <- bc$x[which.max(bc$y)] # λmax = 0.3
geom_mean <- exp(mean(log(df$LIMIT_BAL)))
LIMIT_BAL_bc <- geom_mean^(1-λ) * (df$LIMIT_BAL ^ λ - 1)/λ


## Predictor Transformations (Signed Log)
signed_log <- function(x) sign(x) * log(abs(x) + 1)

for (i in 1:6) {
  df[[paste0("",  "BILL_AMT", i)]] <- signed_log(df[[paste0("BILL_AMT", i)]])
  df[[paste0("",  "PAY_AMT",  i)]] <- signed_log(df[[paste0("PAY_AMT",  i)]])
}

## Transformed Model
model_bc <- lm(
  LIMIT_BAL_bc ~ SEX + EDUCATION + MARRIAGE + AGE +
    PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
    BILL_AMT1 + BILL_AMT2 + BILL_AMT3 +
    BILL_AMT4 + BILL_AMT5 + BILL_AMT6 +
    PAY_AMT1  + PAY_AMT2  + PAY_AMT3 +
    PAY_AMT4  + PAY_AMT5  + PAY_AMT6,
  data = df,
  y    = TRUE,
)


# add response column from transformed model 
df$LIMIT_BAL_bc <- LIMIT_BAL_bc

# Load broom for tidy model output
library(broom)

# Tidy summary with confidence intervals
model_summary <- tidy(model_bc, conf.int = TRUE)

# Optionally round values for presentation
model_summary <- within(model_summary, {
  estimate  <- round(estimate, 4)
  std.error <- round(std.error, 4)
  conf.low  <- round(conf.low, 4)
  conf.high <- round(conf.high, 4)
  p.value   <- signif(p.value, 3)
})

# Print to console
print(model_summary)


# Print table to console
print(model_summary)

model_metrics <- summary(model_bc)

# Extract key metrics
r2     <- model_metrics$r.squared
adj_r2 <- model_metrics$adj.r.squared
aic    <- AIC(model_bc)
bic    <- BIC(model_bc)

cat("R-squared:", round(r2, 4), "\n")
cat("Adjusted R-squared:", round(adj_r2, 4), "\n")
cat("AIC:", round(aic, 2), "\n")
cat("BIC:", round(bic, 2), "\n")



## Main Plots:
## ──────────────────────────────
## Plot Grid 1 – Model Residual Checks
## ──────────────────────────────
par(mfrow = c(2, 2))  # 2x2 grid

# 1. Standardized Residuals vs. Fitted
plot(model_bc$fitted.values, rstandard(model_bc),
     xlab = "Fitted Values",
     ylab = "Standardized Residuals",
     main = "Std Residuals vs. Fitted")
abline(h = 0, lty = 2, col = "red")

# 2. Histogram of Standardized Residuals
hist(rstandard(model_bc),
     main = "Histogram of Std Residuals",
     xlab = "Standardized Residuals",
     col = "lightblue", breaks = 50)

# 3. Q–Q Plot of Standardized Residuals
qqnorm(rstandard(model_bc),
       main = "Q–Q Plot of Std Residuals")
qqline(rstandard(model_bc), col = "red", lwd = 2)

# 4. Residuals vs. Fitted Values
plot(model_bc$fitted.values, resid(model_bc),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, lty = 2, col = "red")


## ──────────────────────────────
## Plot Grid 2 – Other Diagnostic Views
## ──────────────────────────────
par(mfrow = c(2, 2))  # reset 2x2 grid

# 5. Residuals vs. AGE
plot(df$AGE, resid(model_bc),
     xlab = "AGE",
     ylab = "Residuals",
     main = "Residuals vs. AGE")
abline(h = 0, lty = 2, col = "red")

plot(df$PAY_AMT1, resid(model_bc),
     xlab = "PAY_AMT1",
     ylab = "Residuals",
     main = "Residuals vs. PAY_AMT1")
abline(h = 0, lty = 2, col = "red")

# 6. Observed vs. Fitted
plot(model_bc$fitted.values, df$LIMIT_BAL_bc,
     xlab = "Fitted Values",
     ylab = "Observed (LIMIT_BAL_bc)",
     main = "Observed vs. Fitted")
abline(0, 1, col = "red", lty = 2)

# 7. Residuals vs. Observation Order
plot(rstandard(model_bc) ~ seq_along(rstandard(model_bc)),
     xlab = "Observation Order",
     ylab = "Standardized Residuals",
     main = "Std Residuals vs. Observation Order")
abline(h = 0, lty = 2, col = "red")


# Reset plot layout
par(mfrow = c(1, 1))



## FOR PLOTS (NOT NEEDED IN CODE)


## Uncorrelation of Errors Check
plot(rstandard(model_bc) ~ seq_along(rstandard(model_bc)),
     xlab = "Observation Order",
     ylab = "Standardized Residuals",
     main = "Residuals vs. Observation Order")
abline(h = 0, lty = 2)
## Multicolinearity Table
# List of numeric predictors
preds <- c("AGE",
           paste0("BILL_AMT", 1:6),
           paste0("PAY_AMT", 1:6))

# Initialize empty list to store results
results <- data.frame(
  Predictor = character(),
  R_squared = numeric(),
  VIF = numeric(),
  stringsAsFactors = FALSE
)

# Loop through predictors
for (pred in preds) {
  other_preds <- setdiff(preds, pred)
  formula_str <- paste(pred, "~", paste(other_preds, collapse = " + "))
  model <- lm(as.formula(formula_str), data = df)
  
  rss <- sum(resid(model)^2)
  sst <- sum((df[[pred]] - mean(df[[pred]]))^2)
  rj_squared <- 1 - rss / sst
  vif_val <- 1 / (1 - rj_squared)
  
  results <- rbind(results, data.frame(
    Predictor = pred,
    R_squared = round(rj_squared, 4),
    VIF = round(vif_val, 3)
  ))
}

# View the table
print(results)



## Residuals vs. Observation Order - For checking ind. of errors
plot(rstandard(model_bc) ~ seq_along(rstandard(model_bc)),
     xlab = "Observation Order",
     ylab = "Standardized Residuals",
     main = "Residuals vs. Observation Order")
abline(h = 0, lty = 2)

## Transformed LIMIT_BAL_bc

hist(df$LIMIT_BAL_bc,
     main = "Transformed LIMIT_BAL_bc",
     xlab = "Transformed Credit Limit",
     col = "lightgreen",
     breaks = 50)

## Summary from psych
summary_stats <- describe(df$LIMIT_BAL_bc)

# Pull Q1 and Q3
qs <- quantile(df$LIMIT_BAL_bc, probs = c(0.25, 0.75))

# Create nicely formatted table
summary_table <- data.frame(
  Statistic = c("n (observations)", "Min", "Q1", "Median", "Mean", "Q3", "Max", "SD", "Skewness", "Kurtosis"),
  Value = round(c(
    summary_stats$n,
    summary_stats$min,
    qs[1],                        # Q1
    summary_stats$median,
    summary_stats$mean,
    qs[2],                        # Q3
    summary_stats$max,
    summary_stats$sd,
    summary_stats$skew,
    summary_stats$kurtosis
  ), 3)
)

print(summary_table, row.names = FALSE)

# Custom summarizer function
summarize_variable <- function(x) {
  if (is.numeric(x)) {
    mean_val <- mean(x)
    sd_val   <- sd(x)
    med_val  <- median(x)
    iqr_val  <- IQR(x)
    
    sprintf("%.2f ± %.2f; %.2f [IQR = %.2f]", mean_val, sd_val, med_val, iqr_val)
    
  } else if (is.factor(x) || is.character(x)) {
    tbl <- table(x)
    prop <- prop.table(tbl) * 100
    paste0(
      sapply(names(tbl), function(k) {
        sprintf("%s = %s (%.2f%%)", k, tbl[[k]], prop[[k]])
      }),
      collapse = "; "
    )
  } else {
    return("Unsupported type")
  }
}

# Variable list (Box-Cox transformed LIMIT_BAL and signed-log predictors)
vars <- c("LIMIT_BAL", "AGE",
          paste0("BILL_AMT", 1:6),
          paste0("PAY_AMT", 1:6),
          paste0("PAY_", c(0,2:6)),
          "SEX", "EDUCATION", "MARRIAGE")

# Build summary table
summary_table <- data.frame(
  Variable = vars,
  Summary  = sapply(df[vars], summarize_variable),
  row.names = NULL
)

# View table
print(summary_table, row.names = FALSE)

