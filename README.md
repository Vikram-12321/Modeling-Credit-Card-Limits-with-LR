# STA302 Final Project — Predicting Credit-Card Limits from Demographic and Behavioural Data

**Authors:** Vikram Bhojanala & Ellie Clarke  
**Date:** June 17, 2025 

## Overview
This project investigates the question:

> **To what extent do borrower demographics, historical repayment status, and financial behaviour explain variation in approved credit limits?** :contentReference[oaicite:1]{index=1}

Using the **Default of Credit Card Clients** dataset (30,000 Taiwanese credit-card clients, April–September 2005), we model **approved credit limits** with multiple predictors, emphasizing **interpretability** via linear regression. :contentReference[oaicite:2]{index=2}

## Dataset
- **Source:** “Default of Credit Card Clients” dataset (Taiwanese bank; 30,000 anonymous clients, 2005). :contentReference[oaicite:3]{index=3}  
- **Response:** `LIMIT_BAL` (credit limit in NT$). :contentReference[oaicite:4]{index=4}  
- **Predictors:**
  - Demographics: `SEX`, `EDUCATION`, `MARRIAGE`, `AGE` :contentReference[oaicite:5]{index=5}  
  - Repayment status: `PAY_0` … `PAY_6` :contentReference[oaicite:6]{index=6}  
  - Bills: `BILL_AMT1` … `BILL_AMT6` :contentReference[oaicite:7]{index=7}  
  - Payments: `PAY_AMT1` … `PAY_AMT6` :contentReference[oaicite:8]{index=8}  

## Methods
### Transformations & Diagnostics
Initial modeling on raw data showed skewness and heteroscedasticity. The final workflow:
1. **Box–Cox transform** on `LIMIT_BAL` (λ ≈ 0.3). :contentReference[oaicite:9]{index=9}  
2. **Signed-log transform** on `BILL_AMT*` and `PAY_AMT*` predictors:  
   `sign(x) * log(abs(x) + 1)` :contentReference[oaicite:10]{index=10}  
3. **Multicollinearity check:** VIFs for numeric predictors were all < 5 (no serious multicollinearity). :contentReference[oaicite:11]{index=11}  

### Influential Observations
We flagged observations that were simultaneously:
- high leverage,
- outliers,
- influential (Cook’s D / DFFITS / DFBETAS-style rule)

This identified **45 observations (0.15%)**, which were removed. Model fit improved slightly (Adjusted R² from 0.313 → 0.317) and estimates stabilized. :contentReference[oaicite:12]{index=12}  

### Model Selection
Final model selected using **backward stepwise selection with BIC** on the cleaned, fully transformed dataset, balancing fit and parsimony. :contentReference[oaicite:13]{index=13}  

## Key Findings (High-level)
- **Behaviour dominates demographics:** repayment status and recent payments drive most of the explained variation, while demographic effects (e.g., `AGE`, `SEX`) are smaller in magnitude. :contentReference[oaicite:14]{index=14}  
- **Delinquency is heavily penalized:** chronic/severe delinquency strongly reduces predicted limits. :contentReference[oaicite:15]{index=15}  
- **Recent activity matters most:** recent bills and payments are more informative than older transactions. :contentReference[oaicite:16]{index=16}  

## Model Performance
Performance metrics reported for the final chosen model set:
- BIC-optimal model Adj. R² ≈ **0.3141** (and comparable AIC/Adj R² variants are reported for comparison). :contentReference[oaicite:17]{index=17}  

