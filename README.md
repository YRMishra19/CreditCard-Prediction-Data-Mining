Bank Credit Card Classification — Logistic Regression (R)

## Overview
A classification project using the UniversalBank dataset to predict whether a bank
customer is likely to hold/be issued a credit card, based on their financial and
demographic profile. Built as a team project.

## Dataset
- Source: UniversalBank.csv (5,000 customer records, 14 variables)
- **Target variable: CreditCard (binary — holds credit card or not)
- Predictors: Age, Experience, Income, Family size, CCAvg, Education, Mortgage,
  Personal Loan, Securities Account, CD Account, Online banking usage

## Approach
1. Exploratory Data Analysis** — examined distributions of Age, Experience, Income,
   and CCAvg; visualized categorical splits (Personal Loan, Securities Account, CD
   Account) using histograms and bar plots
2. Data Cleaning** — converted Education and Family into categorical variables;
   removed non-predictive columns (ID, ZIP Code)
3. Outlier Detection** — used boxplots to flag outliers in Income and CCAvg
4. Heteroscedasticity Testing** — ran a Breusch-Pagan test (bptest) and corrected
   standard errors using HC1 robust estimation (vcovHC)
5. Logistic Regression Modeling** — iteratively tested predictors, narrowing down to
   CD Account, Securities Account, Personal Loan, and Family size as the most
   statistically significant drivers of credit card ownership
6. Model Evaluation** — split data into training/test sets (70/30), determined the
   optimal classification cutoff, and evaluated performance using a confusion matrix

## Tools & Libraries 
R, lmtest, sandwich, tidyverse, ggplot2, caTools, caret, ISLR, InformationValue

## Contributors
Yash Ramakant Mishra
www.yrmishra.com
