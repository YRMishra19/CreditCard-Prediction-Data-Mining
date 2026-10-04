getwd()

uni_b_df <- read.csv('UniversalBank.csv')

library(lmtest)
library(sandwich)
library(tidyverse)

## preliminary analysis ##
dim(uni_b_df)

# null values in data #
sum(is.na(uni_b_df))

# we will check the distribution of numeric variables #
# and plot bar chart for categorical ones. #

# histogram for age #
hist(uni_b_df$Age, main = 'distribution of age', xlab = 'Age', freq = F, col = 'violet')
lines(density(uni_b_df$Age), lwd = 4, col = "red")

# histogram for Experience #
hist(uni_b_df$Experience, main = 'distribution of Experience', xlab = 'Experience', freq = F, col = 'blue')
lines(density(uni_b_df$Experience), lwd = 4, col = "red")

# histogram for Income #
hist(uni_b_df$Income, main = 'distribution of income', xlab = 'Income', freq = F, col = 'grey')
lines(density(uni_b_df$Income), lwd = 4, col = "black")

# histogram for CCAvg #
hist(uni_b_df$CCAvg, main = 'distribution of CCAvg', xlab = 'CCAvg', freq = F, col = 'white')
lines(density(uni_b_df$CCAvg), col = 'black', lwd = 4)

# barplot for personal loan #
counts <- table(uni_b_df$Personal.Loan)
barplot(counts, main = 'number of people who have taken loan', xlab = 'personal loan', col = c('red', 'blue'), legend = rownames(counts))

# barplot for securities account #
counts_sc <- table(uni_b_df$Securities.Account)
barplot(counts_sc, main = 'number of people who have securities account', xlab = 'securities account', col = c('green', 'yellow'), legend = rownames(counts_sc))

# barplot for CD Account #
counts_cd <- table(uni_b_df$CD.Account)
barplot(counts_cd, main = 'number of people who have CD account', xlab = 'CD Account', col = c('red', 'yellow'), legend = rownames(counts_cd))

######################Yash Mishra#########################################
# data types in data frame #
str(uni_b_df)
summary(uni_b_df)

# converting education, family in categorical variables #
uni_b_df$Education <- as.factor(uni_b_df$Education)
uni_b_df$Family <- as.factor(uni_b_df$Family)

# taking out columns which are not be used in analysis #
uni_b_df <- subset(uni_b_df, select = -c(ID, ZIP.Code))

# we will check the outliers using the bar plot. #
boxplot(uni_b_df$Experience, 
        main = 'Experience of person',
        xlab = 'Experience')
boxplot(uni_b_df$CCAvg,
        main = 'CCavg',
        xlab = 'CCavg')
boxplot(uni_b_df$Income,
        main = 'income of person',
        xlab = 'income')
boxplot(uni_b_df$Age,
        main = 'Age of person',
        xlab = 'Age')
boxplot(uni_b_df$Education,
        main = 'Education of person',
        xlab = 'education')
boxplot(uni_b_df$Family,
        main = 'number of members in Family',
        xlab = 'family')

##there are outliers in CCavg and income variables.##
##########################################################################
#############################################################################
#testing assumptions 
uni_b_df.ols <- lm(CreditCard ~ Family, data = uni_b_df)
uni_b_df$resi <- uni_b_df.ols$residuals

var.func <- lm(resi^2 ~ CreditCard, data = uni_b_df)
summary(var.func)

library(lmtest)
bptest(uni_b_df.ols)
#If the p-value is less than the level of significance (in this case if the p-value is less 
#than α=0.05), then you reject the null hypothesis. Since 0.006579 < 0.05, we can reject
#the null hypothesis.

summary(uni_b_df.ols)

library(lmtest)
library(sandwich)
coeftest(uni_b_df.ols, vcov = vcovHC(uni_b_df.ols, "HC1"))

#####################################Viren Patel##################################

# now we will do logistic regression to understand the relationship whether a person should be issued credeit card or not, #
log_reg <- glm(CreditCard ~ ., data = uni_b_df, family = binomial)
coeftest(log_reg, vcov. = vcovHC(log_reg, type = 'HC1'))

##mortgage, personal loan, securities account, CD. account and online.##

ggplot(data = uni_b_df) + geom_point(mapping = aes(x = CD.Account, y = CreditCard)) +
  geom_abline(slope = 3.67, intercept = -0.60, color = 'red')


# as we can see we do not get a clear idea using these much variables. #
# now we will try doing separate analysis. #

log_reg_1 <- glm(CreditCard ~  CD.Account, data = uni_b_df, family = binomial)
coeftest(log_reg_1, vcov. = vcovHC(log_reg_1, type = 'HC1'))


ggplot(data = uni_b_df) + geom_point(mapping = aes(x = CD.Account, y = CreditCard)) +
  geom_abline(slope = 2.39, intercept = -1.03, color = 'red')

dim(uni_b_df[uni_b_df$CreditCard == 1,])

# trying regression using other variables which were statistically significant. #
log_reg_2 <- glm(CreditCard ~ CD.Account + Securities.Account + Personal.Loan + Family, data = uni_b_df, family = binomial ) 
coeftest(log_reg_2, vcov. = vcovHC(log_reg_2, type = 'HC1'))

ggplot(data = uni_b_df) + geom_point(mapping = aes(x = CD.Account, y = CreditCard)) +
  geom_abline(slope = 3.53, intercept = -0.999, color = 'red')

######################################################################################################################################################

# now we will check how well this model works.#
# splitting this data in training and test data set.#
# separating out the dataset #

uni_b_df_1 <- subset(uni_b_df, select = c(CreditCard,CD.Account, Securities.Account, Personal.Loan, Family))

set.seed(1)
library(caTools)

sample <- sample.split(uni_b_df_1, SplitRatio = 0.70)
train_df <- subset(uni_b_df_1, sample == T)
test_df <- subset(uni_b_df_1, sample == F)

dim(train_df)
dim(test_df)

library(dplyr)
library(caret) 
library(ISLR)
library(InformationValue)
log_reg_t <- glm(CreditCard ~ ., data = train_df, family = "binomial")
summary(log_reg_t)

lmpred.train<- predict(log_reg_t, data = train_df)
length(lmpred.train)
lmpred.valid<- predict(log_reg_t, data = test_df)
length(lmpred.valid)
optimal_test <- optimalCutoff(test_df$CreditCard, lmpred.valid)[1]

optimal_train <- optimalCutoff(train_df$CreditCard, lmpred.train)[1]
lmpred.train <- ifelse(lmpred.train >= optimal_test, 1, 0)
lmpred.valid <- ifelse(lmpred.valid >= optimal_test, 1, 0)
length(lmpred.valid)
length(test_df$CreditCard)

library(ISLR)
library(InformationValue)
confusionMatrix(lmpred.train, train_df$CreditCard)