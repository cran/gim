## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  glm(formula, family, data)

## ---- eval = FALSE------------------------------------------------------------
#  for(m in model){
#    glm(m$formula, family, data)
#  }

## -----------------------------------------------------------------------------
N <- 800
set.seed(0)
sex <- sample(c('F', 'M'), N, TRUE, c(.4, .6))
snp <- rbinom(N, 2, c(.4, .3, .3))
age <- runif(N, 20, 60)
pr <- plogis(.1 + .5 * I(sex == 'F') + .5 * I(age > 40) - .2 * snp)
cancer <- rbinom(N, 1, pr)
ext <- data.frame(cancer, sex, snp, age, stringsAsFactors = FALSE)

## -----------------------------------------------------------------------------
m1 <- glm(cancer ~ sex, data = ext, family = "binomial")
m2 <- glm(cancer ~ I(age < 30) + I(age > 50), data = ext, family = "binomial")
m3 <- glm(cancer ~ I(snp == 0), data = ext, family = "binomial")

## -----------------------------------------------------------------------------
summary(m2)$coef

## -----------------------------------------------------------------------------
model1 <- list(formula = 'cancer ~sex', 
               info = data.frame(var = names(coef(m1))[-1], 
                                 bet = coef(m1)[-1], stringsAsFactors = FALSE))
model2 <- list(formula = 'cancer~I(age< 30) + I(age > 50)', 
               info = data.frame(var = names(coef(m2))[3], 
                                 bet = coef(m2)[3], stringsAsFactors = FALSE))
model3 <- list(formula = 'cancer ~ I(snp == 0)', 
               info = data.frame(var = names(coef(m3))[-1], 
                                 bet = coef(m3)[-1], stringsAsFactors = FALSE))
model <- list(model1, model2, model3)
model2

## -----------------------------------------------------------------------------
nsample <- matrix(N, 3, 3)

## -----------------------------------------------------------------------------
ncase <- matrix(sum(cancer), 3, 3)
nctrl <- matrix(sum(!cancer), 3, 3)

## -----------------------------------------------------------------------------
n <- 300
set.seed(1)
sex <- sample(c('F', 'M'), n, TRUE, c(.4, .6))
snp <- rbinom(n, 2, c(.4, .3, .3))
age <- runif(n, 20, 60)
pr <- plogis(.3 + .5 * I(sex == 'F') + .5 * I(age > 40) - .2 * snp)
cancer <- rbinom(n, 1, pr)
smoking <- sample(c(TRUE, FALSE), n, TRUE, c(.3, .7))
int <- data.frame(cancer, sex, snp, age, smoking, stringsAsFactors = FALSE)

## -----------------------------------------------------------------------------
library(gim)
fit1 <- gim(cancer ~ I(sex == "F") + I(age > 40) + snp + smoking, 
            "case-control", int, model, 
            ncase = ncase, nctrl = nctrl)
summary(fit1)

## -----------------------------------------------------------------------------
fit0 <- glm(cancer ~ I(sex =="F") + I(age>40) +snp + smoking, 
            "binomial", int)
summary(fit0)$coef

## -----------------------------------------------------------------------------
int$sex_F <- ifelse(int$sex == "F", 1, 0)
int$age_gt_40 <- ifelse(int$age > 40, 1, 0)
fit2 <- gim(cancer ~ sex_F + age_gt_40 + snp + smoking, 
            "case-control", int, model, 
            ncase = ncase, nctrl = nctrl)
summary(fit2)

## -----------------------------------------------------------------------------
model[[1]]

## -----------------------------------------------------------------------------
int$dummy_sex <- ifelse(int$sex == "M", 1, 0)
model1 <- list(formula = 'cancer ~ dummy_sex', 
               info = data.frame(var = 'dummy_sex', 
                                 bet = coef(m1)[-1], stringsAsFactors = FALSE))
model1
model <- list(model1, model2, model3)
fit3 <- gim(cancer ~ I(sex == "F") + I(age > 40) + snp + smoking, 
            "case-control", int, model, 
            ncase = ncase, nctrl = nctrl)
summary(fit3)

## -----------------------------------------------------------------------------
coef(fit1)
confint(fit1, level = 0.9)
all(diag(vcov(fit1))^.5 == summary(fit1)$coef[, 2])

