## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(cofad)

## -----------------------------------------------------------------------------
d <- data.frame(empathy = c(51, 56, 61, 58, 54, 62, 67, 57, 65, 59, 50, 49, 47, 45,
                            44, 50, 45, 40, 49, 41),
                major = as.factor(
                  rep(c("psychology", "education", "business",
                              "chemistry"), each = 5)))
head(d)

## -----------------------------------------------------------------------------
ca <- calc_contrast(dv = empathy, between = major,
                    lambda_between = c("psychology" = 1, "education" = -1,
                                       "business" = 0, "chemistry" = 0),
                    data = d)
ca

## -----------------------------------------------------------------------------
summary(ca)

## -----------------------------------------------------------------------------
lambdas <- rep(c(1, -1, 0, 0), each = 5)
cor(d$empathy, lambdas)

## -----------------------------------------------------------------------------
ca <- calc_contrast(dv = empathy, between = major,
                    lambda_between = c("psychology" = 0, "education" = 0,
                                       "business" = 1, "chemistry" = -1),
                    data = d)
ca
ca <- calc_contrast(dv = empathy, between = major,
                    lambda_between = c("psychology" = 1, "education" = 1,
                                       "business" = -1, "chemistry" = -1),
                    data = d)
ca

## -----------------------------------------------------------------------------
ca <- calc_contrast(dv = empathy, between = major,
                    lambda_between = c("psychology" = 73, "education" = 61,
                                       "business" = 51, "chemistry" = 38),
                    data = d)
ca

## -----------------------------------------------------------------------------
lambdas <- rep(c(73, 61, 51, 38), each = 5)
cor(d$empathy, lambdas)

## -----------------------------------------------------------------------------
d <- data.frame(reading_test = c(27, 25, 30, 29, 30, 33, 31, 35,
                                 25, 26, 32, 29, 28, 30, 32, 34,
                                 21, 25, 23, 26, 27, 26, 29, 31, 
                                 23, 24, 24, 28, 24, 26, 27, 32),
                participant = as.factor(rep(1:8, 4)),
                music = as.factor(rep(c("without music", "white noise", "classic", "jazz"), each = 8)))
head(d)
calc_contrast(dv = reading_test, within = music,
              lambda_within = c("without music" = 1.25, 
                                "white noise" = 0.25, "classic" = -0.75, "jazz" = -0.75),
             ID = participant, data = d)

## -----------------------------------------------------------------------------
mtr <- matrix(d$reading_test, ncol = 4)
lambdas <- c(1.25, 0.25, -0.75, -0.75)
lc1 <- mtr %*% lambdas
t.test(lc1)

## -----------------------------------------------------------------------------
tab53 <- data.frame(
    Var = c(3, 1, 4, 4, 5, 5, 6, 5, 7, 2, 2, 5,
            5, 6, 7, 6, 6, 8, 3, 1, 5, 4, 5, 6,
            7, 6, 8, 3, 2, 5, 6, 6, 7, 8, 8, 9),
    age = as.factor(
      rep(rep(c("Age 8", "Age 10", "Age 12"), c(3, 3, 3)), 4)
      ),
    time = as.factor(rep(1:4, c(9, 9, 9, 9))),
    ID = as.factor(rep(1:9, 4 ))
    )
head(tab53)
lambda_within <- c("1" = -3, "2" = -1, "3" = 1, "4" = 3)
lambda_between <-c("Age 8" = -1, "Age 10" = 0, "Age 12" = 1)

contr_mx <- calc_contrast(dv = Var, 
                          between = age,
                          lambda_between = lambda_between,
                          within = time,
                          lambda_within = lambda_within,
                          ID = ID, 
                          data = tab53
                          )
contr_mx

## -----------------------------------------------------------------------------
summary(contr_mx)

