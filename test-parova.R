# test parova ---
# izvor podataka:
# Steel and Torrie (1980). Principles and Procedures of Statistics: A Biometrical Approach

library(reshape2)
library(ggpubr)
library(asreml)
library(nlme)
library(lucid)

id <- c("1","2","3","4","5","6","7","8","9","10")
T4 <- c(62.5,65.2,67.6,69.9,69.4,70.1,67.8,67.0,68.5,62.4)
T9 <- c(51.7,54.2,53.3,57.0,56.4,61.5,57.2,56.2,58.2,55.8)
dt <- data.frame(id,T4,T9)
head(dt, 3)

dt.melt <- melt(dt,
                id.vars = "id",
                variable.name = "trt",
                value.name = "koncentracija")
head(dt.melt, 3)

dt.melt$id <- as.factor(dt.melt$id)
dt.melt$trt <- as.factor(dt.melt$trt)

# t-test ---
t.test(dt$T4, dt$T9,
       paired = TRUE,
       alternative = "two.sided",
       var.equal = TRUE)

# alternativni nacin ---
razlika <- dt$T4 - dt$T9
t.test(razlika, alternative = "two.sided")

# vizuelizacija ---
ggpaired(dt.melt,
         x = "trt",
         y = "koncentracija",
         id = "id",
         point.size = 1,
         xlab = "Tretman",
         ylab = "Koncentracija secera u nektaru")

# nlme ---
m1 <- lme(fixed = koncentracija ~ trt,
          random = ~ 1 | id,
          data = dt.melt)
summary(m1)
VarCorr(m1)
anova(m1)
coef(m1)

# asreml ---
m2 <- asreml(fixed = koncentracija ~ trt,
             random = ~ id,
             data = dt.melt)
vc(m2)
#   effect component std.error z.ratio bound %ch
#       id     5.229     3.088     1.7     P 0.3
#  units!R     2.46      1.161     2.1     P 0

wald(m2)
# Wald tests for fixed effects.
# Response: koncentracija
# Terms added sequentially; adjusted for those above.
#
#               Df Sum of Sq Wald statistic Pr(Chisq)    
# (Intercept)    1   14489.6         5889.1 < 2.2e-16 ***
# trt            1     593.0          241.0 < 2.2e-16 ***
# residual (MS)          2.5                             
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

predict(m2, classify = "trt", sed = TRUE, trace = FALSE)
#   trt predicted.value std.error    status
# 1  T4           67.04 0.8763212 Estimable
# 2  T9           56.15 0.8763212 Estimable
#
# $sed
# 2 x 2 Matrix of class "dspMatrix"
#           [,1]      [,2]
# [1,]        NA 0.7010231
# [2,] 0.7010231        NA
#
# $avsed
#       min      mean       max 
# 0.7010231 0.7010231 0.7010231