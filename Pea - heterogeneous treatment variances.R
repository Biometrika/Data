
library(asreml)
library(asremlPlus)
library(ggplot2)

df <- data.frame(
  sugar = c("1","1","1","1","1","1","1","1","1","1","2","2","2","2","2","2","2","2","2","2","3","3","3","3","3","3","3","3","3","3","4","4","4","4","4","4","4","4","4","4","5","5","5","5","5","5","5","5","5","5"),
  length = c(75,67,70,75,65,71,67,67,76,68,57,58,60,59,62,60,60,57,59,61,58,61,56,58,57,56,61,60,57,58,58,59,58,61,57,56,58,57,57,59,62,66,65,63,64,62,65,65,62,67)
  )

df$trt <- as.factor(df$sugar)

# Box-plot ---
ggplot(df, aes(trt, length)) + 
  geom_boxplot() +
  geom_jitter(shape = 1, position = position_jitter(0.25)) +
  xlab("Treatment") +
  ylab("Lenght (mm)")


# Model with homogeneous treatment variances ---
m0 <- asreml(fixed = length ~ trt,
             data = df)
summary(m0)$varcomp
#            gamma component std.error  z.ratio constraint
# R!variance     1  5.455556  1.150132 4.743416   Positive

wald(m0, denDF = "default", ssType = "conditional")
# $Wald
#             Df denDF    F.inc    F.con Margin           Pr
# (Intercept)  1    45 35160.00 35160.00        9.360931e-67
# trt          4    45    49.37    49.37      A 6.737436e-16
# $stratumVariances


# Predictions for m0 --
lenght.m0.pvs <- predict(m0, classify = "trt", maxiter = 1, trace = TRUE, sed = TRUE) 
lenght.m0.pvs$predictions

# $pvals
# Notes:
# - The predictions are obtained by averaging across the hypertable
#   calculated from model terms constructed solely from factors in
#   the averaging and classify sets.
# - Use "average" to move ignored factors into the averaging set.

#   trt predicted.value standard.error est.status
# 1   1            70.1      0.7386173  Estimable
# 2   2            59.3      0.7386173  Estimable
# 3   3            58.2      0.7386173  Estimable
# 4   4            58.0      0.7386173  Estimable
# 5   5            64.1      0.7386173  Estimable

# $sed
#          [,1]     [,2]     [,3]     [,4]     [,5]
# [1,] 0.000000 1.044563 1.044563 1.044563 1.044563
# [2,] 1.044563 0.000000 1.044563 1.044563 1.044563
# [3,] 1.044563 1.044563 0.000000 1.044563 1.044563
# [4,] 1.044563 1.044563 1.044563 0.000000 1.044563
# [5,] 1.044563 1.044563 1.044563 1.044563 0.000000

# $avsed
#      min     mean      max 
# 1.044563 1.044563 1.044563


# Model with heterogeneous treatment variances ---
m1 <- asreml(fixed = length ~ trt,
             rcov = ~ at(trt):units,
             data = df)
summary(m1)$varcomp
#                    gamma component std.error z.ratio constraint
# trt_1!variance 15.877778 15.877778  7.484856 2.12132   Positive
# trt_2!variance  2.677778  2.677778  1.262317 2.12132   Positive
# trt_3!variance  3.511111  3.511111  1.655154 2.12132   Positive
# trt_4!variance  2.000000  2.000000  0.942809 2.12132   Positive
# trt_5!variance  3.211111  3.211111  1.513732 2.12132   Positive

wald(m1, denDF = "default", ssType = "conditional")
# $Wald
#             Df denDF    F.inc    F.con Margin           Pr
# (Intercept)  1  37.0 55340.00 55340.00        2.439746e-60
# trt          4  19.9    33.61    33.61      A 1.354244e-08
# $stratumVariances

# Predictions for m1 --
lenght.m1.pvs <- predict(m1, classify = "trt", maxiter = 1, trace = TRUE, sed = TRUE) 
lenght.m1.pvs$predictions
# $pvals

# Notes:
# - The predictions are obtained by averaging across the hypertable
#   calculated from model terms constructed solely from factors in
#   the averaging and classify sets.
# - Use "average" to move ignored factors into the averaging set.

#   trt predicted.value standard.error est.status
# 1   1            70.1      1.2600705  Estimable
# 2   2            59.3      0.5174725  Estimable
# 3   3            58.2      0.5925463  Estimable
# 4   4            58.0      0.4472136  Estimable
# 5   5            64.1      0.5666667  Estimable

# $sed
#          [,1]      [,2]      [,3]      [,4]      [,5]
# [1,] 0.000000 1.3621878 1.3924399 1.3370781 1.3816255
# [2,] 1.362188 0.0000000 0.7866949 0.6839428 0.7673910
# [3,] 1.392440 0.7866949 0.0000000 0.7423686 0.8198916
# [4,] 1.337078 0.6839428 0.7423686 0.0000000 0.7218803
# [5,] 1.381625 0.7673910 0.8198916 0.7218803 0.0000000

# $avsed
#       min      mean       max 
# 0.6839428 0.9995500 1.3924399


# Model comparison --
info.crit.asreml(m0)
#   DF      AIC      BIC   logREML
# 1  1 134.8615 136.6681 -66.43074

info.crit.asreml(m1)
#   DF      AIC      BIC   logREML
# 1  5 128.3034 137.3367 -59.15168

reml.lrt.asreml(m1, m0, positive.zero = TRUE)
#    REMLLRT DF           p
# 1 14.55811  4 0.001208614


