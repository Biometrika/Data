# problem heterogenosti varijansi tretmana ---

library(asreml)
library(asremlPlus)
library(ggplot2)

dt <- data.frame(
  sugar = c("1","1","1","1","1","1","1","1","1","1","2","2","2","2","2","2","2","2","2","2","3","3","3","3","3","3","3","3","3","3","4","4","4","4","4","4","4","4","4","4","5","5","5","5","5","5","5","5","5","5"),
  length = c(75,67,70,75,65,71,67,67,76,68,57,58,60,59,62,60,60,57,59,61,58,61,56,58,57,56,61,60,57,58,58,59,58,61,57,56,58,57,57,59,62,66,65,63,64,62,65,65,62,67)
  )

dt$trt <- as.factor(dt$sugar)

# boks dijagram ---
ggplot(data = dt,
       aes(x = trt,
           y = length)) + 
  geom_boxplot(fill = NA) +
  geom_jitter(shape = 1,
              position = position_jitter(0.1)) +
  labs(x = "Treatment",
       y = "Lenght (mm)")

# model sa homogenim varijansama treatmana ---
m0 <- asreml(fixed = length ~ trt,
             data = dt)
summary(m0)$varcomp
#         component std.error  z.ratio bound %ch
# units!R  5.455556  1.150132 4.743416     P   0

wald(m0, denDF = "default", ssType = "conditional")
#             Df denDF F.inc F.con Margin         Pr
# (Intercept)  1    45 35160 35160        0.0000e+00
# trt          4    45    49    49      A 6.7374e-16

# predikcija za m0 ---
# predict(m0, classify = "trt", trace = FALSE, sed = FALSE) 
# $pvals

# Notes:
# - The predictions are obtained by averaging across the hypertable
#   calculated from model terms constructed solely from factors in
#   the averaging and classify sets.
# - Use 'average' to move ignored factors into the averaging set.

#   trt predicted.value std.error    status
# 1   1            70.1 0.7386173 Estimable
# 2   2            59.3 0.7386173 Estimable
# 3   3            58.2 0.7386173 Estimable
# 4   4            58.0 0.7386173 Estimable
# 5   5            64.1 0.7386173 Estimable

# $avsed
#  overall 
# 1.044563

# uporedjenje razlika parova predikcija ---
m0.diff <- predictPlus(classify = "trt",
                       asreml.obj = m0,
                       error.intervals = "halfLeast",
                       wald.tab = m0$wald.tab,
                       sortFactor = "trt",
                       tables = "predictions")
m0.diff$predictions
#   trt predicted.value standard.error upper.halfLeastSignificant.limit
# 4   4            58.0      0.7386173                         59.05193
# 3   3            58.2      0.7386173                         59.25193
# 2   2            59.3      0.7386173                         60.35193
# 5   5            64.1      0.7386173                         65.15193
# 1   1            70.1      0.7386173                         71.15193
#   lower.halfLeastSignificant.limit est.status
# 4                         56.94807  Estimable
# 3                         57.14807  Estimable
# 2                         58.24807  Estimable
# 5                         63.04807  Estimable
# 1                         69.04807  Estimable

m0.diff$differences
#      4    3    2    5     1
# 4  0.0 -0.2 -1.3 -6.1 -12.1
# 3  0.2  0.0 -1.1 -5.9 -11.9
# 2  1.3  1.1  0.0 -4.8 -10.8
# 5  6.1  5.9  4.8  0.0  -6.0
# 1 12.1 11.9 10.8  6.0   0.0

m0.diff$p.differences
#              4            3            2            5            1
# 4           NA 8.490205e-01 2.197439e-01 5.396515e-07 4.272717e-15
# 3 8.490205e-01           NA 2.979297e-01 1.035836e-06 7.504852e-15
# 2 2.197439e-01 2.979297e-01           NA 3.493239e-05 1.809754e-13
# 5 5.396515e-07 1.035836e-06 3.493239e-05           NA 7.478652e-07
# 1 4.272717e-15 7.504852e-15 1.809754e-13 7.478652e-07           NA

m0.diff$sed
#          4        3        2        5        1
# 4       NA 1.044563 1.044563 1.044563 1.044563
# 3 1.044563       NA 1.044563 1.044563 1.044563
# 2 1.044563 1.044563       NA 1.044563 1.044563
# 5 1.044563 1.044563 1.044563       NA 1.044563
# 1 1.044563 1.044563 1.044563 1.044563       NA

m0.diff$LSD
#          minLSD  meanLSD   maxLSD
# minLSD 2.103857 2.103857 2.103857

# vizuelizacija ---
plotPredictions(m0.diff$predictions,
                classify = "trt",
                y = "predicted.value",
                error.intervals = "half")
plotPvalues(m0.diff)


# model sa heterogenim varijansama tretmana ---
m1 <- asreml(fixed = length ~ trt,
             residual = ~ dsum(~ units|trt),
             data = dt)
summary(m1)$varcomp
#         component std.error  z.ratio bound %ch
# trt_1!R 15.877778 7.4843814 2.121455     P   0
# trt_2!R  2.677778 1.2622365 2.121455     P   0
# trt_3!R  3.511111 1.6550487 2.121455     P   0
# trt_4!R  2.000000 0.9427492 2.121455     P   0
# trt_5!R  3.211111 1.5136363 2.121455     P   0

wald(m1, denDF = "default", ssType = "conditional")
#             Df denDF F.inc F.con Margin          Pr
# (Intercept)  1  37.0 55340 55340        0.00000e+00
# trt          4  19.9    34    34      A 1.35424e-08

# predikcija za m1 ---
predict(m1, classify = "trt", trace = FALSE, sed = FALSE)
# $pvals

# Notes:
# - The predictions are obtained by averaging across the hypertable
#   calculated from model terms constructed solely from factors in
#   the averaging and classify sets.
# - Use 'average' to move ignored factors into the averaging set.

#   trt predicted.value std.error    status
# 1   1            70.1 1.2600705 Estimable
# 2   2            59.3 0.5174725 Estimable
# 3   3            58.2 0.5925463 Estimable
# 4   4            58.0 0.4472136 Estimable
# 5   5            64.1 0.5666667 Estimable

# $avsed
#  overall 
# 1.044563

# uporedjenje razlika parova predikcija ---
m1.diff <- predictPlus(classify = "trt",
                       asreml.obj = m1,
                       error.intervals = "halfLeast",
                       wald.tab = m0$wald.tab,
                       sortFactor = "trt",
                       tables = "predictions")
m1.diff$predictions
#   trt predicted.value standard.error upper.Confidence.limit
# 4   4            58.0      0.4472136               58.90073
# 3   3            58.2      0.5925463               59.39345
# 2   2            59.3      0.5174725               60.34224
# 5   5            64.1      0.5666667               65.24133
# 1   1            70.1      1.2600705               72.63791
#   lower.Confidence.limit est.status
# 4               57.09927  Estimable
# 3               57.00655  Estimable
# 2               58.25776  Estimable
# 5               62.95867  Estimable
# 1               67.56209  Estimable

m1.diff$differences
#      4    3    2    5     1
# 4  0.0 -0.2 -1.3 -6.1 -12.1
# 3  0.2  0.0 -1.1 -5.9 -11.9
# 2  1.3  1.1  0.0 -4.8 -10.8
# 5  6.1  5.9  4.8  0.0  -6.0
# 1 12.1 11.9 10.8  6.0   0.0

m1.diff$p.differences
#              4            3            2            5            1
# 4           NA 7.888473e-01 6.375234e-02 7.696699e-11 1.078585e-11
# 3 7.888473e-01           NA 1.688903e-01 5.221289e-09 5.604456e-11
# 2 6.375234e-02 1.688903e-01           NA 1.305002e-07 4.382350e-10
# 5 7.696699e-11 5.221289e-09 1.305002e-07           NA 7.900960e-05
# 1 1.078585e-11 5.604456e-11 4.382350e-10 7.900960e-05           NA

m1.diff$sed
#           4         3         2         5        1
# 4        NA 0.7423686 0.6839428 0.7218803 1.337078
# 3 0.7423686        NA 0.7866949 0.8198916 1.392440
# 2 0.6839428 0.7866949        NA 0.7673910 1.362188
# 5 0.7218803 0.8198916 0.7673910        NA 1.381625
# 1 1.3370781 1.3924399 1.3621878 1.3816255       NA

m1.diff$LSD
#          minLSD  meanLSD   maxLSD
# minLSD 1.377532 2.103857 2.804518

# vizuelizacija ---
plotPredictions(m1.diff$predictions,
                classify = "trt",
                y = "predicted.value",
                error.intervals = "half")
plotPvalues(m1.diff)

# uporedjenje modela --
infoCriteria(m0)
#   fixedDF varDF NBound      AIC      BIC    loglik
# 1       0     1      0 134.8615 136.6681 -66.43074

infoCriteria(m1)
#   fixedDF varDF NBound      AIC      BIC    loglik
# 1       0     5      0 128.3034 137.3367 -59.15168

lrt.asreml(m1, m0, boundary = TRUE)
# Likelihood ratio test(s) assuming nested random models.
# (See Self & Liang, 1987)

#       df LR-statistic Pr(Chisq)   
# m1/m0  4       14.558  0.001209 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1