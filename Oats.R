library(asreml)
data(oats)
head(oats,3)

#   Blocks Nitrogen Subplots    Variety Wplots yield
# 1      1  0.6_cwt        1 Marvellous      1   156
# 2      1  0.4_cwt        2 Marvellous      1   118
# 3      1  0.2_cwt        3 Marvellous      1   140

m1 <- asreml(fixed = yield ~ Variety*Nitrogen,
             random = ~ Blocks/Wplots,
             data = oats)
summary(m1)$varcomp
#                              gamma component std.error  z.ratio constraint
# Blocks!Blocks.var        1.2111647  214.4771 168.83404 1.270343   Positive
# Blocks:Wplots!Blocks.var 0.5989373  106.0618  67.87553 1.562593   Positive
# R!variance               1.0000000  177.0833  37.33244 4.743416   Positive

wald(m1)
# Wald tests for fixed effects
# Response: yield
# Terms added sequentially; adjusted for those above
#                  Df Sum of Sq Wald statistic Pr(Chisq)    
# (Intercept)       1     43410        245.141    <2e-16 ***
# Variety           2       526          2.971    0.2264    
# Nitrogen          3     20021        113.057    <2e-16 ***
# Variety:Nitrogen  6       322          1.817    0.9357    
# residual (MS)             177                             
# ---
# Signif. codes:  0  ***  0.001  **  0.01  *  0.05  .  0.1     1

predict(m1, classify = "Variety", maxiter = 1)$predictions
# $pvals
# Notes:
# - The predictions are obtained by averaging across the hypertable
#   calculated from model terms constructed solely from factors in
#   the averaging and classify sets.
# - Use "average" to move ignored factors into the averaging set.
# - The SIMPLE averaging set:  Nitrogen 
# - The ignored set:  Blocks Wplots 
#       Variety predicted.value standard.error est.status
# 1 Golden_rain        104.5000       7.797539  Estimable
# 2  Marvellous        109.7917       7.797539  Estimable
# 3     Victory         97.6250       7.797539  Estimable
# $avsed
#  overall 
# 7.078904

predict(m1, classify = "Nitrogen", maxiter = 1)$predictions
# $pvals
# Notes:
# - The predictions are obtained by averaging across the hypertable
#   calculated from model terms constructed solely from factors in
#   the averaging and classify sets.
# - Use "average" to move ignored factors into the averaging set.
# - The SIMPLE averaging set:  Variety 
# - The ignored set:  Blocks Wplots 
#   Nitrogen predicted.value standard.error est.status
# 1    0_cwt        79.38889        7.17471  Estimable
# 2  0.2_cwt        98.88889        7.17471  Estimable
# 3  0.4_cwt       114.22222        7.17471  Estimable
# 4  0.6_cwt       123.38889        7.17471  Estimable
# $avsed
#  overall 
# 4.435755

predict(m1, classify = "Variety:Nitrogen", maxiter = 1)$predictions
# $pvals
# Notes:
# - The predictions are obtained by averaging across the hypertable
#   calculated from model terms constructed solely from factors in
#   the averaging and classify sets.
# - Use "average" to move ignored factors into the averaging set.
# - The ignored set:  Blocks Wplots 
#        Variety Nitrogen predicted.value standard.error est.status
# 1  Golden_rain    0_cwt        80.00000       9.106977  Estimable
# 2  Golden_rain  0.2_cwt        98.50000       9.106977  Estimable
# 3  Golden_rain  0.4_cwt       114.66667       9.106977  Estimable
# 4  Golden_rain  0.6_cwt       124.83333       9.106977  Estimable
# 5   Marvellous    0_cwt        86.66667       9.106977  Estimable
# 6   Marvellous  0.2_cwt       108.50000       9.106977  Estimable
# 7   Marvellous  0.4_cwt       117.16667       9.106977  Estimable
# 8   Marvellous  0.6_cwt       126.83333       9.106977  Estimable
# 9      Victory    0_cwt        71.50000       9.106977  Estimable
# 10     Victory  0.2_cwt        89.66667       9.106977  Estimable
# 11     Victory  0.4_cwt       110.83333       9.106977  Estimable
# 12     Victory  0.6_cwt       118.50000       9.106977  Estimable
# $avsed
#  overall 
# 9.205419