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
wald(m0, denDF = "default", ssType = "conditional")

predikcija za m0 ---
predict(m0, classify = "trt", trace = FALSE, sed = FALSE) 
$pvals

# uporedjenje razlika parova predikcija ---
m0.diff <- predictPlus(classify = "trt",
                       asreml.obj = m0,
                       error.intervals = "halfLeast",
                       wald.tab = m0$wald.tab,
                       sortFactor = "trt",
                       tables = "predictions")
m0.diff$predictions
m0.diff$differences
m0.diff$p.differences
m0.diff$sed
m0.diff$LSD

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
wald(m1, denDF = "default", ssType = "conditional")

# predikcija za m1 ---
predict(m1, classify = "trt", trace = FALSE, sed = FALSE)

# uporedjenje razlika parova predikcija ---
m1.diff <- predictPlus(classify = "trt",
                       asreml.obj = m1,
                       error.intervals = "halfLeast",
                       wald.tab = m1$wald.tab,
                       sortFactor = "trt",
                       tables = "predictions")
m1.diff$predictions
m1.diff$differences
m1.diff$p.differences
m1.diff$sed
m1.diff$LSD

# vizuelizacija ---
plotPredictions(m1.diff$predictions,
                classify = "trt",
                y = "predicted.value",
                error.intervals = "half")
plotPvalues(m1.diff)

# uporedjenje modela --
infoCriteria(m0)
infoCriteria(m1)
lrt.asreml(m1, m0, boundary = TRUE)