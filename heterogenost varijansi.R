# problem heterogenosti varijansi tretmana ---

library(asreml)
library(asremlPlus)
library(ggplot2)

dt <- data.frame(Treatment = c(rep("Control", 10),
                               rep("Glucose", 10),
                               rep("Fructose", 10),
                               rep("GlucFruc", 10),
                               rep("Sucrose", 10)),
                 Length = c(75,67,70,75,65,71,67,67,76,68,
                            57,58,60,59,62,60,60,57,59,61,
                            58,61,56,58,57,56,61,60,57,58,
                            58,59,58,61,57,56,58,57,57,59,
                            62,66,65,63,64,62,65,65,62,67))

dt$Treatment <- as.factor(dt$Treatment)
dt$Treatment <- factor(dt$Treatment, levels = c("Control","Glucose","Fructose","GlucFruc","Sucrose"), ordered = TRUE)

# boks dijagram ---
ggplot(data = dt,
       aes(x = Treatment,
           y = Length)) + 
  geom_jitter(shape = 15,
              size = 2,
              color = "tomato",
              position = position_jitter(0.1)) +
  labs(x = "Treatment",
       y = "Lenght (mm)") +
  theme_light()

# model sa homogenim varijansama treatmana ---
model1 <- asreml(fixed = Length ~ Treatment,
                 data = dt)
summary(model1)$varcomp
wald(model1, denDF = "default", ssType = "conditional")
plot(model1)

# predikcija za model1 ---
predict(model1, classify = "Treatment", trace = FALSE, sed = FALSE) 

# uporedjenje razlika parova predikcija ---
model1.diff <- predictPlus(classify = "Treatment",
                           asreml.obj = model1,
                           error.intervals = "halfLeast",
                           wald.tab = model1$wald.tab,
                           sortFactor = "Treatment",
                           tables = "predictions")
model1.diff$predictions
model1.diff$differences
model1.diff$p.differences
model1.diff$sed
model1.diff$LSD

# vizuelizacija ---
plotPredictions(model1.diff$predictions,
                classify = "Treatment",
                y = "predicted.value",
                error.intervals = "half")
plotPvalues(model1.diff)


# model sa heterogenim varijansama tretmana ---
model2 <- asreml(fixed = Length ~ Treatment,
                 residual = ~ dsum(~ id(units) | Treatment),
                 data = dt)
summary(model2)$varcomp
wald(model2, denDF = "default", ssType = "conditional")
plot(model2)

# predikcija za model2 ---
predict(model2, classify = "Treatment", trace = FALSE, sed = TRUE)

# uporedjenje razlika parova predikcija ---
model2.diff <- predictPlus(classify = "Treatment",
                           asreml.obj = model2,
                           error.intervals = "halfLeast",
                           wald.tab = model2$wald.tab,
                           sortFactor = "Treatment",
                           tables = "predictions")
model2.diff$predictions
model2.diff$differences
model2.diff$p.differences
model2.diff$sed
model2.diff$LSD

# vizuelizacija ---
plotPredictions(model2.diff$predictions,
                classify = "Treatment",
                y = "predicted.value",
                error.intervals = "half")
plotPvalues(model2.diff)

# uporedjenje modela --
infoCriteria(model1)
infoCriteria(model2)
lrt.asreml(model2, model1, boundary = TRUE)