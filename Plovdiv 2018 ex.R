
rm(list=ls(all=TRUE))
ls()

df1 <- read.csv("f:/Plovdiv 2018/olives.csv", header = TRUE)
summary(df1)
head(df1)

#--------------------------------------------------------#
# Descriptive parameters
#--------------------------------------------------------#

# Mean
mean(df1$palmitic)

# Minimum value
min(df1$palmitic)

# Maximum value
max(df1$palmitic)

# Trimmed mean (10%)
mean(df1$palmitic, trim = 0.10)

# Range
diff(range(df1$palmitic))

# Median
median(df1$palmitic)

# Mode
library(modeest)
mlv(df1$palmitic, method = "mfv")

# Standard deviation
sd(df1$palmitic)

# Variance
var(df1$palmitic)

# Coefficient of variation (CV)
CV <- sd(df1$palmitic)/mean(df1$palmitic)*100
CV

# Geometric mean
library(psych)
geometric.mean(df1$palmitic)

# Harmonic mean
harmonic.mean(df1$palmitic)

# Quantile
quantile(df1$palmitic)

# Skewness
library(moments)
skewness(df1$palmitic)

# Kurtosis
library(moments)
kurtosis(df1$palmitic)

# Median apsolute deviation (MAD)
mad(df1$palmitic)

# Interquartile range (IQR)
IQR(df1$palmitic)


#--------------------------------------------------------#
# Godness of fit tests
#--------------------------------------------------------#
# Pearson Chi-Square test
library(nortest)
pearson.test(df1$palmitic)

# Cramer-von Mises test
cvm.test(df1$palmitic)

# Shapiro-Francia test
sf.test(df1$palmitic)

# Shapiro-Wilks test
shapiro.test(df1$palmitic)

# Lilliefors test
lillie.test(df1$palmitic)

# Kolmogorov-Smirnov test
ks.test(df1$palmitic, "pnorm")

# Anderson-Darling test
library(nortest)
ad.test(df1$palmitic)

# Jarque Bera test
library(tseries)
jarque.bera.test(df1$palmitic)

# D'Agostino skewness test
library(fBasics)
dagoTest(df1$palmitic)


#--------------------------------------------------------#
# Graphical displays
#--------------------------------------------------------#

# Stem & leaf plot
library(fmsb)
gstem(df1$palmitic, scale = 1)

# Dotplot
stripchart(df1$palmitic,
           method = "jitter",
           xlab = "Concentration of palmitic acid in olives",
           col = "thistle3")

# Histogram
hist(df1$palmitic,
     xlab = "Concentration of palmitic acid in olives",
     ylab = "Count")
rug(df1$palmitic, col = "thistle3")

# Box-plot
# for single variable
boxplot(df1$palmitic,
        ylab = "Concentration of palmitic acid in olives",
        col = "thistle3")

# with groupping variable
boxplot(df1$palmitic ~ df1$Region,
        xlab = "Region in Italy",
        ylab = "Concentration of palmitic acid in olives",
        col = c("sienna1","steelblue3","yellow1")
        )

# Q-Q plot
qqnorm(df1$palmitic,
       main = "Concentration of palmitic acid in olives",
       col = "thistle3")
qqline(df1$palmitic)


#--------------------------------------------------------#
# Independent and dependent t-test
#--------------------------------------------------------#

# Independent t-test
# Feeding of cows with soybean meal
cows <- c("1","1","1","1","1","1","1","1","1","1",
          "2","2","2","2","2","2","2","2","2","2")
meal <- c(1180,1044,777,1092,1295,1279,1233,1226,1326,1332,
          903,1262,1321,1170,1384,1073,1233,1247,1405,1168)
df2 <- data.frame(cows,meal)
t.test(meal ~ cows, var.equal = TRUE, data = df2) # equal variances
t.test(meal ~ cows, var.equal = FALSE, data = df2) # unequal variances

# Box-plot
boxplot(meal ~ cows,
        ylab = "Cow weight (kg)",
        xlab = "Group of cows",
        col = c("sienna1","steelblue3")
        )

# Dependent t-test
# trees1 - apple yield with N fertilizer
# trees2 - apple yield without N fertilizer
trees1 <- c(13,12,10,6,13,15,19,10,11,11,9,14,12,12,13)
trees2 <- c(11,6,3,1,7,10,9,4,3,6,5,7,6,4,8)
df3 <- data.frame(trees1,trees2)
t.test(trees1, trees2, paired = TRUE, data = df3)


#--------------------------------------------------------#
# Linear correlation
#--------------------------------------------------------#

library(psych)

# Pearson correlation
corr.test(df1[2:7], use = "pairwise", method = "pearson", adjust = "holm", alpha = 0.05)
corr.test(df1[2:7], use = "pairwise", method = "pearson", adjust = "holm", alpha = 0.01)

# Spearman correlation
corr.test(df1[2:7], use = "pairwise", method = "spearman", adjust = "holm", alpha = 0.05)
corr.test(df1[2:7], use = "pairwise", method = "spearman", adjust = "holm", alpha = 0.01)

# Visual display

# Scatterplot
pairs(df1[2:7])

# Heat-maps

mat.pearson <-cor(df1[2:7], method = "pearson")
cor.plot(mat.pearson, numbers = TRUE, colors = TRUE)

mat.spearman <-cor(df1[2:7], method = "spearman")
cor.plot(mat.spearman, numbers = TRUE, colors = TRUE)


#--------------------------------------------------------#
# Linear regresion
#--------------------------------------------------------#

# Wheat dataset

# akw - apsolute kernel weight
# nks - number of kernels per spike
# yield - grain yield
akw <- c(25.0,29.2,32.5,28.5,24.8,28.3,35.3,27.0,26.8,29.9,42.2,32.2,29.7,33.4,39.0,30.9,30.7,31.1,40.6,32.4,27.7,31.4,39.0,28.1,30.3,33.9,44.1,32.3,28.0,29.5,28.4,31.2,25.5,27.6,26.0,26.0)
nks <- c(28.3,22.8,33.4,27.7,49.3,55.3,58.4,55.0,39.0,47.1,43.8,46.0,43.8,45.4,48.7,46.0,45.2,51.2,56.2,47.0,51.9,54.9,60.4,52.8,32.2,35.5,44.2,38.9,23.7,30.6,33.4,29.2,38.8,45.6,39.1,46.2)
yield <- c(22.1,23.7,34.9,21.3,34.9,53.2,53.3,41.5,35.0,52.9,57.3,38.7,46.3,55.8,63.1,48.3,39.8,58.5,57.6,40.8,35.7,56.3,54.9,39.3,44.0,53.4,54.2,42.2,30.0,37.9,38.4,33.8,23.9,27.2,45.9,30.5)
wheat <- data.frame(akw,nks,yield)

m1 <- lm(yield ~ akw, data = wheat)
summary(m1)
plot(yield ~ akw,
     xlab = "X = Apsolute kernel weight",
     ylab = "Y = Grain yield")
abline(m1, col = "red", lwd = 2.0)
text(41, 23, expression(italic(R)^2==0.46), cex = 2.0)

# Some diagnostic plots
par(mfrow = c(2,2))
plot(m1, which = 1)
plot(m1, which = 2)
plot(m1, which = 3)
plot(m1, which = 4)

# Normality of residuals
shapiro.test(residuals(m1))

# Heteroskedasticity in a linear regression by Breusch-Pagan test
# Whether the variance of the errors from a regression is
# dependent on the values of the independent variables
library(car)
ncvTest(m1, data = df)

# Influential opservations

# DFFITs
round(dffits(m1), 2)

# DFBETAs
round(dfbetas(m1), 2)

# Comparison with model without influential opservations
library(car)
m1b <- update(m1, subset = -c(1,15,27,35))
compareCoefs(m1, m1b)



#--------------------------------------------------------#
# One-way ANOVA
#--------------------------------------------------------#

# Nitrogen concentration in potato roots
treatment <- c("N0","N0","N0","N0","N1","N1","N1","N1","N2","N2","N2","N2","N3","N3","N3","N3","N4","N4","N4","N4")
nitrogen <- c(105,115,91,141,135,131,145,175,147,143,153,157,148,161,161,164,132,149,155,164)
potato <- data.frame(treatment,nitrogen)

# Treatment as factor variable
potato$trt <- as.factor(potato$treatment)

# ANOVA
m2 <- aov(nitrogen ~ trt, data = potato)
summary(m2)

# Treatment values
library(effects)
effect("trt", m2)

# Diagnostic plots

# Ordinary residuals

plot(resid(m2),
     xlab = "Opservation",
     ylab = "Ordinary residuals")
abline(h = 0, lty = 3, col = "red3")

# Q-Q plot of ordinary residuals
qqnorm(resid(m2), col = "red3")
qqline(resid(m2))

# Tretman vs. nitrogen concentration (quadratic response)
stripchart(potato$nitrogen ~ potato$trt,
           method = "stack",
           vertical = TRUE,
           xlab = "Tretman",
           ylab = "Nitrogen concentration (mg/kg)")

# Homogeneity of variances

# Bartlet test
bartlett.test(potato$nitrogen ~ potato$trt, data = potato)

# Leven test
library(car)
leveneTest(nitrogen, potato$trt, center = "mean")

# Multiple comparison tests
library(agricolae)

# LSD test (with Bonferroni correction)
LSD.trt.bonf <- LSD.test(m2, "trt", p.adj = "bonferroni")
LSD.trt.bonf$statistics
LSD.trt.bonf$parameters
LSD.trt.bonf$groups

# Graphical display
bar.group(LSD.trt.bonf$groups,
          ylim = c(0,200),
          density = 10,
          border = "black",
          xlab = "Treatment",
          ylab = "Nitrogen concentration (mg/kg)",
          main = "LSD test")

# Tukey test
HSD.trt <- TukeyHSD(m2, "trt")
HSD.trt

# Plot with CIs
plot(HSD.trt, las = 1)

# Duncan test
duncan.trt <- duncan.test(m2, "trt")
duncan.trt$statistics
duncan.trt$parameters
duncan.trt$groups

# Graphical display
bar.group(duncan.trt$groups,
          ylim = c(0,200),
          density = 10,
          border = "black",
          xlab = "Treatment",
          ylab = "Nitrogen concentration (mg/kg)",
          main = "Duncan test")

# Dunett test
library(multcomp)
dunnett.trt <- glht(m2, linfct = mcp(trt = "Dunnett"))
confint(dunnett.trt)
plot(dunnett.trt)

