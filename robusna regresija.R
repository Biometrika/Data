# robusna regresija ---

library(foreign)
library(MASS)
library(robust)
library(robustbase)

dt <- read.csv("https://raw.githubusercontent.com/Biometrika/Data/master/robXY.csv", header = TRUE)
head(dt, 3)

# reziduali OLS modela ---
res <- resid(m1 <- lm(y ~ x, data = dt))
summary(m1)

# vizuelizacija reziduala ---
n <- nrow(dt)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(m1, las = 1)
d <- cooks.distance(m1)
r <- stdres(m1)
a <- cbind(dt, d, r)
a[d > 4/n, ]

# najveci reziduali ---
rabs <- abs(r)
a <- cbind(dt, d, r, rabs)
asorted <- a[order(-rabs), ]
asorted[1:15, ]

# robusna regresija (iterated re-weighted least squares-IRLS) ---
summary(rr.huber <- rlm(y ~ x, data = dt))
hweights <- data.frame(dt, resid = rr.huber$resid, weight = rr.huber$w)
hweights2 <- hweights[order(rr.huber$w), ]
hweights2[1:15, ]

# bisquare weighting funkcija ---
rr.bisquare <- rlm(y ~ x, data = dt, psi = psi.bisquare)
summary(rr.bisquare)
biweights <- data.frame(dt, resid = rr.bisquare$resid, weight = rr.bisquare$w)
biweights2 <- biweights[order(rr.bisquare$w), ]
biweights2[1:15, ]

# graficki prikaz ---
plot(dt, cex = 1)
abline(m1, col = "black", lwd = 1.2)
abline(rr.huber, col = "red", lwd = 1.2)
abline(rr.bisquare, col = "blue", lwd = 1.2)

# MM-regresija Yohai (1987) ---
stack.rob <- lmRob(y ~ x, data = dt)
summary(stack.rob)

# robusna korelacija ---
covRob(dt, estim = "mcd", cor = TRUE)
covRob(dt, estim = "M", cor = TRUE)
covRob(dt, estim = "donostah", cor = TRUE)
covRob(dt, estim = "weighted", cor = TRUE)

# Pearson i Spearman korelacija ---
cor(dt, method = "pearson")
cor(dt, method = "spearman")

# MCD ---
cov.rob(dt, method = "mcd", cor = TRUE)