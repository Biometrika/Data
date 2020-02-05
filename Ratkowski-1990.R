# Izvor podataka Ratkowski (1990)
Ratkowsky D.A. (1990). Handbook of Nonlinear Regression Models. Marcel Dekker, New York.
Ratkowski D.A. (1993). Principles of nonlinear regression modelling. Journal of Industrial Microbiology 12:195-199.

vreme <- c(9,14,21,28,42,57,63,70,79)
prinos <- c(8.93,10.80,18.59,22.33,39.35,56.11,61.73,64.62,67.08)
df <- data.frame(vreme,prinos)
plot(vreme, prinos)

m1 <- lm(data = df,
         formula = prinos ~ vreme)
summary(m1)
plot(vreme, prinos, mgp = c(1.8,0.6,0), cex.axis = 0.8,
     xlab = "Duzina vegetacije", ylab = "Prinos semena")
abline(m1, col = "tomato", lwd = 2.0)

# kvadratni termin ---
vreme2 <- vreme^2
m2 <- lm(data = df,
         formula = prinos ~ vreme + vreme2)
summary(m2)
vreme0 <- seq(10,80,1)
pred_vreme <- predict(m2, list(vreme = vreme0, vreme2 = vreme0^2))
plot(vreme, prinos, mgp = c(1.8,0.6,0), cex.axis = 0.8,
     xlab = "Duzina vegetacije", ylab = "Prinos semena")
lines(vreme0, pred_vreme, col = "tomato", lwd = 2.0)

# kvadratni i kubni termin ---
vreme2 <- vreme^2
vreme3 <- vreme^3
m3 <- lm(data = df,
         formula = prinos ~ vreme + vreme2 + vreme3)
summary(m3)

m3b <- lm(data = df,
          formula = prinos ~ poly(vreme, 3, raw = TRUE))
summary(m3b)

plot(vreme, prinos, mgp = c(1.8,0.6,0), cex.axis = 0.8,
     xlab = "Duzina vegetacije", ylab = "Prinos semena")
curve(7.8839-0.15728*x+0.03336*x^2-0.00028*x^3,
      add = TRUE, col = "tomato", lwd = 2.0)