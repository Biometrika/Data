# kompozicioni podaci ---

library(psych)

# vektor kompozicije ---
y <- c(0.50,0.35,0.15)
sum(y)

# geometrijska srednja vrednost ---
exp(mean(log(y)))
geometric.mean(y)

# clr ---
log(y[1] / exp(mean(log(y))))
log(y[2] / exp(mean(log(y))))
log(y[3] / exp(mean(log(y))))