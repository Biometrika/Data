x <- seq(-4.5,4.5,0.01)
kriva <- dnorm(x)
df <- c(1,5,20)

boja <- c("gold","tomato","yellowgreen","black")
naziv <- c("df=1","df=10","df=20","normalna")

plot(x, kriva, type = "l", lty = 1, xlab = "X", ylab = "p", mgp = c(1.8,0.6,0), cex.axis = 0.85)

for (i in 1:3){
  lines(x, dt(x, df[i]), lwd = 2, col = boja[i])
}

legend(-4.6, 0.403, title = "Raspodela", cex = 0.85, naziv, lwd = 2, lty = c(1,1,1,1), col = boja)