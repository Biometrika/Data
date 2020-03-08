x <- seq(-4.5,4.5,0.01)
normal.curve <- dnorm(x)
df <- c(1,5,20)

colors <- c("gold","tomato","yellowgreen","black")
labels <- c("df=1","df=10","df=20","normalna")

plot(x, normal.curve, type = "l", lty = 1, xlab = "X", ylab = "p", mgp = c(1.8,0.6,0), cex.axis = 0.85)

for (i in 1:3){
  lines(x, dt(x, df[i]), lwd = 2, col = colors[i])
}

legend(-4.6, 0.403, title = "Raspodela", cex = 0.85, labels, lwd = 2, lty = c(1,1,1,1), col = colors)