
# sigma pravilo normalne raspodele ---

set.seed(12345)
x <- rnorm(10)
mean(x)
sd(x)

plot(seq(-3.2, 3.2, length = 50),
     dnorm(seq(-3, 3, length = 50), 0, 1),
     type = "l",
     ylim = c(0, 0.5),
     lwd = 1.5,
     cex.axis = 0.85,
     mgp = c(1.8,0.6,0),
     xlab = "",
     ylab = "")
segments(x0 = c(-3,3),
         y0 = c(-1,-1),
         x1 = c(-3,3),
         y1 = c(1,1),
         lty = 3)
text(x = 0,
     y = 0.45,
     labels = expression("99.7% u okviru 3" ~ sigma),
     col = "red3")
arrows(x0 = c(-2,2),
       y0 = c(0.45, 0.45),
       x1 = c(-3,3),
       y1 = c(0.45, 0.45),
       length = 0.15,
       angle = 12)
segments(x0 = c(-2,2),
         y0 = c(-1,-1),
         x1 = c(-2,2),
         y1 = c(0.4,0.4),
         lty = 3)
text(x = 0,
     y = 0.3,
     labels = expression("95% u okviru 2" ~ sigma),
     col = "red3")
arrows(x0 = c(-1.5, 1.5),
       y0 = c(0.3, 0.3),
       x1 = c(-2, 2),
       y1 = c(0.3, 0.3),
       length = 0.15,
       angle = 12)
segments(x0 = c(-1, 1),
         y0 = c(-1, -1),
         x1 = c(-1, 1),
         y1 = c(0.25, 0.25),
         lty = 3)
text(x = 0,
     y = 0.15,
     labels = expression("68% u okviru 1" * sigma),
     col = "red3")
