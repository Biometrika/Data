
library(plotrix)

Dim1 <- c(0.16,0.39,0.46,-0.45,0.08,0.08,-0.37,0.03,-0.29,0.23)
Dim2 <- c(0.15,-0.10,-0.30,-0.39,-0.12,-0.42,0.14,0.49,-0.03,-0.50)
Prop1 <- c(0.25,0.72,0.46,0.82,0.15,0.64,0.09,0.42,0.34,0.52)
Prop2 <- c(0.75,0.28,0.54,0.18,0.85,0.36,0.91,0.58,0.66,0.48)
df <- data.frame(Dim1,Dim2,Prop1,Prop2)

n <- length(Dim1)
plot(-0.5:0.5, -0.5:0.5,
     xlab = "Dim 1", ylab = "Dim 2",
     type = "n", cex.axis = 0.75,
     mgp = c(1.8,0.6,0))
for (i in 1:n){floating.pie(Dim1[i], Dim2[i], c(Prop1[i], Prop2[i]),
               radius = 0.03, col = c("tomato","gold")
               )
     }
