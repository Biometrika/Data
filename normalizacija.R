# normalizacija promenljivih u tabeli ---

# max ---

normalizeMax <- function(dt) {
  for (col in names(dt)) {
    if (is.numeric(dt[[col]])) {
      maxVal <- max(dt[[col]])
      dt[[col]] <- dt[[col]] / maxVal
    }
  }
  
  return(dt)
}

iris2 <- iris[1:4]
iris2_normMax <- normalizeMax(iris2)
head(iris2_normMax, 10)


# min ---

normalizeMin <- function(dt) {
  for (col in names(dt)) {
    if (is.numeric(dt[[col]])) {
      minVal <- min(dt[[col]])
      dt[[col]] <- dt[[col]] / minVal
    }
  }
  
  return(dt)
}

iris2_normMin <- normalizeMin(iris2)
head(iris2_normMin, 10)

# sepal length ---
par(mfrow = c(1,2))
qqnorm(iris2_normMax$Sepal.Length, col = "#277BC0", pch = 16, frame = FALSE)
qqline(iris2_normMax$Sepal.Length, lwd = 1.5, col = "#A19882")
qqnorm(iris2_normMin$Sepal.Length, col = "#FFCB42", pch = 16, frame = FALSE)
qqline(iris2_normMin$Sepal.Length, lwd = 1.5, col = "#A19882")
summary(iris2_normMax$Sepal.Length)
summary(iris2_normMin$Sepal.Length)

# sepal width ---
par(mfrow = c(1,2))
qqnorm(iris2_normMax$Sepal.Width, col = "#277BC0", pch = 16, frame = FALSE)
qqline(iris2_normMax$Sepal.Width, lwd = 1.5, col = "#A19882")
qqnorm(iris2_normMin$Sepal.Width, col = "#FFCB42", pch = 16, frame = FALSE)
qqline(iris2_normMin$Sepal.Width, lwd = 1.5, col = "#A19882")
summary(iris2_normMax$Sepal.Width)
summary(iris2_normMin$Sepal.Width)

# petal length ---
par(mfrow = c(1,2))
qqnorm(iris2_normMax$Petal.Length, col = "#277BC0", pch = 16, frame = FALSE)
qqline(iris2_normMax$Petal.Length, lwd = 1.5, col = "#A19882")
qqnorm(iris2_normMin$Petal.Length, col = "#FFCB42", pch = 16, frame = FALSE)
qqline(iris2_normMin$Petal.Length, lwd = 1.5, col = "#A19882")
summary(iris2_normMax$Petal.Length)
summary(iris2_normMin$Petal.Length)

# petal width ---
par(mfrow = c(1,2))
qqnorm(iris2_normMax$Petal.Width, col = "#277BC0", pch = 16, frame = FALSE)
qqline(iris2_normMax$Petal.Width, lwd = 1.5, col = "#A19882")
qqnorm(iris2_normMin$Petal.Width, col = "#FFCB42", pch = 16, frame = FALSE)
qqline(iris2_normMin$Petal.Width, lwd = 1.5, col = "#A19882")
summary(iris2_normMax$Petal.Width)
summary(iris2_normMin$Petal.Width)