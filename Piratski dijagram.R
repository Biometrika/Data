
library(yarrr)

df <- read.csv("d:/Biometrika/masline.csv", header = TRUE)
head(df,3)
names(df)
attach(df)

pirateplot(formula = palmitinska ~ oblast,
           data = df,
           theme = 1,
           main = "theme = 1")

pirateplot(formula = palmitinska ~ oblast,
           data = df,
           theme = 2,
           main = "theme = 2")

pirateplot(formula = palmitinska ~ oblast,
           data = df,
           theme = 3,
           main = "theme = 3")

pirateplot(formula = palmitinska ~ oblast,
           data = df,
           theme = 4,
           main = "theme = 4")

# palete boja ---
piratepal("all")

pirateplot(formula = palmitinska ~ oblast,
           data = df,
           pal = "espresso", 
           theme = 1,
           main = "pony color palette")

pirateplot(formula = palmitinska ~ oblast,
           data = df,
           theme = 2,
           pal = "black",
           main = "pal = 'black")

