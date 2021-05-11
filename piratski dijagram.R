# piratski dijagram ---

library(pdfCluster)
library(yarrr)
data(oliveoil)
dt <- oliveoil
head(dt)

pirateplot(formula = oleic ~ macro.area,
           data = dt,
           theme = 1,
           main = "theme = 1")

pirateplot(formula = oleic ~ macro.area,
           data = dt,
           theme = 2,
           main = "theme = 2")

pirateplot(formula = oleic ~ macro.area,
           data = dt,
           theme = 3,
           main = "theme = 3")

pirateplot(formula = oleic ~ macro.area,
           data = dt,
           theme = 4,
           main = "theme = 4")

# palete boja ---
piratepal("all")

pirateplot(formula = oleic ~ macro.area,
           data = dt,
           pal = "espresso", 
           theme = 1,
           main = "pony color palette")

pirateplot(formula = oleic ~ macro.area,
           data = dt,
           theme = 2,
           pal = "black",
           main = "pal = black")