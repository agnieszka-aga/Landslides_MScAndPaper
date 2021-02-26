#### Code for correlation matrix

library(corrplot)

DataSet <- read.delim("D:/.../DataSet_Correlation.txt")

# View(DataSet)

M <- cor(DataSet, use="everything", method=c("pearson"))

# View(M)

corrplot(M, method="color", addCoef.col="black", type=c("upper"), diag=FALSE, tl.cex=0.4, tl.col="black", number.cex=0.3, cl.cex=0.6)

