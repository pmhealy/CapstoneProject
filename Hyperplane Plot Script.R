### Linearly Inseparable

 DF1 <- data.frame(
A=c(2, 3, 3, 4, 4.25, 4.5),
B=c(2, 2, 4, 3, 2, 3),
C=c(1, 1, 2, 2, 1, 2)
)

### Linearly Separable

 DF2 <- data.frame(
A=c(2, 3, 3, 4, 1.5, 4.5),
B=c(2, 2, 4, 3, 3.5, 3),
C=c(1, 1, 2, 2, 1, 2)
)

par(mfrow=c(1,2))

plot(B ~ A,
      xlab = "",
      ylab = "",
      pch = c(16, 17),  # different 'pch' types 
      main="Fig 1.1: Linearly Inseparable",
      col = c("red","blue"),
      data = DF1, cex=1.5, xlim=c(1, 5), ylim=c(1, 5))


plot(B ~ A,
      xlab = "",
      ylab = "",
      pch = c(16, 17),  # different 'pch' types 
      main="Fig 1.2: Linearly Separable",
      col = c("red","blue"),
      data = DF2, cex=1.5, xlim=c(1, 5), ylim=c(1, 5))
	abline(a=-0.6, b=1,col="black")

