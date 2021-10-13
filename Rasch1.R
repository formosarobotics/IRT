icc1 <- function(b,D,color) {
  theta <- seq(-3,3,.1)
  P1 <- 1/(1+exp(-D*(theta-b)))
  plot(theta, P1, type="l", xlim=c(-3,3), ylim=c(0,1),
       xlab="Ability", ylab="Prob(correct response)",
       col = color)
}

icc2 <- function(b,D,color) {
  theta <- seq(-3,3,.1)
  P1 <- 1/(1+exp(-D*(theta-b)))
  lines(theta, P1, col = color)
}

D <- 1.7
icc1(b=-1, D, "red")
icc2(b=0, D, "green")
icc2(b=1, D, "blue")

