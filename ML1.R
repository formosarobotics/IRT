ability <- function(X, b) {
  D <- 1.7
  D2 <- D * D
  N <- length(b)
  MaxCnt <- 50
  theta <- 1.
  Tiny <- 0.001
  for (i in 1:MaxCnt) {
    numerator  <- 0
    denominator <- 0
    for (n in 1:N) {
      p <- 1/(1+exp(-D*(theta-b[n])))
      numerator  <- numerator  + D * (X[n] - p)
      denominator <- denominator - D2 * p * (1-p)
    }
    diff <- numerator / denominator
    theta <- theta - diff
    cat(paste("[", i, "] ability =", theta, "  diff=", diff, "\n")); flush.console()
    if (abs(diff) < Tiny) break
  }
}


b <- c(0.5, -0.5, 0.7, 0.6, -1, 0.9, 0, -0.5, 0) # difficulty level
X <- c(1, 0, 1, 1, 0, 1, 0, 1, 0) # individual responses
# #individuals M = 1
ability(X,b)
