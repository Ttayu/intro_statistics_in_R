a <- 10
x <- (5 + 3) * 2 - a / 5
print(x)

d <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
mean(d)
var(d)
sd(d)

library(assertthat)
a <- sum(d) / length(d)
assert_that(a == mean(d))
b <- sum((d - a)^2) / (length(d) - 1)
assert_that(b == var(d))
c <- sqrt(b)
assert_that(c == sd(d))
