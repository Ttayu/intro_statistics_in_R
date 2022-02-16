# 一般化線形混合モデル(GLMM)と過分散対応
# 少なからずデータセットは，想定している二項分布やポアソン分布などの理論分布に従わず
# 過大分散や過小分散になっていることがある．

d <- read.csv("../dataset/table11-1.csv")
plot(d$y ~ d$x, pch = as.character(d$block))

library(lme4)
res.1 <- glmer(d$y ~ d$x + (1 | d$block), family = gaussian(link = identity))
res.2 <- lmer(d$y ~ d$x + (1 | d$block))
summary(res.1)
summary(res.2)

library(lme4)
library(glmmML)
d <- read.csv("../dataset/table11-2.csv")
res.1 <- glmer(d$y ~ d$x + (1 | d$ID), family = binomial(logit))
res.2 <- glmmML(d$y ~ d$x, cluster = d$ID, family = binomial(logit))
res.3 <- summary(res.1)

plot(jitter(d$y, 0.1) ~ jitter(d$x, 0.1), xlab = "x", ylab = "y", xlim = c(0, 5))
pred.x <- seq(0, 5, 0.01)
pred.y1 <- 1 / (
  1 + exp(-(res.3$coefficient[1] + res.3$coefficient[2] * pred.x))
)
pred.y2 <- 1 / (
  1 + exp(-(res.2$coefficient[1] + res.2$coefficient[2] * pred.x))
)
lines(pred.x, pred.y1)
lines(pred.x, pred.y2, col = "gray", lty = 2)
glmer(d$y ~ d$x + (1 | d$ID), family = binomial(logit))
glmmML(d$y ~ d$x, cluster = d$ID, family = binomial(logit))
summary(res.1)
summary(res.2)

d <- read.csv("../dataset/table11-3.csv")
res.1 <- glmer(cbind(d$y, 1 - d$y) ~ d$wt + (1 | d$mother), family = binomial(logit))
res.2 <- glmmML(cbind(d$y, 1 - d$y) ~ d$wt, cluster = d$mother, family = binomial(logit))
res.3 <- summary(res.1)

plot(jitter(d$y, 0.04) ~ jitter(d$wt, 0.2),
  xlab = "offspring wt(mg)", ylab = "sex ratio (male)", xlim = c(0, 0.6)
)
pred.wt <- seq(0, 0.6, 0.01)
pred.y1 <- 1 / (
  1 + exp(-(res.3$coefficient[1] + res.3$coefficient[2] * pred.wt))
)
pred.y2 <- 1 / (
  1 + exp(-(res.2$coefficient[1] + res.2$coefficient[2] * pred.wt))
)
lines(pred.wt, pred.y1)
lines(pred.wt, pred.y2, col = "gray", lty = 2)

summary(res.1)
summary(res.2)

# 二項分布からずれている
# 単純な二項分布と比べて，過分散が生じている
matrix(c(d$y), nrow = 4, ncol = 12)

d <- read.csv("../dataset/table11-3.csv")
res.3 <- glm(cbind(d$y, 1 - d$y) ~ d$wt, family = binomial(logit))
summary(res.3)

# 過分散: データが平均から期待されるよりもずっと大きな分散を持つ現象
# カウントデータの場合の過分散の対処法
# (1) ポアソン分布からのずれが過分散として生じている場合は，
# 負の二項分布など期待値とは独立に分散を自由に調整できる分布で回帰することで
# 過分散のデータにも対処できる
# (2) ランダム変量要因のブロック構造として，グループ・組・調査区・個体差
# などの効果を，一般線形混合モデルとして組み込む(GLMM)
# (3) 関数glm()では誤差構造として疑似尤度が使える.
# familyでquasibinomialやquasipoissonを指定すると疑似尤度が使われ，
# それぞれ二項分布やポアソン分布で，分散が平均に依存して変化する場合に有効
d <- read.csv("../dataset/table11-4.csv")
result <- glm(d$flw ~ d$wt, family = poisson)
plot(d$wt, d$flw,
  xlab = "bulb(g)", ylab = "No. of flowers",
  xlim = c(20, 60), ylim = c(0, 16)
)
x.wt <- seq(20, 60, 0.1)
y.flw <- exp(result$coefficient[1] + result$coefficient[2] * x.wt)
lines(x.wt, y.flw, xlim = c(20, 60), ylim = c(0, 16), lwd = 2)
summary(result)
# (Residual deviance) / (degrees of freedom) = 71.071 / 34 = 2.09なので
# 過分散の目安となる1.5を大きく超えているので過分散となっている．
# つまりポアソン回帰の適用は不適切．
library(MASS)
d <- read.csv("../dataset/table11-4.csv")
result <- glm(d$flw ~ d$wt, family = negative.binomial(1))
summary(result)
# 過分散は解消されているが，負の二項回帰のほうが曲率が強い
# 負の二項分布を実行するとき，パラメータθをどの程度の値に決めるかが1つの課題となり
# パラメータθを最尤推定する．
result <- glm.nb(d$flw ~ d$wt)
summary(result)

d <- read.csv("../dataset/table11-5.csv")
res.1 <- glm(d$y ~ d$x, family = quasipoisson)
plot(d$y ~ d$x, xlim = c(1, 7), ylim = c(0, 50))
pred.x <- seq(0, 7, 0.01)
pred.y1 <- exp((res.1$coefficient[1] + res.1$coefficient[2] * pred.x))
lines(pred.x, pred.y1, lwd = 2)
summary(res.1)
logLik(res.1)

library(MASS)
d <- read.csv("../dataset/table11-5.csv")
res.2 <- glm(d$y ~ d$x, family = negative.binomial(1))
res.3 <- glm.nb(d$y ~ d$x)
summary(res.2)
summary(res.3)
