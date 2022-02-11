# 10. 一般線形モデル(GLM)
# 一般化線形モデル (GLM): 回帰モデルの誤差分布を正規分布だけでなく
# 二項分布やポアソン分布,負の二項分布なども含めて一般化して扱うモデル
# 応答変数: 現象の結果と想定されるデータ
# 説明変数: 原因と想定されるデータ
# GLM: f(y) = β₀ + β₁x₁ + β₂x₂ + … + ϵ
# 左辺は応答変数のリンク関数，有縁は線形予測子
x <- seq(0, 15, 1)
freq <- dpois(x, lambda = 5)
plot(x, freq, type = "b", lwd = 2)

d <- read.csv("../dataset/table10-2.csv")
result <- glm(cbind(d$dead, 1 - d$dead) ~ d$dose, family = binomial(logit))
summary(result)

plot(d$dose, d$dead, xlab = "dose", ylab = "mortality", xlim = c(1.0, 6.0))
pred.dose <- seq(1.0, 6.0, 0.01)
pred.y <- 1 / (
  1 + exp(-(result$coefficient[1]
  + result$coefficient[2] * pred.dose)))
lines(pred.dose, pred.y)
# z value: Wald統計量，パラメータの最尤推定値をSE(標準誤差)で割った値であり
# 推定値がゼロから十分に離れているかの目安になる
d <- read.csv("../dataset/table10-3.csv")
result <- glm(d$flw ~ d$wt, family = poisson)
summary(result)
plot(d$wt, d$flw,
  xlab = "bulb weight (g)", ylab = "No. of flowers",
  xlim = c(20, 40), ylim = c(0, 10)
)
x.wt <- seq(20, 40, 0.1)
y.flw <- exp(result$coefficient[1] + result$coefficient[2] * x.wt)
lines(x.wt, y.flw, xlim = c(20, 40), ylim = c(0, 10))

# 尤度: 尤もらしさを定量的に示すもの．
y <- 0:9
p <- dpois(y, lambda = 2)
options(digits = 4)
p
# 対数尤度: 尤度関数は総積を繰り返すためとても扱いにくいので対数変換することで総和に変換することができる．
# 最尤推定: 最大対数尤度をとるパラメータを求め，パラメータの最適な推定値とする．

set.seed(123)
d <- rpois(50, lambda = 3)
logL <- function(m) sum(dpois(d, m, log = T))
lambda <- seq(2, 5, 0.1)
plot(lambda, sapply(lambda, logL), ylab = "logL", type = "l")

# 赤池の情報量基準(AIC): 最大対数尤度と最尤推定した自由パラメータのバランスで決まる値
# いくつか候補となるモデルの出力するAICを比較して相対的に小さい値を示すモデルが良い
logLik(result)
AIC <- -2 * logLik(result) + 2 * 2
AIC

# 逸脱度: AICの右辺の D = -2 * 最大対数尤度，データの当てはまりの悪さ
# ナルモデル: 説明変数の有意な傾きが全く無い，切片だけのモデル
# 最小逸脱度: フルモデルを当てはめたときの逸脱度
# 残差逸脱度: D - 最小逸脱度
# 最大逸脱度: ナルモデルを当てはめたときの逸脱度
# ナル逸脱度: 最大逸脱度 - 最小逸脱度

d <- read.csv("../dataset/table10-3.csv")
sum(log(dpois(d$flw, lambda = d$flw)))
result <- glm(d$flw ~ 1, family = poisson)
summary(result)
logLik(result)

# 尤度比検定: 観察されたデータに対し求めた2つのモデルの逸脱度の差(ΔD)が帰無仮説が正しいと仮定
# つまり，パラメータが切片だけのモデルからデータをサンプリングしたときにどの程度起こりやすいのかを求め，
# 起こりにくい(P < α)のであれば帰無仮説を棄却する．

# 2つ以上の説明変数をもつときのモデル選択
d <- read.csv("../dataset/table10-5.csv")
result <- glm(cbind(d$y, 1 - d$y) ~ d$dose + d$sex, family = binomial(logit))
summary(result)
plot(d$dose, d$y,
  xlab = "dose", ylab = "mortality",
  xlim = c(0, 4), pch = c(21, 19)[d$sex]
)
pred.dose <- seq(1, 4, 0.01)
pred.ym <- 1 / (1 + exp(
  -(
    result$coefficient[1]
    + result$coefficient[2] * pred.dose
      + result$coefficient[3] * 1)
))
pred.yf <- 1 / (1 + exp(
  -(
    result$coefficient[1]
    + result$coefficient[2] * pred.dose
      + result$coefficient[3] * 2)
))
lines(pred.dose, pred.ym, lwd = 2, col = "gray")
lines(pred.dose, pred.yf, lwd = 2, col = "black")
legend("topleft", legend = c("M", "F"), pch = c(21, 19))

# AICは平均的な予測が良いモデルを相対的に選ぶものであり，
# AIC最小のモデルが新のモデルとどれだけ確実に一致しているか
# あるいは他のモデルが確実に否定されるかを述べているわけではない

# GLMにおけるオフセットの利用
d <- read.csv("../dataset/table10-6.csv")
result <- glm(d$plants ~ d$light, offset = log(d$w_area), family = poisson)
summary(result)
plot(d$plants / d$w_area ~ d$light,
  xlab = "light (lx)", ylab = "no. of plants per m^2",
  xlim = c(0, 4000), ylim = c(0, 0.6)
)
pred.light <- seq(0, 4000, 0.1)
pred.y <- exp(result$coefficient[1] + result$coefficient[2] * pred.light)
lines(pred.light, pred.y, lwd = 2, col = "gray")
