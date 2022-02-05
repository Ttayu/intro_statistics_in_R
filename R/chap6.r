# 6. 一元配置の分散分析と多重比較
# 3標本以上の仮説検定を正しく行う分散分析(ANOVA: analysis of variance)
# 一元配置の分散分析: 一つの要因で3つの条件を設定して分析する．
# 3標本などでt検定を繰り返すのは誤り．
# 全体偏差: 群間偏差と郡内偏差の和
# 群間偏差: 群iの平均(X̄ᵢ)から総平均(X̄)の偏差
# 郡内偏差: 群i内の各データ(Xᵢⱼ)から群iの平均(X̄ᵢ)との偏差
# 平均平方: 群間平方和と郡内平方和をおのおのの自由度で割ることで得られる2つの分散の推定値
# Fisher値: 群間分散 / 郡内偏差

d1 <- c(
  28.2, 33.2, 36.4, 34.6, 29.1, 31.0,
  39.6, 40.8, 37.9, 37.1, 43.6, 42.4,
  46.3, 42.1, 43.5, 48.8, 43.7, 40.1,
  41.0, 44.1, 46.4, 40.2, 38.6, 36.3,
  56.3, 54.1, 59.4, 62.7, 60.0, 57.3
)
lake <- c(rep(1, 6), rep(2, 6), rep(3, 6), rep(4, 6), rep(5, 6))
lake <- factor(lake)
summary(aov(d1 ~ lake))

# データ入力からANOVAのモデルを指定
d2 <- read.csv("../dataset/table6-3.csv")
lake <- factor(d2$lake)
summary(aov(d2$stron ~ lake))

m <- 5
n <- 6
SS.w <- 0
SS.b <- 0
for (i in 1:m) {
  SS.w <- SS.w + sum((d1[(n * (i - 1) + 1):(n * i)] - mean(d1[(n * (i - 1) + 1):(n * i)]))^2)
  SS.b <- SS.b + sum(n * mean(d1[(n * (i - 1) + 1):(n * i)] - mean(d1))^2)
}
SS.w
SS.b

MS.b <- SS.b / (m - 1)
MS.w <- SS.w / (m * (n - 1))
MS.b
MS.w

F <- MS.b / MS.w
F

1 - pf(F, 4, 25)

d <- read.csv("../dataset/table6-4.csv")
d

y <- d$Pig
feed <- factor(d$feed)
summary(aov(y ~ feed))

# TukeyのHSD検定 (honestly significant difference test)
# 多重比較法の代表的な検定法
result <- aov(d1 ~ lake)
TukeyHSD(result)

# 多重比較: 3群以上の平均値が得られているときに，どの平均値間に差があるかを検定すること
# 検定の多重性: 例えばt検定を3回実施すると有意水準が0.05よりも増加してしまうこと
# 事前比較(a priori test): ある理論的な予測によって，ANOVAを実施せずに多重比較のデザインを前もって決めておくこと
# 事後比較(post hoc test): 一元配置ANOVAを行った後の結果を見て群間比較のデザインを考案すること
# 各群の分散は等しくなければいけない
# Tukey-Kramerの方法: 群間ですべての対比較(pair-wise comparison)を同時に検定する
# Dunnettの方法: 1つの対照群と2つ以上の処理群の対比較のみを同時に検定する
# Schefféの方法: 対比(contrast)で表現されるすべての帰無仮説を同時に検定する
# Williamsの方法: 1つの対照群と2つ以上の処理群があり，各群に順序付けがある場合に適用する
# 母平均に単調増加(単調減少)を想定できるとき対照群と処理群のみを同時に検定する．検出力が高い
# 対比較ではTukey-Kramerを使うほうが良い

# Tukey-Kramer
d <- read.csv("../dataset/table6-7.csv")
d
group <- factor(d$group)
summary(aov(d$score ~ group))
result <- aov(d$score ~ group)
result
TukeyHSD(result)

# Dunnett
install.packages("multcomp")

library(multcomp)
d <- read.csv("../dataset/table6-8.csv")
group <- factor(d$group)
res.1 <- aov(d$score ~ group)
# スチューデント化された1検定統計量
res.2 <- glht(res.1, linfct = mcp(group = "Dunnett"))
summary(res.2)

# シーケンシャル・ボンフェローニの方法(ホルムの方法)
# 上記の4つの多重比較法は共通して誤差自由度dfₑと誤差分散Vₑを求め，
# 検定統計量を計算して棄却値の表からしかるべき値を見つける方法であった
# ボンフェローニの方法は，群間の対比較でt検定を繰り返すが，その代わりに対比較の有意水準αを帰無仮説の個数に応じて一定率で調節する方法
d <- read.csv("../dataset/table6-7.csv")
pairwise.t.test(d$score, d$group, p.adj = "holm")

d <- read.csv("../dataset/table6-9.csv")
pairwise.t.test(d$score, d$group, p.adj = "holm")
