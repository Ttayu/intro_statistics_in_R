# 7. 多元配置の分散分析と交互作用
# 2つ以上の要因にもとづく多元配置の分散分析と交互作用の検出
# 交互作用: 相乗効果，要因Aと要因Bが同時に効果を及ぼすと，影響が大きく増進され相乗効果が現れること
# 実験計画法: 複数要因ごとに実験を組み立てる研究デザイン
# 要因Aの増減が要因Bの増減にどのように影響するかの交互作用を検出するには実験デザインを組む必要がある．

kaiteki <- c(6, 5, 6, 7, 2, 1, 1, 0, 7, 8, 8, 9, 8, 7, 6, 7)
size <- factor(c(rep("S", 8), rep("L", 8)))
student <- factor(c(rep(1, 4), rep(2, 4), rep(1, 4), rep(2, 4)))
# 二元分散分析のモデル
# size:studentは交互作用(interaction)
summary(aov(kaiteki ~ size + student + size:student))

# 要因配置図(factorial arrangement)
# それぞれ非常に強い主効果が見られる
# 2本の直線が平行から大きくずれているので交互作用も非常に強い効果がある
# 2つの主効果の間で一方の要因がもう片方の要因の影響を左右する
one <- c(mean(kaiteki[1:4]), mean(kaiteki[9:12]))
two <- c(mean(kaiteki[5:8]), mean(kaiteki[13:16]))
plot(
  one,
  xlim = c(0.5, 2.5), ylim = c(0, 10), type = "b",
  xlab = "", ylab = "desirebility", pch = 1, xaxt = "n"
)
points(two, type = "b", pch = 2)
axis(side = 1, at = 1:2, labels = c("small", "large"))

small <- c(mean(kaiteki[1:4]), mean(kaiteki[5:8]))
large <- c(mean(kaiteki[9:12]), mean(kaiteki[13:16]))
plot(
  small,
  xlim = c(0.5, 2.5), ylim = c(0, 10), type = "b",
  xlab = "", ylab = "desirebility", pch = 1, xaxt = "n"
)
points(large, type = "b", pch = 2)
axis(side = 1, at = 1:2, labels = c("one", "two"))

d <- read.csv("../dataset/table7-2.csv")
soil <- factor(d$soil)
ft <- factor(d$ft)
y <- d$plant

summary(aov(y ~ soil + ft + soil:ft))

mean(y[1:10])
mean(y[11:20])
mean(y[21:30])
mean(y[31:40])
mean(y[1:20])
mean(y[21:40])
mean(c(y[1:10], y[21:30]))
mean(c(y[11:20], y[31:40]))

Y1 <- y[1:20]
Y2 <- y[21:40]
Y3 <- c(y[1:10], y[21:30])
Y4 <- c(y[11:20], y[31:40])

Y.mean <- mean(y)
Y.mean

Y.SS <- sum((y - Y.mean)^2)
Y.SS

among.SS <- (
  10 * (mean(y[1:10]) - Y.mean)^2
    + 10 * (mean(y[11:20]) - Y.mean)^2
    + 10 * (mean(y[21:30]) - Y.mean)^2
    + 10 * (mean(y[31:40]) - Y.mean)^2
)
among.SS

soil.SS <- 20 * (mean(Y1) - Y.mean)^2 + 20 * (mean(Y2) - Y.mean)^2
soil.SS

ft.SS <- 20 * (mean(Y3) - Y.mean)^2 + 20 * (mean(Y4) - Y.mean)^2
ft.SS

inter.SS <- among.SS - soil.SS - ft.SS
inter.SS

within.SS <- (
  sum((y[1:10] - mean(y[1:10]))^2)
  + sum((y[11:20] - mean(y[11:20]))^2)
    + sum((y[21:30] - mean(y[21:30]))^2)
    + sum((y[31:40] - mean(y[31:40]))^2)
)
within.SS

# 線形混合モデル: 固定要因とランダム変量要因
d <- read.csv("../dataset/table7-5.csv")
y <- d$wt
treat <- factor(d$treat)
block <- factor(d$block)
# 1は主効果としてのblockによる違い
# 2は主効果としての餌条件処理区の違い
# 3は餌条件と豚舎の交互作用の効果
# 5はWithinと出力されているように残差である個体間ばらつき
summary(aov(y ~ treat + Error(block / treat)))

# 両方とも固定要因からなるANOVAと
# 片方が固定要因でもう片方がランダム変動要因の組み合わせの二元配置混合ANOVAとの計算の仕方が異なる
