# 3. 大数の法則，正規分布，中心極限定理
# 大数の法則; 標本サイズが大きくなるほど，その標本の平均は母集団平均に近づく
# 中心極限定理: 母集団分布がどのような分布であっても，標本を抽出したときの平均は正規分布に収束する．
# 確率密度関数: 確率変数が連続であるとき，そのとりうる値での相対的な起こりやすさ(値は確率ではない)を示す関数
#   常に非負であり，とりうる値全体を積分すると1となる．
# Zスコア: 正規分布に従うとき，スコアを平均値からの差を標準偏差単位で表したもの．
# 累積確率分布: f(x)を-∞からある位置(q)までの範囲まで積分したF(q)の分布

# 正規分布
# 標準偏差分のズレでの累積確率は平均値も標準偏差も異なる２つの正規分布に関わらず共通の累積確率である．
xb1 <- pnorm(12, mean = 10, sd = 2) - pnorm(10, mean = 10, sd = 2)
xb1
xc1 <- pnorm(24, mean = 20, sd = 4) - pnorm(20, mean = 20, sd = 4)
xc1
assertthat::assert_that(xb1 == xc1)

xb2 <- pnorm(14, mean = 10, sd = 2) - pnorm(10, mean = 10, sd = 2)
xb2
xc2 <- pnorm(28, mean = 20, sd = 4) - pnorm(20, mean = 20, sd = 4)
xc2
assertthat::assert_that(xb2 == xc2)

# 中心極限定理のモンテカルロ・シミュレーション
n <- 1000
m <- 100000
mean.d <- numeric(m)
for (k in 1:m) {
  d <- runif(n, 0, 10)
  mean.d[k] <- mean(d)
}
hist(mean.d, breaks = 50)
qqnorm(mean.d)
