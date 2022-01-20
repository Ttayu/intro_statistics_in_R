# 2標本の平均値間の有意差検定: t検定
# t検定: 2つの標本を想定し，それらの平均値間の差がどのような条件を満たしたら"有意な差"となるかt分布を利用して検出する
# 帰無仮説
# 対立仮説
# 帰無仮説検定: 統計的検定とは帰無仮説を検定するのであって対立仮説を検定するのではない
# 第一種の過誤
# 第二種の過誤
# 統計的検定の検出力(statistical power): 第二種の過誤が起こる確率をβ水準とよび，
# その余事象の確率(1-β)は対立仮説が真であるときに帰無仮説を正しく棄却する確率
# マージナル: 有意確率が5%から10%の間の領域
# 効果量: 差があったときにどの程度の差であるかの指標
# 有意水準α，検出力1-β，効果量，標本サイズの4つの値はそのうち3つを決めると残り1つが自動的に決まる．
# たとえば検出力を0.8, 効果料を実験の背景から決めると，標本サイズが算出できる．
# このような手順で実験計画を立て，実験し，統計解析をするのが理想的である．

# 5.4
New <- c(2, 3, 6, 7, 4, 5, 6, 3)
Old <- c(5, 7, 5, 8, 9, 7, 7, 6)
t.test(New, Old, var = T)

mean_N <- mean(New)
mean_N
mean_O <- mean(Old)
mean_O

SS_N <- sum((New - mean_N)^2)
SS_N
SS_O <- sum((Old - mean_O)^2)
SS_O

New - mean_N
Old - mean_O

sP2 <- (SS_N + SS_O) / (length(New) + length(Old) - 2)
sP2

SE_mean <- sqrt(sP2 * (length(New) + length(Old)) / (length(New) * length(Old)))
SE_mean

t <- abs(mean_N - mean_O) / SE_mean
t

# 5.5
# ばらつきの大小の影響
# 個々のデータのばらつきが大きい場合，有意差が得られない．
# よって帰無仮説H₀は棄却できない．
boys <- c(
  143.1, 140.9, 147.2, 139.8, 141.3, 150.7, 149.4, 145.6,
  146.5, 148.5, 141.2, 136.5, 145.8, 148.1, 144.3
)
girls <- c(
  138.7, 142.8, 150.3, 148.4, 141.7, 149.5, 156.5, 144.6,
  144.4, 145.7, 148.3, 140.8, 146.2, 149.9, 144.1
)
t.test(boys, girls, var = T)

# 平均値は同じでばらつきが小さい場合，有意差が得られる．
# 帰無仮説H₀を棄却して対立仮説H₁を採択する．
boys <- c(
  142.3, 142.5, 145.7, 143.5, 144.2, 145.1, 145.9, 145.2,
  146.8, 145.7, 145.4, 144.6, 144.2, 145.9, 142.1
)
girls <- c(
  143.5, 144.6, 143.4, 146.6, 145.3, 147.7, 147.2, 147.8,
  145.3, 145.7, 147.5, 147.2, 148.8, 147.9, 143.3
)
t.test(boys, girls, var = T)

# 標本サイズの大小の影響
# 有意な差は無い．よって帰無仮説H₀は棄却できない．
g.T <- c(5.5, 4.2, 3.7, 5.1, 4.4, 4.3)
g.C <- c(3.9, 4.1, 3.8, 3.2, 4.5, 3.8)
t.test(g.T, g.C, var = T)

# 検出力を高める最も有効な手立ては標本サイズを大きくすることである．
g.T <- c(5.5, 4.2, 3.7, 5.1, 4.4, 4.3, 5.5, 4.2, 3.7, 5.1, 4.4, 4.3)
g.C <- c(3.9, 4.1, 3.8, 3.2, 4.5, 3.8, 3.9, 4.1, 3.8, 3.2, 4.5, 3.8)
t.test(g.T, g.C, var = T)

# 検出力を高める方法: 対応のあるt検定
# 普通のt検定だとデータのばらつきが大きいので有意な差が出ず，帰無仮説が棄却できない．
d.T <- c(6, 16, 10, 14, 24, 8)
d.B <- c(1, 10, 5, 15, 20, 3)
t.test(d.T, d.B, var = T)

# 対応のあるt検定(paird t-test)で分析すると有意な差が得られる．
# 平均値を検定するとき，2つの標本は正規性・等分散性を前提とする
D <- c(5, 6, 5, -1, 4, 5)
t.test(D, mu = 0)

# 正規性はシャピロ・ウィルク(Shapilo-Wilk)検定
# 正規分布に従う母集団から標本が抽出されたという帰無仮説が棄却されない
# よって正規性は確認できる．
# 有意確率はデータが4つ以上あると近似的に求められる．
boys <- c(
  143.1, 140.9, 147.2, 139.8, 141.3, 150.7, 149.4, 145.6,
  146.5, 148.5, 141.2, 136.5, 145.8, 148.1, 144.3
)
girls <- c(
  138.7, 142.8, 150.3, 148.4, 141.7, 149.5, 156.5, 144.6,
  144.4, 145.7, 148.3, 140.8, 146.2, 149.9, 144.1
)
shapiro.test(boys)
shapiro.test(girls)

# 等分散性の検定: バートレットの等分散検定
# 2群以上の標本を対象に，これらの標本は母分散が
# すべて等しい母集団から抽出したものであると考える帰無仮説を検定する
# 分散は等しいとの帰無仮説は棄却できない
score <- c(boys, girls)
sex <- factor(c(rep(1, 15), rep(2, 15)))

bartlett.test(score ~ sex)

# 2つの標本が等分散でないときの有意差検定: ウェルチの検定
# ばらつきの差異が大きい2つの標本の平均値の有意差検定にt検定は使えない，
# Welchのt検定: おのおのの標本の不偏分散を使って少し違ったt値を計算する
# 2つの標本は正規分布の母集団から抽出されているのを前提
X1 <- c(23, 20, 20, 24, 17, 19, 26, 22, 19, 21)
X2 <- c(17, 23, 25, 34, 25, 28, 20, 31, 26, 36)
var(X1)
var(X2)

# 2つの標本の分散には違いがあることを示している
score <- c(X1, X2)
group <- factor(c(rep(1, 10), rep(2, 10)))
bartlett.test(score ~ group)

t.test(X1, X2)