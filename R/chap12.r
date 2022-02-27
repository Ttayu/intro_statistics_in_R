# 12. ノンパラメトリック検定(1): 観測度数の利用
# 分割表や分類区分(カテゴリー)に分けられたデータを観測度数(と期待度数)や順位としてつかう方法
# パラメトリック検定
# - 母集団のデータが特定の分布に従う
# - 要約統計量がデータの分布状態を反映する
# - 母集団や標本が正規分布かつ等分散性が保証されているときは結果の精度が高い
# - 一般化しやすい．ある分布をもつ母集団からのひょうを仮定しているので結果の一般化が容易
# - 等分散条件や正規分布条件を満たしているかで束縛される
# ノンパラメトリック検定
# - 帰無仮説を設定するときに2つ以上の標本が同一母集団から抽出されているならば
# その母集団がどんな分布をもつかは問わない方法も適用できる．
# - 要約統計量が中央値や順位平均の場合は，データの分布状態を反映せず，順位だけに注目する．
# - 要約統計量が度数のときは，観測度数や期待度数をつかって検定統計量を計算できる．

binom.test(c(13, 7), p = 1 / 2)
binom.test(c(65, 35), p = 1 / 2)

# 適合度検定: 観測度数がある原理の帰無仮説から得られたかをX²値を用いて検定するもの．
d <- c(152, 39, 53, 6)
chisq.test(d, p = c(9, 3, 3, 1), rescale = T)
pchisq(8.9724, df = 3, lower.tail = F)
qchisq(0.95, df = 3)

# 正確χ²検定: モンテカルロシミュレーションを用いた適合度検定
egg <- c(
  rep(0, 0), rep(1, 0), rep(2, 60), rep(3, 101),
  rep(4, 84), rep(5, 48), rep(6, 7), rep(7, 0), rep(8, 0)
)
r <- hist(egg,
  br = seq(-0.5, 9.5, 1), main = "Histogram of egg distribution",
  xlab = "eggs per bean", col = "gray"
)
x <- c(0:9)
pois <- length(egg) * dpois(x, mean(egg))
lines(r$mids, pois)

result.table <- rbind(r$counts, pois)
rownames(result.table) <- c("observed", "expected")
colnames(result.table) <- r$mids
result.table[, 8] <- result.table[, 8] + result.table[, 9] + result.table[, 10]
table2 <- result.table[, 1:8]
obs <- as.vector(table2["observed", ])
exp <- as.vector(table2["expected", ])
chisq.test(obs, p = exp, rescale = T)
pchisq(105.8636, df = 6, lower.tail = F)

chisq.test(obs, p = exp, rescale = T, simulate.p.value = T, B = 10000)

# χ²独立性検定
x <- matrix(c(25, 15, 10, 30), ncol = 2, nrow = 2, byrow = T)
chisq.test(x)

x <- matrix(c(25, 15, 10, 30), ncol = 2, nrow = 2, byrow = T)
chisq.test(x, correct = F)

# χ²独立性検定 vs 分割表の対数尤度比検定 (G検定)
# G値は標本サイズが十分に大きいときにはχ²分布に従うため，
# G値をそのままχ²独立性検定と比較することができる.
hair <- matrix(c(32, 43, 16, 9, 55, 65, 64, 16), nrow = 2, ncol = 4, byrow = T)
chisq.test(hair)

library(Deducer)
likelihood.test(hair)

# 標本サイズが大きいときはχ²値はG値よりも早く収束するので有利
# 分割表の各セルの平均観測度数が5未満のときはG値は当てはまりが悪い
# すべてのセルに対して|Oᵢ-Eᵢ| > Eᵢとなる場合には尤度比検定を用いるのが望ましい
# 標本サイズが小さい分割表では，フィッシャーの正確確率法が使える．
data.1 <- matrix(c(12, 7, 2, 9), nrow = 2, ncol = 2, byrow = T)
fisher.test(data.1, alternative = "g")

hair <- matrix(c(32, 43, 16, 9, 55, 65, 64, 16), nrow = 2, ncol = 4, byrow = T)
fisher.test(hair)

d <- matrix(c(54, 10325, 25, 51790), ncol = 2, nrow = 2, byrow = T)
fisher.test(d, or = T, conf.level = 0.95)
