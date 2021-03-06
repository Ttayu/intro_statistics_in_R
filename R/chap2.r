# 2. 母集団と標本
# 母集団(population): 調査対象となる数値や属性などを共有する集合全体
# 標本(sample): 母集団と共通の属性を持ち，母集団の真部分集合として母集団の推定のために設けられたデータの小さな集合
# 母平均μ: 母集団の代表的な値を示す平均値μ
# 母集団σ²: 母集団のデータのばらつき度合いを示す分散
# データ: 実験や調査などで取得した個々の値
# サンプルサイズ(標本サイズ): １つの標本に含まれる"データの数"
# サンプル数(標本数): 母集団から抽出する"標本の数"
# 無作為抽出: 乱数などを使って母集団からランダムに被験者や対象を選ぶやり方
# 母集団の属性を絞りすぎると命題の一般性が失われるというトレードオフ
# 偏差(diviation): 各データの平均値からの隔たり. 偏差 = Xᵢ - X̄
# 平方和(偏差の平方和): SS = Σⁿᵢ₌₁(Xᵢ - X̄)²
# 標本分散s²: 平方和はデータの個数に依存するためデータ個数で割って基準化された統計量
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
