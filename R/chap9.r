# 9. 回帰
# 回帰分析: 片方の変数Xからもう片方の変数Yを説明する四季を求める
# 相関はXとYの両変数の関連性の程度を定量化
# 回帰はXからYを説明する関係式を求め，XとYの間にどのような関係や構造があるかを定量する．
# 説明変数 (独立変数): Yを説明する変数X
# 応答変数 (従属変数): 説明される変数Y
# 単回帰: 線形回帰で説明変数が一つの場合
# 最小二乗法: Σⁿᵢ₌₁σᵢ² = Σⁿᵢ₌₁(Yᵢ - Ŷᵢ)²を最小にすることで直線 Ŷᵢ = a + bXᵢのaとbを求める
x1 <- c(19, 23, 27, 35, 44, 51, 59, 66)
y1 <- c(124, 117, 120, 132, 128, 142, 143, 135)
plot(y1 ~ x1, ylim = c(110, 150))
result <- lm(y1 ~ x1)
abline(result, ylim = c(110, 150))
summary(result)

x2 <- x1
y2 <- c(111, 129, 113, 137, 147, 126, 135, 139)
plot(y2 ~ x2, ylim = c(110, 150))
result <- lm(y2 ~ x2)
abline(result, ylim = c(110, 150))
summary(result)

x3 <- c(x2, x2)
y3 <- c(y2, y2)
plot(y3 ~ x3, ylim = c(110, 150))
result <- lm(y3 ~ x3)
abline(result, ylim = c(110, 150))
summary(result)

# 決定係数: 説明変数が応答変数のどれだけの割合を説明できるかを表す．寄与率．
# 回帰の説明力を表すと言われているが，傾きの有意確率の小ささを意味するわけではない
x <- c(1, 2, 4, 5, 3, 2, 3, 1, 5, 4, 4, 2)
y <- c(3, 4, 4, 5, 5, 3, 4, 3, 6, 6, 5, 5)
plot(x, y, xlim = c(0, 7), ylim = c(0, 7), xlab = "X (year)", ylab = "Y (year")
result <- lm(y ~ x)
abline(result)
summary(result)

age <- c(3.0, 4.0, 5.0, 6.0, 8.0, 9.0, 10.0, 11.0, 12.0, 14.0, 15.0, 16.0, 17.0)
wing <- c(1.4, 1.5, 2.2, 2.4, 3.1, 3.2, 3.2, 3.9, 4.1, 4.7, 4.5, 5.2, 5.0)
summary(result <- lm(wing ~ age))
result.plot <- predict(result, interval = "confidence")
matplot(age, result.plot,
  xlab = "day", ylab = "wing (cm)",
  ylim = c(0, 6), type = "l"
)
par(new = T)
plot(age, wing, xaxt = "n", xlab = "", ylab = "", ylim = c(0, 6), type = "p")

# 回帰直線の95%信頼区間: ランダムサンプリングによってデータを得て，回帰直線を推定したら，
# 真の回帰直線は20回に19回はこの範囲を通る．

# 回帰の95%予測区間: データプロットの95%が含まれる領域
# 将来の未知の1つのデータが得られたときにその予測に対する区間.
age <- c(3.0, 4.0, 5.0, 6.0, 8.0, 9.0, 10.0, 11.0, 12.0, 14.0, 15.0, 16.0, 17.0)
wing <- c(1.4, 1.5, 2.2, 2.4, 3.1, 3.2, 3.2, 3.9, 4.1, 4.7, 4.5, 5.2, 5.0)
summary(result <- lm(wing ~ age))
result.plot <- predict(result, interval = "prediction")
matplot(age, result.plot,
  xlab = "day", ylab = "wing (cm)",
  ylim = c(0, 6), type = "l"
)
par(new = T)
plot(age, wing, xaxt = "n", xlab = "", ylab = "", ylim = c(0, 6), type = "p")
