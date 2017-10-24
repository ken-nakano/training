# 設定 ----------------------------------------------------------------------
library(lavaan)
library(semPlot)

setwd("/Users/nakanoken/git/training/books/共分散構造分析[R編]/")


# データインポート ----------------------------------------------------------------
dat <- read.csv("./00_data/seminar_copy.csv", row.names = 1)
head(dat)

colnames(dat) <- c("x1","x2","x3","x4","x5","x6","x7")


# 構造方程式モデリング --------------------------------------------------------------
## モデル作成
model1 <- '
  f1 =~ x1 + x2 + x3 + x4
  f2 =~ x5 + x6 + x7
  f2 ~ f1
'

# パラメータ推定
fit.sem <- sem(model=model1, data=dat, estimator='ML')
summary(fit.sem)

# 標準化推定値を求める
standardizedSolution(object=fit.sem)

# 構成概念スコアを算出
factor_score <- predict(object=fit.sem)
plot(factor_score)

# パス図の出力
semPaths(object=fit.sem, whatLabels="stand", optimizeLatRes=T)