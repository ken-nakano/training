# settings ----------------------------------------------------------------
## 作業ディレクトリへ移動
setwd("/Users/nakanoken/git/training/books/データ解析のための統計モデリング入門/")

## ライブラリ読み込み
library(dplyr)
library(ggplot2)
library(reshape2)
library(rstan)
library(psych)

## 関数作成
logistic <- function(x) {
  1 / (1+exp(-x))
}

## データ読み込み
curl <- "http://hosho.ees.hokudai.ac.jp/~kubo/stat/2016/Ees/g/fig/nested/d1.csv"
file <- "./00_data/data2.csv"
download.file(curl,file)
d <- read.csv("./00_data/data2.csv")


# 個体差,場所差を考慮しないポアソン回帰 -----------------------------------------------------
## 推定
fit.pois <- glm(y ~ f, data = d, family = poisson(link="log"))
summary(fit.pois) # 施肥処理の効果はマイナス(-0.412)



# 個体差,場所差を考慮したポアソン回帰（改装ベイズ） ------------------------------------------------------
## stanに渡すデータ作成
pot <- as.numeric(d$pot) # potをA~Jから1~10に変換
f <- if_else(d$f == "T", 1, 0) # fを0|1に変換
data <- list(N=nrow(d), N_pot=length(unique(pot)), PID=pot, f=f, y=d$y)


## 推定
fit.hb <- stan("./01_script/Chap10_model2.stan", data = data, seed = 1234)
fit.hb


## 植木鉢差を可視化
ms <- rstan::extract(fit.hb)
ms.pot <- data.frame(
  pot1 = ms$pot[,1],
  pot2 = ms$pot[,2],
  pot3 = ms$pot[,3],
  pot4 = ms$pot[,4],
  pot5 = ms$pot[,5],
  pot6 = ms$pot[,6],
  pot7 = ms$pot[,7],
  pot8 = ms$pot[,8],
  pot9 = ms$pot[,9],
  pot10 = ms$pot[,10]
)
ms.pot.melt <- melt(ms.pot)

p <- ggplot(ms.pot.melt, aes(x=value, fill=variable)) +
  geom_density() +
  labs(title="植木鉢差の推定", x="value", y="Density") +
  theme_bw(base_family = "HiraKakuPro-W3") +
  facet_grid(variable ~.)
ggsave("./02_output/Chap10_2_植木鉢差の推定.png", p, width=7, height=7)


## 施肥処理の効果を推定
ms.b <- data.frame(
  b1 = ms$b1,
  b2 = ms$b2
)
ms.b.melt <- melt(ms.b)
p <- ggplot(ms.b.melt, aes(x=value, fill=variable)) +
  geom_density() +
  labs(title="パラメータb1(切片),b2(施肥処理係数)の推定", x="value", y="Density") +
  theme_bw(base_family = "HiraKakuPro-W3") +
  facet_grid(variable ~.)
ggsave("./02_output/Chap10_2_施肥処理効果の推定.png", p, width=7, height=7)

quantile(ms$b2, probs = c(0.025, 0.05, 0.5, 0.95, 0.975))
# 90%信頼区間が0を含むので、施肥処理効果はあるとは言えない