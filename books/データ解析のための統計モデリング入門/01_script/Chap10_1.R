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
curl <- "http://hosho.ees.hokudai.ac.jp/~kubo/stat/iwanamibook/fig/hbm/data7a.csv"
file <- "./00_data/data1.csv"
download.file(curl,file)
d <- read.csv("./00_data/data1.csv")



# データを眺めてみる ---------------------------------------------------------------
head(d)

describe(d)

p <- ggplot(d, aes(x=y)) +
  geom_histogram() +
  labs(title="実際のデータの分布", x="生存種子数", y="観測数") +
  theme_bw(base_family = "HiraKakuPro-W3")

ggsave("./02_output/Chap10_1_実データの分布.png", p, width=5, height=5)



# 個体差を考慮しないロジスティック回帰 ------------------------------------------------------
## 推定
fit.logistic <- glm(cbind(y, 8-y) ~ 1, data = d, family = binomial(link = "logit"))
summary(fit.logistic)


## 推定した二項分布と、実際の分布を比較
q.logistic <- logistic(fit.logistic[[1]])
bs <- rbinom(100, 8, prob = q.logistic)

d.logistic <- data.frame(
  実際の分布 = d$y,
  個人差考慮せず推定した分布 = bs
)
d.logistic.melt <- melt(d.logistic)

p <- ggplot(d.logistic.melt, aes(x=value, fill=variable)) +
  geom_histogram() +
  labs(title="推定した二項分布と実際の分布", x="生存種子数", y="観測数") +
  theme_bw(base_family = "HiraKakuPro-W3") +
  facet_grid(variable ~.)

ggsave("./02_output/Chap10_1_実データの分布と個体差を考慮しない解析結果の比較.png", p, width=7, height=7)





# 個体差を考慮したロジスティック回帰 -------------------------------------------------------
## stanに渡すデータ作成
data <- list(N=nrow(d), y=d$y)

## 推定
fit.hb <- stan("./01_script/Chap10_model1.stan", data = data, seed = 1234, pars = c("b", "s"))
fit.hb

## 推定した二項分布と、実際の分布を比較
ms <- rstan::extract(fit.hb)

b <- mean(ms$b)
s <- rnorm(100, mean=0, sd=mean(ms$s))

q.hb <- logistic(b + s)
hb <- rbinom(100, 8, prob = q.hb)

d.hb <- data.frame(
  実測値 = d$y,
  個人差考慮して推定した分布 = hb,
  個人差考慮せず推定した分布 = bs
)
d.hb.melt <- melt(d.hb)

p <- ggplot(d.hb.melt, aes(x=value, fill=variable)) +
  geom_histogram() +
  labs(title="推定した二項分布と実際の分布", x="生存種子数", y="観測数") +
  theme_bw(base_family = "HiraKakuPro-W3") +
  facet_grid(variable ~.)

ggsave("./02_output/Chap10_1_実データの分布と個体差を考慮した解析結果の比較.png", p, width=7, height=7)
