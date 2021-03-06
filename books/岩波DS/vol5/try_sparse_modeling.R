# settings -------------------------------------------------------------------
library(MASS)
library(dplyr)
library(psych)
library(glmnet)

options(scipen = 100)

data(Boston)
df <- Boston


# データを眺める -----------------------------------------------------------------
head(df)
summary(df)
describe(df)

## 目的変数：medv (1000ドル単位で示された住宅価格)


# まずは何も考えずに重回帰 ------------------------------------------------------------
df.lm <- data.frame(cbind(scale(df %>% select(-medv)), medv = df$medv))
fit.lm <- lm(medv ~ ., data = df.lm)

summary(fit.lm)

### 手動で変数選択する
fit.lm.2 <- lm(medv ~
                 crim +
                 zn +
                 # indus +
                 chas +
                 nox +
                 rm +
                 # age +
                 dis +
                 rad +
                 tax +
                 ptratio +
                 black +
                 lstat
               , data = df.lm)

summary(fit.lm.2) # indus と age を削除して後は残す。これと以下のL1正則化の結果を比較してみる







# glmnetを試す ---------------------------------------------------------------
y <- unlist(df %>% select(medv))
x <- scale(as.matrix(df %>% select(-medv))) # matrix型にする必要あり。標準化もしておく

fit.glmnet <- glmnet(x, y, standardize = F, alpha = 1, family = "gaussian") # standardizeをTRUEにすると自動的に標準化されるので手動でやらなくてよい。alpha=1 はL1正則化

plot(fit.glmnet, label=T)
coef(fit.glmnet)

### CVで罰則の強さlambdaを選ぶ
cvfit.glmnet <- cv.glmnet(x, y, standardize = F, alpha = 1, family = "gaussian")
plot(cvfit.glmnet)

l.min <- cvfit.glmnet$lambda.min # CVで評価した2乗誤差を最小にするlambda
l.1se <- cvfit.glmnet$lambda.1se # lambda.minの時と比べて、2乗誤差の値がその標準誤差以上増えない最大のlambda

log(l.min) 
log(l.1se) 

coef(cvfit.glmnet, s="lambda.min") 
coef(cvfit.glmnet, s="lambda.1se") # コッチのほうがスパース



# fused lasso -------------------------------------------------------------
## 以降省略

