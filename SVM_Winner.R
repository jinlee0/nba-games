# install.packages('dplyr')
library(dplyr)
# install.packages('e1071')
library(e1071)
# install.packages('Epi')
library(Epi)

### 전처리
games.df = read.csv('./data/games.csv')
games.df = games.df[order(games.df$GAME_DATE_EST), ]
# 2004-01-01 이전 데이터에는 결측치가 있다. 해당 시즌의 데이터는 제거한다.
games.df = games.df %>% filter(GAME_DATE_EST >= "2004-01-01")

sum(is.null(games.df))
sum(is.na(games.df))
head(games.df)

# 정규화
library("caret")
games.df = predict(preProcess(x = games.df[c(8:13, 15:20)], method = c("range")), games.df)
head(games.df)

valid.df = games.df %>% filter(GAME_DATE_EST >= "2022-01-01")
games.df = games.df %>% filter(GAME_DATE_EST < '2022-01-01')

reg = lm(HOME_TEAM_WINS~.,
         data=games.df[c(8:13, 15:20, 21)], 
         na.action = na.exclude)
options(scipen = 999)
summary(reg)

# SVM에서 종속변수, 독립변수에 대한 formula를 정의한다.
#formula.svm = HOME_TEAM_WINS~FG_PCT_home+FG3_PCT_home+AST_home+FG_PCT_away+FG3_PCT_away+AST_away
formula.svm = HOME_TEAM_WINS~FG_PCT_home+FG3_PCT_home+FG_PCT_away+FG3_PCT_away
games.df$HOME_TEAM_WINS = factor(games.df$HOME_TEAM_WINS)

# gamma, cost 최적값 찾기기
# 굉장히 오래 걸리기 때문에 무작위 1000개 데이터로 해본다.
# 실제 모델을 만들 때에는 전체 데이터를 활용해 좀 더 정확한 값을 찾아야 한다.
games.best.model = tune.svm(formula.svm,
         data = games.df[sample(1:nrow(games.df), size = 1000),],
         gamma = 2^(-1:1),
         cost = 2^(2:4))
games.best.model

train.index = sample(1:nrow(games.df), size = nrow(games.df) * 0.7)
train.df = games.df[train.index,]
test.df = games.df[-train.index, ]

# 트레이닝 데이터 학습. 오래 걸린다. 2분 정도.
train.model = svm(formula.svm,
    data = train.df,
    gamma = games.best.model$best.parameters$gamma,
    cost = games.best.model$best.parameters$cost)
train.model

# 테스트 데이터로 예측값 구하기
test.pred.list = predict(train.model, test.df)
test.pred.table = table(real=test.df$HOME_TEAM_WINS, pred=test.pred.list)
# confusion matrix
test.pred.table
# accuracy
(test.pred.table[1,1] + test.pred.table[2,2]) / sum(test.pred.table)

Epi::ROC(test=test.pred.list, stat=test.df$HOME_TEAM_WINS, plot='ROC', AUC=T, main="SVM")

# 모델 완성하기. 역시 오래 걸린다.
# model = svm(formula.svm,
#                   data = games.df,
#                   gamma = games.best.model$best.parameters$gamma,
#                   cost = games.best.model$best.parameters$cost)
# model
