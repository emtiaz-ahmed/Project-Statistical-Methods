setwd("E:/Github/1.project/Project-Statistical-Methods")
#getwd()
data = read.csv("4.HypothesisData/hypothesis_1_dataformat.csv")
data
names(data)

score1.analysis = lm(Score1 ~ meanData + task.label + session.label, data=data)
summary(score1.analysis)

score1.analysis = lm(Score2 ~ meanData + task.label + session.label, data=data)
summary(score1.analysis)