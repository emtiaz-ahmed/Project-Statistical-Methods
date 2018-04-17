setwd("E:/Github/1.project/Project-Statistical-Methods")

required.df = read.csv("4.HypothesisData/hyp_test_for_randomized_data.csv")
required.df

model1 = lm(score ~ meanPP + age + sex + session + task + scorer, data=required.df)
summary(model1)

model2 = lm(score ~ log(meanPP) + age + sex + session + task + scorer, data=required.df)
summary(model2)

model3 = glm(score ~ meanPP + age + sex + session + task + scorer, data=required.df)
summary(model3)
