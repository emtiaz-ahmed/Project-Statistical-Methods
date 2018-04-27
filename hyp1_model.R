setwd("E:/Github/1.project/Project-Statistical-Methods")
#getwd()
data = read.csv("4.HypothesisData/hypothesis_1_dataformat.csv")
data
names(data)

score1.analysis = lm(Score1 ~ meanData + task.label + session.label, data=data)
summary(score1.analysis)

# Call:
#   lm(formula = Score1 ~ meanData + task.label + session.label, 
#      data = data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -10.6459  -2.5121   0.5515   2.6759   7.3944 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             17.0108     1.4610  11.643  < 2e-16 ***
#   meanData               -81.4889   176.9427  -0.461  0.64587    
# task.labelSuturing      -1.7409     0.6629  -2.626  0.00963 ** 
#   session.labelSession 2   4.3351     1.0533   4.116 6.68e-05 ***
#   session.labelSession 3   6.1223     1.0550   5.803 4.41e-08 ***
#   session.labelSession 4   7.4383     1.0491   7.090 6.78e-11 ***
#   session.labelSession 5   8.3846     1.0713   7.826 1.30e-12 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.939 on 135 degrees of freedom
# (68 observations deleted due to missingness)
# Multiple R-squared:  0.3859,	Adjusted R-squared:  0.3586 
# F-statistic: 14.14 on 6 and 135 DF,  p-value: 1.885e-12


score1.analysis = lm(Score2 ~ meanData + task.label + session.label, data=data)
summary(score1.analysis)


# Call:
#   lm(formula = Score2 ~ meanData + task.label + session.label, 
#      data = data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -8.4869 -2.5161  0.3714  2.5522  8.4473 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              17.6854     1.3529  13.072  < 2e-16 ***
#   meanData               -190.5619   163.8542  -1.163 0.246883    
# task.labelSuturing       -0.7816     0.6138  -1.273 0.205108    
# session.labelSession 2    3.8890     0.9754   3.987 0.000109 ***
#   session.labelSession 3    6.2548     0.9769   6.403 2.34e-09 ***
#   session.labelSession 4    7.2321     0.9715   7.444 1.03e-11 ***
#   session.labelSession 5    7.3521     0.9921   7.411 1.23e-11 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.648 on 135 degrees of freedom
# (68 observations deleted due to missingness)
# Multiple R-squared:  0.3733,	Adjusted R-squared:  0.3454 
# F-statistic:  13.4 on 6 and 135 DF,  p-value: 6.995e-12



