###################################################################
#  This file read data from 4.HypothesisData folder and Performance file for all subjects and 
#  format the data for hypothesis testing.
###################################################################

setwd("E:/Github/1.project/Project-Statistical-Methods")
#getwd()


pp.data = read.csv("4.HypothesisData/hyp_test_3_final.csv")
performance.data = read.csv("Data/MicrosurgeryPerformance.csv")


id = unlist(lapply(performance.data$ID, function(x){rep(c(x),20)}))
id

age = unlist(lapply(performance.data$Age, function(x){rep(c(x),20)}))
age

sex = unlist(lapply(performance.data$Sex, function(x){if(x==1){rep(c("Male"),20)} else{rep(c("Female"),20)}}))
sex

subject = unlist(lapply(performance.data$ID, function(x){rep(paste(c("Subject",x), collapse = " "),20)}))
subject


session = rep(c(rep(c("Session 1"),4), rep(c("Session 2"),4), rep(c("Session 3"),4), rep(c("Session 4"),4),
            rep(c("Session 5"),4)),length(subject)/20)
session

task = rep(c(rep(c("Cutting"),2), rep(c("Suturing"),2)), length(subject)/4)
task


scorer = rep(c("Scorer 1","Scorer 2"),length(subject)/2)
scorer

score = as.numeric(as.vector(apply(performance.data, 1, function(x){c(x[8],x[10],x[9],x[11],x[15],x[17],x[16],x[18],x[22],
                                                 x[24],x[23],x[25],x[29],x[31],x[30],x[32],
                                                 x[36],x[38],x[37],x[39])})))
score



meanPP = unlist(lapply(pp.data$Mean.PP, function(x){rep(c(x),2)}))
meanPP


required.df = data.frame(id, age, sex, subject, session, task, meanPP,scorer, score)
required.df

write.csv(file="4.HypothesisData/hyp_test_for_randomized_data.csv", required.df, row.names = FALSE)








