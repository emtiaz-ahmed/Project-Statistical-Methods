###################################################################
#  This file read data from 3.Mean folder for all subjects and 
#  format the data for hypothesis 1 testing.
###################################################################

setwd("E:/Github/1.project/Project-Statistical-Methods")
#getwd()

totalSubject = dir(path = "3.Mean Data", full.names = TRUE, recursive = FALSE, pattern="*.csv")

formatted.df = data.frame(s_id.label = character(), subject.label=character(), session.label=character(), task.label=character(),
                          meanData=character(), score = character())



for(i in totalSubject){
  # i = "3.Mean Data/subject03.csv"
  data = read.csv(i)

  subject.name = strsplit(i,"/")[[1]]
  subject.name = strsplit(subject.name,".csv")[[2]]
  subject.name
  s_id = strsplit(subject.name, "subject")[[1]][2]
  s_id = as.character(as.integer(s_id))
  s_id
  
  s_id.label = rep(c(s_id),10)
  subject.label = rep(c(subject.name), 10)
  session.label = c(rep(c("Session 1"),2),rep(c("Session 2"),2),rep(c("Session 3"),2),
                    rep(c("Session 4"),2),rep(c("Session 5"),2))
  task.label = rep(c("Cutting","Suturing"),5)
  
  
  sub.data = data.frame(data$Baseline.Mean, data$Cutting.Mean, data$Suturing.Mean)
  
  
  meanData1 = c(as.character(unname(data[1,3])),as.character(unname(data[1,4])))
  meanData2 = c(as.character(unname(data[2,3])),as.character(unname(data[2,4])))
  meanData3 = c(as.character(unname(data[3,3])),as.character(unname(data[3,4])))
  meanData4 = c(as.character(unname(data[4,3])),as.character(unname(data[4,4])))
  meanData5 = c(as.character(unname(data[5,3])),as.character(unname(data[5,4])))
  meanData = c(meanData1, meanData2, meanData3, meanData4,meanData5)
  
  
  subject.dataframe = data.frame(s_id.label,subject.label, session.label, task.label, meanData)
  
  formatted.df = rbind(formatted.df, subject.dataframe)
  
  

}



write.csv(file="4.HypothesisData/hypothesis_1_dataformat.csv", formatted.df, row.names = FALSE)














