setwd("E:/Github/1.project/Project-Statistical-Methods")
#getwd()
library(ggplot2)
library(lubridate)


#...draw the performance plot for each subject based on their score
draw_performance_plot <- function(){
  #...read input file
  data = read.csv("MicrosurgeryPerformance.csv", header = TRUE, sep=",")
  #...create dataframe using subject name and all scores
  score = data.frame(data$Subject,data$Score.1, data$Score.2, data$Score.3, data$Score.4, data$Score.5)
  #...find the number of rows
  number.of.row = nrow(score)
  
  for(i in 1:number.of.row){
    #...select specific row from the dataframe
    sub.score = score[i,] 
    #...find the subject number
    subject.name = sub.score$data.Subject
    
    #...create x axis ticks
    label = c("Session 1", "Session 2", "Session 3", "Session 4", "Session 5")
    #...copy only the scores
    performance = c(sub.score$data.Score.1, sub.score$data.Score.2, sub.score$data.Score.3, sub.score$data.Score.4, sub.score$data.Score.5)
    
    #...create dataframe using label and performance
    subject.performance = data.frame(label, performance)
    
    #...concate string to create title
    barTitle = paste("Barplot of performance of subject", subject.name)
    xLabel = paste("Subject", subject.name)
    
    #...plot the graph
    ggplot(subject.performance, aes(x = label, y = performance)) + geom_bar(stat="identity", col = "red", fill="green", alpha=0.3) +
      labs(title = barTitle, x = xLabel, y = "Score") +
      theme(plot.title = element_text(hjust=0.5)) + 
      scale_y_continuous(breaks = seq(0,30,by=5), limits = c(0,30))
    
    outputFile = paste("performance/performance_plot_of_subject_",subject.name,".png")
    #...save the output files
    ggsave(file = outputFile, dpi = 600, width = 10, height = 8, units = "in")
  }
  
  
  
}

#...draw the cutting accuracy plots for each subject based on their cutting time
draw_cutting_accuracy_plot <- function(){
  #...read input file
  data = read.csv("MicrosurgeryPerformance.csv", header = TRUE, sep=",")
  #...create dataframe using subject name and all cutting time
  accuracy = data.frame(data$Subject, data$Cutting.Time.1, data$Cutting.Time.2, data$Cutting.Time.3, data$Cutting.Time.4, data$Cutting.Time.5)
  
  #...find the number of rows
  number.of.row = nrow(accuracy)
  
  for(i in 1:number.of.row){
    #...select specific row from the dataframe
    sub.accuracy = accuracy[i,] 
    #...find the subject number
    subject.name = sub.accuracy$data.Subject
    
    #...create x axis ticks
    label = c("Session 1", "Session 2", "Session 3", "Session 4", "Session 5")
    #...convert timing into time using ms()[member of lubridate library] and then convert into numeric value
    session1 = as.numeric(ms(sub.accuracy$data.Cutting.Time.1)) / 60
    session2 = as.numeric(ms(sub.accuracy$data.Cutting.Time.2)) / 60
    session3 = as.numeric(ms(sub.accuracy$data.Cutting.Time.3)) / 60
    session4 = as.numeric(ms(sub.accuracy$data.Cutting.Time.4)) / 60
    session5 = as.numeric(ms(sub.accuracy$data.Cutting.Time.5)) / 60
   
    #...copy the cutting accuracy's
    cutting.accuracy = c(session1, session2, session3, session4, session5)
    
    #...create dataframe using label and cutting accuracy
    subject.accuracy = data.frame(label, cutting.accuracy)
    
    #...concate string to create title
    barTitle = paste("Barplot of Cutting accuracy of subject", subject.name)
    xLabel = paste("Subject", subject.name)
    
    #...plot the graph
    ggplot(subject.accuracy, aes(x = label, y = cutting.accuracy)) + geom_bar(stat="identity", col = "red", fill="green", alpha=0.3) +
      labs(title = barTitle, x = xLabel, y = "Time") +
      theme(plot.title = element_text(hjust=0.5)) +
      scale_y_continuous(breaks = seq(0,10,by=2), limits = c(0,11))
      
    
    outputFile = paste("Cutting_accuracy/cutting_accuracy_plot_of_subject_",subject.name,".png")
    #...save the output files
    ggsave(file = outputFile, dpi = 600, width = 10, height = 8, units = "in")
  }
  
  
  
}

#...draw the suturing accuracy plots for each subject based on their suturing time
draw_suturing_accuracy_plot <- function(){
  #...read input file
  data = read.csv("MicrosurgeryPerformance.csv", header = TRUE, sep=",")
  #...create dataframe using subject name and all cutting time
  accuracy = data.frame(data$Subject, data$Suturing.Time.1 , data$Suturing.Time.2, data$Suturing.Time.3,
                        data$Suturing.Time.4, data$Suturing.Time.5)
  
  #...find the number of rows
  number.of.row = nrow(accuracy)
  
  for(i in 1:number.of.row){
    #...select specific row from the dataframe
    sub.accuracy = accuracy[i,] 
    #...find the subject number
    subject.name = sub.accuracy$data.Subject
    
    #...create x axis ticks
    label = c("Session 1", "Session 2", "Session 3", "Session 4", "Session 5")
    #...convert timing into time using ms()[member of lubridate library] and then convert into numeric value
    session1 = as.numeric(ms(sub.accuracy$data.Suturing.Time.1)) / 60
    session2 = as.numeric(ms(sub.accuracy$data.Suturing.Time.2)) / 60
    session3 = as.numeric(ms(sub.accuracy$data.Suturing.Time.3)) / 60
    session4 = as.numeric(ms(sub.accuracy$data.Suturing.Time.4)) / 60
    session5 = as.numeric(ms(sub.accuracy$data.Suturing.Time.5)) / 60
    
    #...copy the suturing accuracy's
    suturing.accuracy = c(session1, session2, session3, session4, session5)
    
    #...create dataframe using label and suturing accuracy
    subject.accuracy = data.frame(label, suturing.accuracy)
    
    #...concate string to create title
    barTitle = paste("Barplot of Suturing accuracy of subject", subject.name)
    xLabel = paste("Subject", subject.name)
    
    #...plot the graph
    ggplot(subject.accuracy, aes(x = label, y = suturing.accuracy)) + geom_bar(stat="identity", col = "red", fill="green", alpha=0.3) +
      labs(title = barTitle, x = xLabel, y = "Time") +
      theme(plot.title = element_text(hjust=0.5)) +
      scale_y_continuous(breaks = seq(0,20,by=5), limits = c(0,20))
    
    
    outputFile = paste("Suturing_accuracy/suturing_accuracy_plot_of_subject_",subject.name,".png")
    #...save the output files
    ggsave(file = outputFile, dpi = 600, width = 10, height = 8, units = "in")
  }
  
  
  
}

#...call the function to draw plots
draw_performance_plot()

draw_cutting_accuracy_plot()

draw_suturing_accuracy_plot()

















