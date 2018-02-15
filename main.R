setwd("C:/Users/Emtiaz/Documents/R/code/Project")
#getwd()
library(ggplot2)

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

#...call the function to draw plots
draw_performance_plot()

















