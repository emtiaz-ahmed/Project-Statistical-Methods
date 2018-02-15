setwd("E:/Github/1.project/Project-Statistical-Methods")
#getwd()
library(ggplot2)
library(lubridate)

#...read input file
data = read.csv("MicrosurgeryPerformance.csv", header = TRUE, sep=",")




draw_Biographic_Data_plots<-function(data){
  label = c("Male", "Female")
  
  male = sum(data$Sex == 1)
  female = sum(data$Sex == 2)
  genders = c(male, female)
  
  gender.frame = data.frame(label, genders)
  gender.frame
  
  ggplot(gender.frame, aes(x=label, y=genders, fill=label)) + geom_bar(stat = "identity") +
    scale_fill_manual(values = c("deeppink1","dodgerblue1")) +
    labs(title="Barplot of Gender distribution", x="Gender", y="Count") +
    theme(plot.title = element_text(hjust=0.5)) +
    scale_y_continuous(breaks = seq(0,12,by=2), limits = c(0,12))
  
  ggsave(file="Quality_control/Biographic_data/gender_distribution.png", dpi = 600, width = 10, height = 8, units = "in")
  
}


draw_Biographic_Data_plots(data)
