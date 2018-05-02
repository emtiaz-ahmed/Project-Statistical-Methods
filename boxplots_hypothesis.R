setwd("E:/Github/1.project/Project-Statistical-Methods")

library(ggplot2)

required.df = read.csv("4.HypothesisData/final_formatted_data.csv")
required.df


ggplot(aes(y = score,x = session),data = required.df) + geom_boxplot() +
  theme_bw() +
  labs(title = "Boxplot of Session vs Score",x="", y="Score") +
  theme(plot.title = element_text(hjust=0.5)) +
  stat_summary(fun.y=mean, colour="red", geom="point", shape=8, size=3, show.legend=F)

ggsave(file="hypothesis_plots/session_vs_score.pdf",width = 8, height = 6, units = "in")

ggplot(aes(y = score,x = task),data = required.df) + geom_boxplot() +
  theme_bw() +
  labs(title = "Boxplot of Task vs Score",x="", y="Score") +
  theme(plot.title = element_text(hjust=0.5)) +
  stat_summary(fun.y=mean, colour="red", geom="point", shape=8, size=3, show.legend=F)

ggsave(file="hypothesis_plots/task_vs_score.pdf",width = 8, height = 6, units = "in")


ggplot(aes(y = timing,x = session),data = required.df) + geom_boxplot() +
  theme_bw() +
  labs(title = "Boxplot of Session vs Timing",x="", y="Time [s]") +
  theme(plot.title = element_text(hjust=0.5)) +
  stat_summary(fun.y=mean, colour="red", geom="point", shape=8, size=3, show.legend=F)

ggsave(file="hypothesis_plots/session_vs_timing.pdf",width = 8, height = 6, units = "in")

ggplot(aes(y = timing,x = task),data = required.df) + geom_boxplot() +
  theme_bw() +
  labs(title = "Boxplot of Task vs Timing",x="", y="Time [s]") +
  theme(plot.title = element_text(hjust=0.5)) +
  stat_summary(fun.y=mean, colour="red", geom="point", shape=8, size=3, show.legend=F)

ggsave(file="hypothesis_plots/task_vs_timing.pdf",width = 8, height = 6, units = "in")

ggplot(aes(y = sutures,x = session),data = required.df) + geom_boxplot() +
  theme_bw() +
  labs(title = "Boxplot of Session vs number of Sutures",x="", y="Number of Sutures") +
  theme(plot.title = element_text(hjust=0.5)) +
  stat_summary(fun.y=mean, colour="red", geom="point", shape=8, size=3, show.legend=F)

ggsave(file="hypothesis_plots/session_vs_sutures.pdf",width = 8, height = 6, units = "in")

ggplot(aes(y = sutures,x = factor(ms.year)),data = required.df) + geom_boxplot() +
  theme_bw() +
  labs(title = "Boxplot of Experience vs number of Sutures",x="Year", y="Number of Sutures") +
  theme(plot.title = element_text(hjust=0.5)) +
  stat_summary(fun.y=mean, colour="red", geom="point", shape=8, size=3, show.legend=F)  

ggsave(file="hypothesis_plots/experience_vs_sutures.pdf",width = 8, height = 6, units = "in")

ggplot(aes(y = score,x = factor(ms.year)),data = required.df) + geom_boxplot() +
  theme_bw() +
  labs(title = "Boxplot of Experience vs Score",x="Year", y="Score") +
  theme(plot.title = element_text(hjust=0.5)) +
  stat_summary(fun.y=mean, colour="red", geom="point", shape=8, size=3, show.legend=F)


ggsave(file="hypothesis_plots/experience_vs_score.pdf",width = 8, height = 6, units = "in")




