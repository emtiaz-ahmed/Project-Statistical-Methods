#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(dplyr)


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
concatePath <- function(path1, path2) {
  file.path(path1, path2)
}

findFileAndSetPath <- function(file_dir, file_name_pattern) {
  file_name <- list.files(path=file_dir, pattern=file_name_pattern, recursive=FALSE)
  concatePath(file_dir, file_name)
}

getMean <- function(file_dir, file_name_pattern) {
  file_name <- list.files(path=file_dir, pattern=file_name_pattern, recursive=FALSE)
  file_path <- concatePath(file_dir, file_name)
  
  if(identical(file_path, character(0))) {
    return('N/A')
  } else {
    df <- read.csv(file_path)
    # return(mean(df[!is.na(df$Perspiration)]))
    print(mean(df$Perspiration))
    return(mean(df$Perspiration))
  }
  
}

convertToCamelCase <- function(x){
  capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
  sapply(strsplit(x, "\\."), function(x) paste(capit(x), collapse=""))
}

# create_subj_dir_mean <- function(subj_name) {
#   subj_dir_mean <- file.path(mean_output_dir, subj_name)
#   
#   if (!file.exists(subj_dir_mean)){
#     dir.create(subj_dir_mean)
#   }
# }

convert_to_csv <- function(mean_df, subj_name) {
  subj_path_mean <- paste0(file.path(mean_output_dir, subj_name), '.csv')
  write.csv(mean_df, file = subj_path_mean, row.names = F)
}

#-------------------------#
#-------Main Program------#
#-------------------------#
data_dir <- "T:/Google Drive/University of Houston/CS - UH/@@Spring 2018/Statistical Methods for Research/@Project-Stats/Code/Project-Statistical-Methods/Data"
mean_output_dir <- "T:/Google Drive/University of Houston/CS - UH/@@Spring 2018/Statistical Methods for Research/@Project-Stats/Code/Project-Statistical-Methods/3.Mean Data"

setwd(data_dir)
getwd()

baseline_file_pattern <- ".*Baseline..csv"
cutting_file_pattern <- ".*Cutting..csv"
suturing_file_pattern <- ".*Suturing..csv"

#USE APPLY FAMILY
subj_list <- list.dirs(path=data_dir, full.names=FALSE, recursive=FALSE)


for(subj_idx in 1 : length(subj_list)) {
  # for(subj_idx in 8 : 8) {
  
  subj_name <- subj_list[subj_idx]
  subj_dir <- file.path(data_dir, subj_name)
  session_list <- list.dirs(path=subj_dir, full.names=FALSE, recursive=FALSE)
  
  ## INITIALIZING EMPTY DATAFRAME ##
  mean_df <- data.frame(
      "Session Names" = character(0),
      "Baseline Mean" = integer(0),
      "Cutting Mean" = integer(0),
      "Suturing Mean" = integer(0),
      check.names = F,
      stringsAsFactors = F
    )
  
  for(sess_idx in 1 : length(session_list)) {
    # for(sess_idx in 1 : 1) {
    
    session_name <- session_list[sess_idx]
    session_path <- file.path(subj_name, session_name)
    
    baseline_mean <- getMean(session_path, baseline_file_pattern)
    cutting_mean <- getMean(session_path, cutting_file_pattern)
    suturing_mean <- getMean(session_path, suturing_file_pattern)
    
    ## APPENDING EACH ROW FOR A SESSION ##
    mean_df <- rbind(mean_df,
      data.frame(
        "Session Names" = convertToCamelCase(session_name),
        "Baseline Mean" = baseline_mean,
        "Cutting Mean" = cutting_mean,
        "Suturing Mean" = suturing_mean,
        check.names = F,
        stringsAsFactors = F
      )
    )
    
    rm(baseline_mean, cutting_mean, suturing_mean)
  }
  
  convert_to_csv(mean_df, subj_name)
}