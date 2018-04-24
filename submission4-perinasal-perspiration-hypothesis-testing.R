#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(dplyr)
library(here)


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

convertToCamelCase <- function(x){
  capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
  sapply(strsplit(x, "\\."), function(x) paste(capit(x), collapse=""))
}

convert_to_csv <- function(hyp_df, output_file) {
  file_path <- paste0(file.path(hyp_dir, output_file), '.csv')
  write.csv(hyp_df, file=file_path, row.names = F)
}

#-------------------------#
#-------Main Program------#
#-------------------------#
current_dir <- here()
hyp_dir <- concatePath(current_dir,  "4.HypothesisData")

setwd(hyp_dir)


hyp_data <- read.csv('hyp_test_3_final.csv')
hyp_data$Base.Mean.PP <- hyp_data$Mean.PP 
head(hyp_data)


min_pp_abs <- abs(min(hyp_data$Mean.PP, na.rm=T))
pos_pp <- hyp_data$Mean.PP[hyp_data$Mean.PP > 0]
e <- min(pos_pp, na.rm=T)
hyp_data$Mean.PP <- hyp_data$Mean.PP + min_pp_abs + e
head(hyp_data)

convert_to_csv(hyp_data, 'hyp_test_4')



