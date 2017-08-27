library(stringr)
library(data.table)
setwd("C:/Users/aqmhussa/Desktop/Attribution_modelling")
data <- fread('results_90days.csv', sep = ',')
data <- data.frame(data)
data <- subset(data,convert == 1)

names(data)[2] <- 'path'

p <- data$path
p <- as.character(p)
p <- str_replace_all(p, fixed(" "), "")
p <- str_replace_all(p,fixed("(>)"),"")


all_points <- NULL
for(i in 1:nrow(data))
{
  lst <- unlist(strsplit(p[i],'>',fixed = TRUE))
  all_points <- rbind(all_points,data.frame(lst))
}


unique_points <- unique(all_points$lst)


conv_value <- data$Conversion


final_df <- data.frame(matrix(0,nrow(data),length(unique_points)))
final_df <- data.frame(final_df)
names(final_df) <- unique_points

for(i in 1:nrow(data))
{
  lst <- unlist(strsplit(p[i],'>',fixed = TRUE))
  last <- lst[length(lst)]
  last_weight <- conv_value[i]
  final_df[i,last] <- last_weight
}


last_click_model <- colSums(final_df)


total <- sum(last_click_model)
percent_contribution <- sapply(last_click_model, function(x) {x/total * 100})


write_out <- data.frame(last_click_model)
final <- cbind(names(last_click_model), write_out[,1])
final <- data.frame(final)
names(final) <- c("Placement points","Contribution")

write.csv(final, "last_click_DBS.csv", row.names = F)


