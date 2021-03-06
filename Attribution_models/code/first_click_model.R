library(stringr)
setwd("C:/Users/aqmhussa/Desktop")
data <- read.csv('tcp.csv')
names(data)[1] <- 'path'
names(data)[4] <- 'revenue'
p <- data$path
p <- as.character(p)
p <- str_replace_all(p, fixed(" "), "")


all_points <- NULL
for(i in 1:nrow(data))
{
  lst <- unlist(strsplit(p[i],'>',fixed = TRUE))
  all_points <- rbind(all_points,data.frame(lst))
}

all_points <- as.character(all_points[complete.cases(all_points),])
unique_points <- unique(all_points)


conv_value <- data$revenue
conv_value <- gsub('SGD','',conv_value)
conv_value <- gsub(',','',conv_value)
conv_value <- as.numeric(conv_value)


final_df <- data.frame(matrix(0,nrow(data),length(unique_points)))
names(final_df) <- unique_points

for(i in 1:nrow(data))
{
  lst <- unlist(strsplit(p[i],'>',fixed = TRUE))
  first <- lst[1]
  first_weight <- conv_value[i]
  final_df[i,as.character(first)] <- first_weight
}


first_click_model <- colSums(final_df)


total <- sum(first_click_model)
percent_contribution <- sapply(first_click_model, function(x) {x/total * 100})





