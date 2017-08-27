library(stringr)
library(data.table)
setwd("C:/Users/aqmhussa/Desktop/Attribution_modelling")
data <- fread('results_90days.csv', sep=',')
data <- data.frame(data)
data <- subset(data, convert == 1)
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
names(final_df) <- unique_points



for(i in 1:nrow(data))
{
  lst <- unlist(strsplit(p[i],'>',fixed = TRUE))
  df <- data.frame(table(lst))
  df$weight <- 0
  df$path_number <- i
  
 # w <- matrix(0,length(df$lst),1)
  c <- 1

  for(var in df$lst)
  {  
    if(length(grep('(impression)',var)) == 0)
    {
      df[c,'weight'] <- df[c,]$Freq
    }
    else
    {
      df[c,'weight'] <- 0.5 * df[c,]$Freq
    }
    c <- c + 1
  }
  
  total_weight <- sum(df$weight)
  df$avg_weighted <- df$weight/total_weight
  df$final_weighted <- df$avg_weighted * conv_value[i]
  
  final_df[i,as.character(df$lst)] <- df$final_weighted           ####Automatically takes into account the ordering.
}

weighted_model <- colSums(final_df)


# sum(weighted_model) == sum(conv_value)  #####To check correctness

# print(sum(weighted_model), digits = 20)

# print(sum(conv_value), digits = 20)

total <- sum(weighted_model)
percent_contribution <- sapply(weighted_model, function(x) {x/total * 100})


write_out <- data.frame(weighted_model)
final <- cbind(names(weighted_model), write_out[,1])
final <- data.frame(final)
names(final) <- c("Placement points","Contribution")

write.csv(final, "weighted_model_DBS.csv", row.names = F)




