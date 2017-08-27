library(stringr)
library(data.table)
library(ChannelAttribution)
setwd("C:/Users/aqmhussa/Desktop/Attribution_modelling")
data <- fread('results_90days.csv', sep=',')
data <- data.frame(data)

data$PlacementPath <- as.character(data$PlacementPath)
data$PlacementPath <- str_replace_all(data$PlacementPath, fixed(" "), "")
data$PlacementPath <- str_replace_all(data$PlacementPath,fixed("(>)"),"")
data$PlacementPath <- str_replace_all(data$PlacementPath,fixed(">")," > ")


data$Conversion <- as.numeric(data$Conversion)

markov_df <- subset(data,convert == 1)

markov_df$convert <- NULL

aggr <- setDT(markov_df)[,lapply(.SD,sum),by = PlacementPath]

markov_df <- setDF(aggr)

names(markov_df) <- c('path','total_conversions')

M <- markov_model(markov_df, 'path', 'total_conversions',order = 1,out_more = 1) 

rm_effects <- M$removal_effects

write.csv(rm_effects,'removal.csv',row.names = FALSE)

##############Weighetd model###############

p <- markov_df$path
p <- as.character(p)
p <- str_replace_all(p, fixed(" "), "")


all_points <- NULL
for(i in 1:nrow(markov_df))
{
  lst <- unlist(strsplit(p[i],'>',fixed = TRUE))
  all_points <- rbind(all_points,data.frame(lst))
}

unique_points <- unique(all_points$lst)



conv_value <- markov_df$total_conversions


final_df <- data.frame(matrix(0,nrow(markov_df),length(unique_points)))
names(final_df) <- unique_points



for(i in 1:nrow(markov_df))
{
  lst <- unlist(strsplit(p[i],'>',fixed = TRUE))
  df <- data.frame(table(lst))
  df$weight <- 0
  df$path_number <- i

  w <- matrix(0,length(df$lst),1)
  c <- 1
  for(point in df$lst)
  {
    t <- subset(rm_effects,point == channel_name)$removal_effects
    w[c,1] <- t 
    c <- c + 1
  }
#  w <- data.frame(w)
  
  df$weight <- w
  
  total_weight <- sum(df$weight)
  df$avg_weighted <- df$weight/total_weight
  df$final_weighted <- df$avg_weighted * conv_value[i]
  
  final_df[i,as.character(df$lst)] <- df$final_weighted           ####Automatically takes into account the ordering.
}

markov_model <- colSums(final_df)


# sum(weighted_model) == sum(conv_value)  #####To check correctness

# print(sum(weighted_model), digits = 20)

# print(sum(conv_value), digits = 20)

total <- sum(markov_model)
percent_contribution <- sapply(markov_model, function(x) {x/total * 100})


write_out <- data.frame(markov_model)
final <- cbind(names(markov_model), write_out[,1])
final <- data.frame(final)
names(final) <- c("Placement points","Contribution")

write.csv(final, "markov_model_DBS.csv", row.names = F)


