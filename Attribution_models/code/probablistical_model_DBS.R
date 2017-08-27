library(data.table)

library(stringr)

setwd("C:/Users/aqmhussa/Desktop/Attribution_modelling")


data <- fread('results_90days.csv',sep=',',header = TRUE)
data <- data.frame(data)

names(data)[2] <- 'path'

p <- data$path
p <- as.character(p)
p <- str_replace_all(p, fixed(" "), "")
p <- str_replace_all(p,fixed("(>)"),"")

conv_value <- as.numeric(data$Conversion)

data_converted <- subset(data,convert == 1)

p_converted <- data_converted$path
p_converted <- as.character(p_converted)
p_converted <- str_replace_all(p_converted, fixed(" "), "")
p_converted <- str_replace_all(p_converted,fixed("(>)"),"")


conv_value_converted <- as.numeric(data_converted$Conversion)

#####Calculating the unique placement points#######

points_total <- sapply(p,function(x) strsplit(x,'>',fixed = TRUE))

table_total <- data.frame(table(unlist(sapply(points_total,function(x) unique(x)))))

names(table_total) <- c('Channel','Occurence')



first_all <- matrix(0,1,length(table_total$Channel))
colnames(first_all) <- table_total$Channel

first_convert <- matrix(0,1,length(table_total$Channel))
colnames(first_convert) <- table_total$Channel


for(i in 1:nrow(data))
{
  lst <- unique(unlist(strsplit(p[i],'>',fixed = TRUE)))
  
  for(j in lst)
  {
    if(data$convert[i] == 1)
    {
      first_convert[1,j] <- first_convert[1,j] + conv_value[i]
    }
    first_all[1,j] <- first_all[1,j] + conv_value[i]
  }
}

first_prob <- first_convert/first_all






pair_prob_all <- matrix(0,ncol(first_prob),ncol(first_prob))

rownames(pair_prob_all) <- table_total$Channel
colnames(pair_prob_all) <- table_total$Channel

pair_prob_convert <- matrix(0,ncol(first_prob),ncol(first_prob))

rownames(pair_prob_convert) <- table_total$Channel
colnames(pair_prob_convert) <- table_total$Channel


for( i in 1:nrow(data))
{
  lst <- unique(unlist(strsplit(p[i],'>',fixed = TRUE)))
  
  if(length(lst) == 1)
  {
    next
  }
  
  for(j in 1:(length(lst) - 1))
  {
    for(k in (j+1):length(lst))
    {
      
      first <- lst[j]
      
      second <- lst[k]
      
      if(data$convert[i] == 1)
      {
        pair_prob_convert[first,second] <- pair_prob_convert[first,second] + conv_value[i]
        
        pair_prob_convert[second,first] <- pair_prob_convert[second,first] + conv_value[i]
        
      }
      
      pair_prob_all[first,second] <- pair_prob_all[first,second] + conv_value[i]
      
      pair_prob_all[second,first] <- pair_prob_all[second,first] + conv_value[i]
      
      
    }
  }
  
}

second_prob <- pair_prob_convert/pair_prob_all

second_prob[is.nan(second_prob)] <- 0

contribution <- matrix(0,nrow(data_converted),length(table_total$Channel))

contribution <- data.frame(contribution)

names(contribution) <- as.character(table_total$Channel)

for(i in 1:nrow(data_converted))
{
  lst <- unlist(strsplit(p_converted[i],'>',fixed = TRUE))
  
  if(length(lst) == 1)
  {
    contribution[i,lst] <- 1
    
    next
  }
  
  N <- length(unique(lst)) - 1
  
  {
    if(N == 0)
    {
      
      contribution[i,unique(lst)] <- 1
      
      next
    }
  } 
  for(j in lst)
  {
    sum <- 0
    val <- first_prob[,j]
    for(k in lst)
    {
      if(k == j)
      {
        next
      }
      first <- second_prob[j,k]
      second <- first_prob[,k]
      temp <- first - val - second
      sum <- sum + temp
    }
    
    tmp <- 1/(2 * N)
    cnt <- val + (tmp * sum)
    {
      if(cnt < 0) 
      {
        cnt <- 1/1000 
      }
    }
    contribution[i,j] <- contribution[i,j] + cnt   
    
  }
}

final_contribution <- (contribution/(rowSums(contribution))) * conv_value_converted


channel_contribution <- colSums(final_contribution)
total <- sum(channel_contribution)
percent_channel_contribution <- sapply(channel_contribution, function(x) {x/total * 100})




write_out <- data.frame(channel_contribution)
final <- cbind(names(channel_contribution), write_out[,1])
final <- data.frame(final)
names(final) <- c("Placement points","Contribution")

write.csv(final, "Probablistical_model_DBS.csv", row.names = F)




