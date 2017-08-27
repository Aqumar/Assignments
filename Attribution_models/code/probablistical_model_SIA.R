library(data.table)

library(stringr)

setwd("C:/Users/aqmhussa/Desktop")

data <- fread('tcp.csv', sep = ',', header = TRUE)
data <- data.frame(data)

convert <- sample(c(0,1), replace = TRUE, size = nrow(data))
data$convert <- convert


names(data)[1] <- 'path'
names(data)[4] <- 'revenue'

p <- data$path
p <- as.character(p)
p <- str_replace_all(p, fixed(" "), "")


conv_value <- data$revenue
conv_value <- gsub('SGD','',conv_value)
conv_value <- gsub(',','',conv_value)
conv_value <- as.numeric(conv_value)

conversion <- data$Conversions
conversion <- gsub(',','',conversion)
conversion <- as.numeric(conversion)


data_converted <- subset(data,convert == 1)

p_converted <- data_converted$path
p_converted <- as.character(p_converted)
p_converted <- str_replace_all(p_converted, fixed(" "), "")


conv_value_converted <- data_converted$revenue
conv_value_converted <- gsub('SGD','',conv_value_converted)
conv_value_converted <- gsub(',','',conv_value_converted)
conv_value_converted <- as.numeric(conv_value_converted)


conversion_converted <- data_converted$Conversions
conversion_converted <- gsub(',','',conversion_converted)
conversion_converted <- as.numeric(conversion_converted)







#####Calculating the occurence of unique placement points#######

points_total <- NULL
for(i in 1:nrow(data))
{
  lst <- unlist(strsplit(p[i],'>',fixed = TRUE))
  lst_unique <- unique(lst)
  points_total <- rbind(points_total,data.frame(lst_unique))
  
}

table_total <- table(points_total)    ##Calculates occurence of all placement points

table_total <- data.frame(table_total)

names(table_total) <- c('Channel','Occurence')


####Calculating first order prob

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
      first_convert[1,j] <- first_convert[1,j] + conversion[i]
    }
    first_all[1,j] <- first_all[1,j] + conversion[i]
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
        pair_prob_convert[first,second] <- pair_prob_convert[first,second] + conversion[i]
        
        pair_prob_convert[second,first] <- pair_prob_convert[second,first] + conversion[i]
        
      }
      
      pair_prob_all[first,second] <- pair_prob_all[first,second] + conversion[i]
      
      pair_prob_all[second,first] <- pair_prob_all[second,first] + conversion[i]
      
      
    }
  }
  
}

second_prob <- pair_prob_convert/pair_prob_all







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

