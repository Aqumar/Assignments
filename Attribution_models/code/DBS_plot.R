setwd('C:/Users/aqmhussa/Desktop')

library(ggplot2)

last_click <- read.csv('last_click_DBS.csv')

agr <- aggregate(last_click$Contribution,by = list(last_click$Channel),FUN = sum)

names(agr) <- c('Channel','Contribution')

final_aggr_last <- agr[with(agr,order(-Contribution)), ]

final_aggr_last$percent_contribution <- (final_aggr_last$Contribution/sum(final_aggr_last$Contribution)) * 100

write.csv(final_aggr_last,'last_click_DBS_byChannel.csv',row.names = FALSE)



weighted <-read.csv('weighted_model_DBS.csv')

agr <- aggregate(weighted$Contribution,by = list(weighted$Channel),FUN = sum)

names(agr) <- c('Channel','Contribution')

final_aggr_weighted <- agr[with(agr,order(-Contribution)), ]

final_aggr_weighted$percent_contribution <- (final_aggr_weighted$Contribution/sum(final_aggr_weighted$Contribution)) * 100

write.csv(final_aggr_weighted,'weighted_model_DBS_byChannel.csv',row.names = FALSE)






markov <-read.csv('markov_model_DBS.csv')

agr <- aggregate(markov$Contribution,by = list(weighted$Channel),FUN = sum)

names(agr) <- c('Channel','Contribution')

final_aggr_weighted <- agr[with(agr,order(-Contribution)), ]

final_aggr_weighted$percent_contribution <- (final_aggr_weighted$Contribution/sum(final_aggr_weighted$Contribution)) * 100

write.csv(final_aggr_weighted,'markov_model_DBS_byChannel.csv',row.names = FALSE)





prob_model <-read.csv('Probablistical_model_DBS.csv')

agr <- aggregate(prob_model$Contribution,by = list(prob_model$Channel),FUN = sum)

names(agr) <- c('Channel','Contribution')

final_aggr_prob_model <- agr[with(agr,order(-Contribution)), ]

final_aggr_prob_model$percent_contribution <- (final_aggr_prob_model$Contribution/sum(final_aggr_prob_model$Contribution)) * 100

write.csv(final_aggr_prob_model,'Probablistical_model_DBS_byChannel.csv',row.names = FALSE)












bp<- ggplot(final_aggr_prob_model, aes(x="", y=Contribution, fill=Channel))+
  geom_bar(width = 1, stat = "identity")


pie <- bp + coord_polar("y", start=0) 


