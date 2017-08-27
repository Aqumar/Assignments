library(stringr)

setwd('C:/Users/aqmhussa/Desktop')

TR <- read.csv('mapping_TR.csv')

markov <- read.csv('markov_model_DBS.csv')

TR_selected <- TR[,c('Placement','Channel')]

TR_selected$Placement <- str_replace_all(TR_selected$Placement, fixed(" "), "")

m <- merge(markov,TR_selected,by.x = 'Placement.points', by.y = 'Placement', all.x = TRUE)

write.csv(m,'markov_model_DBS.csv', row.names = FALSE)
