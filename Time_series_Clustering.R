

dm = read.csv('C:/Users/gvykr/Desktop/dm_project/train/train.csv')

for (i in unique(dm$Store))
{
  for (j in unique(dm$Dept)){
    if(nrow(dm[dm$Store==i & dm$Dept==j,])!=143){
      dm<-dm[!(dm$Store==i & dm$Dept==j),]
    }
  }
}


dm[dm$IsHoliday == 'TRUE','IsHoliday']= 1

dm[dm$IsHoliday == 'FALSE','IsHoliday']= 0

write.csv(dm,file = 'C:/Users/gvykr/Desktop/dm_project/train/train_new.csv')

#########################################Store and department wise analysis###########################
############################################Reshaping the data########################################

dm_reshaped_sample<- dm[dm$Date =='2010-02-05',c('Store','Dept')]

for (i in unique(dm$Date))
{
  dm_reshaped_sample[,i] = dm[dm$Date ==i,c('Weekly_Sales')]
}

##Giving Unique ID for each of the Time series

dm_reshaped_sample[,"U_ID"] <-paste ( 'S',dm_reshaped_sample$Store,'D',dm_reshaped_sample$Dept ,sep = "", collapse = NULL)

dm_reshaped_sample[,"Store"] = NULL

dm_reshaped_sample[,"Dept"] = NULL


sample(1:100, 10)

###Time series clustering

require(graphics)

distMatrix <- dist(dm_reshaped_sample[,-which(names(dm_reshaped_sample) %in% c("U_ID"))], method="euclidean")

### hierarchical clustering
hc <- hclust(distMatrix, method="average")

plot(hc, labels=dm_reshaped_sample$U_ID, main="")



n <- 40
s <- sample(1:2660, n)

sample2 <- dm_reshaped_sample[s,]

distMatrix <- dist(sample2[,-which(names(sample2) %in% c("U_ID"))], method="euclidean")

### hierarchical clustering
hc <- hclust(distMatrix, method="average")

plot(hc, labels=sample2$U_ID, main="",col = 'White')

source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")

op = par(bg = "gray15")
cols = hsv(c(0.2, 0.4,0.6,0.8,1), 1, 1, 0.8)
A2Rplot(hc, k = 5, boxes = FALSE, col.up = "gray50", col.down = cols)

######################################Store wise data#################################

View(dm)


dm_store_wise <- data.frame(aggregate(dm$Weekly_Sales,by = list(Category = dm$Store,dm$Date),FUN = mean))

View(dm_reshaped_store_wise)

#########Reshaping the data


dm_reshaped_store_wise<- data.frame(dm_store_wise[dm_store_wise$Date =='2010-02-05',c('Store')])

names(dm_store_wise)= c("Store","Date","Weekly_Sales")

for (i in unique(dm_store_wise$Date))
{
  dm_reshaped_store_wise[,toString(i)] = dm_store_wise[dm_store_wise$Date ==toString(i),c('Weekly_Sales')]
}


dm_reshaped_store_wise$Store <- dm_reshaped_store_wise$dm_store_wise.dm_store_wise.Date.....2010.02.05...c..Store...

dm_reshaped_store_wise <- dm_reshaped_store_wise[,-which(names(dm_reshaped_store_wise) %in% c("dm_store_wise.dm_store_wise.Date.....2010.02.05...c..Store..."))]




########Clustering



distMatrix_store <- dist(dm_reshaped_store_wise[,-which(names(dm_reshaped_store_wise) %in% c("Store"))], method="euclidean")

### hierarchical clustering
hc_store <- hclust(distMatrix_store, method="average")

plot(hc_store, labels=dm_reshaped_store_wise$Store, main="",col = 'White')

source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")

op = par(bg = "gray15")
cols = hsv(c(0.2, 0.4,0.6,0.8,1), 1, 1, 0.8)
A2Rplot(hc, k = 5, boxes = FALSE, col.up = "gray50", col.down = cols)



############Writing into csv file

write.csv(dm_store_wise,file = 'C:/Users/gvykr/Desktop/dm_project/train/dm_store_wise.csv')