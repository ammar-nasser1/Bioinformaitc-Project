cs<- read.csv("Breast Cancer.csv",header = TRUE)
head(cs)
boxplot(cs$area_mean,cs$area_worst)


t.test(x=cs$area_mean,y=cs$area_worst,alternative = "two.sided",mu =  0,var.equal = FALSE , conf.level = .95,paired = T)

library("ggplot2")
library("caTools")
library("corrplot")
library("dplyr")

View(head(cs))
glimpse(cs)

str(cs)

summary(cs)

data1<-cs[-33]
summary(data1)
data1 %>% count(diagnosis)

data1 %>% count(diagnosis)%>%group_by(diagnosis) %>%
  summarize(perc_dx = round((n / 569)* 100, 2))

diagnosis.table <- table(data1$diagnosis)
colors <- terrain.colors(2) 
# Create a pie chart 
diagnosis.prop.table <- prop.table(diagnosis.table)*100
diagnosis.prop.df <- as.data.frame(diagnosis.prop.table)
pielabels <- sprintf("%s - %3.1f%s", diagnosis.prop.df[,1], diagnosis.prop.table, "%")
pie(diagnosis.prop.table,
    labels=pielabels,  
    clockwise=TRUE,
    col=colors,
    border="gainsboro",
    radius=0.8,
    cex=0.8, 
    main="frequency of cancer diagnosis")
legend(1, .4, legend=diagnosis.prop.df[,1], cex = 0.7, fill = colors)

c <- cor(data1[,3:31])
corrplot(c, order = "hclust", tl.cex = 0.7)

ggplot(data=data1,aes(x=diagnosis,y=radius_mean,fill="pink"))+geom_boxplot()+ggtitle("radius of Benign Vs Malignant")

ggplot(data=data1,aes(x=diagnosis,y=area_mean))+geom_boxplot()+ggtitle("area of Benign Vs Malignant")

ggplot(data=data1,aes(x=diagnosis,y=concavity_mean))+geom_boxplot()+ggtitle("concavity of  Benign Vs Malignant")

ggplot(data1,aes(x=diagnosis,fill=texture_mean))+geom_bar()+ggtitle("women affected in benign and malingnant stage")

sel_data=data1[data1$radius_mean>10&
                data1$radius_mean<15&
                data1$compactness_mean>0.1,]
ggplot(sel_data,aes(x=diagnosis,y=radius_mean,fill=diagnosis))+geom_col()+ggtitle("womens affected in higher levels based on mean")

ggplot(data1,aes(x=texture_mean,fill=as.factor(diagnosis)))+geom_density(alpha=0.4)+ggtitle(" texture mean  for benign vs malignant")

ggplot(data1,aes(x=as.factor(diagnosis),y=perimeter_mean))+geom_violin()+ggtitle(" perimeter mean  for benign vs malignant")

data2=data1%>%filter(concavity_mean>0.2)
ggplot(data2,aes(x=concavity_mean,y=diagnosis,size=perimeter_se))+geom_point()+ggtitle("concavity mean  for benign vs malignant")

ggplot(data1, aes(x = area_se>15, fill = diagnosis)) +geom_bar(position = "fill")+ggtitle("area se for benign vs malignant")

ggplot(data1, aes(x = texture_se)) +
  geom_histogram(binwidth=10) +
  facet_wrap(~ diagnosis)+ggtitle(" texture se  for benign vs malignant")

ggplot(data1, aes(x = perimeter_mean)) +
  geom_histogram(binwidth=10) +
  facet_wrap(~ diagnosis)+ggtitle(" perimeter mean  for benign vs malignant")

ggplot(data1, aes(x = area_mean)) +
  geom_histogram(binwidth=10) +
  facet_wrap(~ diagnosis)+ggtitle(" area mean  for benign vs malignant")

ggplot(data1, aes(x = smoothness_mean)) +
  geom_histogram(binwidth=10) +
  facet_wrap(~ diagnosis)+ggtitle(" smoothness_mean  for benign vs malignant")

d <- dist(data1[, 1:4])
fitH <- hclust(d, "ward.D2")
plot(fitH)
rect.hclust(fitH, k = 3, border = "red") 
clusters <- cutree(fitH, k = 3) 
plot(iris, col = clusters)

library(mclust)
fitM <- Mclust(data1)
plot(fitM)
M <- c("Mar","Apr","May","Jun","Jul")

barplot(data1$area_mean,xlab="area_mean",ylab="worst_area",col="blue", main="area_mean", border="red")

barplot(data1$radius_mean,xlab="radius_mean",ylab="radius_worst",col="blue", main="radius_mean", border="red")
barplot(data1$texture_mean,xlab="texture_mean",ylab="texture_worst",col="blue", main="texture_mean", border="red")
barplot(data1$perimeter_mean,xlab="perimeter_mean",ylab="perimeter_worst",col="blue", main="perimeter_mean", border="red")
barplot(data1$smoothness_mean,xlab="smoothness_mean",ylab="smoothness_worst",col="blue", main="smoothness_mean", border="red")
