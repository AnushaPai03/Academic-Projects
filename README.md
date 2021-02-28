#WINE data analysis

#installing packages related text mining and wordcloud
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(rtweet)
library(tm)
library(dplyr)
library(ggplot2)
library(caTools)
library(caret)
library(corrplot)
library(rrr)
library(datasets)
library()

#reading the csv file to generate wordcloud
df<-read.csv("winemag-data_first150k.csv")
set.seed(1234)
summary(df)
str(df)

#Combining 4 csv files to one single dataset

Red<-read.csv("R final dataset/Red.csv")
Rose<-read.csv("R final dataset/Rose.csv")
White<-read.csv("R final dataset/White.csv")
Sparkling<-read.csv("R final dataset/Sparkling.csv")
wine<-rbind(Red,Rose,White,Sparkling)
str(wine)    
summary(wine)

#Performing Exploratory Data Analysis( Data Cleaning, column renaming, removing NANs)
wine$Year<-as.integer(wine$Year) #converting year from char to int  
sum(is.na(wine$Year)) #744 of 13834 values are NA, <5% of data
wine<-na.omit(wine)#removing NA values
names(wine)[1]="Name" #Rename first field from "i..Name" to "Name"
wine$Winery<-as.factor(wine$Winery)
wine <- as.data.frame(wine)
glimpse(wine)

#correlation plot between different variables
GGally::ggcorr(wine)
cor(wine$Price,wine$Rating)#Positive correlation value
plot(wine$Rating,wine$Price,main="Corelation plot",xlab = "Rating",ylab = "Price",pch=19,frame=FALSE)
abline(lm(Price~Rating,data=wine),col="blue")
cor(wine$Price,wine$Year)#Negative correlation value
plot(wine$Year,wine$Price,main="Corelation plot",xlab = "Year",ylab = "Price",pch=19,frame=FALSE)
abline(lm(Price~Year,data=wine),col="blue")

#constructing a word cloud from more than 150,000 reviews 
review <- df$description #creating a vector to store reviews from df data frame
docs<-Corpus(VectorSource(review))#creating a document corpus
inspect(docs)
writeLines(as.character(docs[20]))#to view one of the document created, in this case 20th position
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords,c(stopwords("en"),"wine"))
#text cleaning to remove punctuation, new lines, spaces etc
gsub("https\\S*", "", review) 
gsub("@\\S*", "", review) 
gsub("amp", "", review) 
gsub("[\r\n]", "", review)
gsub("[[:punct:]]", "", review)
wordcloud(words = docs, min.freq = 500,max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

# countries producing top 5 wines
wine_country <- group_by(wine, Country) %>%
  summarize(Count= n()) %>%
  filter(Count>0) %>%
  arrange(desc(Count))

#plot of the count of wine produced in all countries
ggplot(wine_country, aes(x= reorder(Country,-Count),Count)) +
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))+
  xlab("Country of production")+ylab("Count of wine")
#To see top 5
Top<-head(wine_country,5)

#Frequency of wine Rating
hist(wine$Rating, main="Wine Rating", breaks = 5, col="steelblue")

#Country wise average wine rating
ggplot(wine, aes(x= reorder(Country,-Rating),Rating)) +
  geom_boxplot(color="black",outlier.colour ="black",fill="blue")+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))+
  xlab("Country of production")+ylab("Rating")

#Best wineries in France
wine%>%filter(wine$Country=="France")%>%arrange(desc(Region))%>%slice(1:24)%>%
  ggplot()+
  geom_bar(mapping = aes(x=Region),fill="steelblue")+
  labs(x="Country:France",y="Rating", title="Wineries per region")+
  coord_flip()+
  theme_minimal()

#Year wise average wine rating
filter(wine$Country=="France")%>%ggplot(wine, aes(x= reorder(Year,-Rating),Rating)) +
  geom_boxplot(color="blue",outlier.colour ="black",fill="green")+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust = 1))+
  xlab("Year of production")+ylab("Rating")

#Year vs Price for different winestyles
ggplot(wine, aes(x=Rating, y=Price)) + geom_point(aes(col=WineType))+
  geom_smooth(method="loess", se=F) + labs(y="Price", x="Rating", title="Rating Vs Price")
ggplot(wine, aes(x=Year, y=Price)) + geom_point(aes(col=WineType))+
  geom_smooth(method="lm", se=F) + labs(y="Price", x="Year", title="Year Vs Price")+
  scale_y_log10(breaks=c(1,2,3,4,5,10,100,1000,3000))

#topwines in france
france_wine=wine%>%filter(wine$Country=="France"& wine$Rating>4.5)%>%group_by(Rating)%>%arrange(desc(Price))
View(france_wine)

#Building a price prediction model from input/independent variables
set.seed(123)
sample = sample.split(wine,SplitRatio = 0.70)
train =subset(wine,sample ==TRUE) # creates a training dataset named train with rows which are marked as TRUE
test=subset(wine, sample==FALSE)
model <- lm(Price~Rating+Year,data=train) #NumberOfRatings is not very significant
summary(model)
coef(model)
par(mfrow=c(2,2)) #2x2 layout
plot(model)
confint(model)
result<-predict(model,test,na.action = na.pass)

#Analysis of Variance for input categorical variable
anvmodel<-aov(Price~Winery+Region,data=train)
anova(anvmodel)
par(mfrow=c(1,2))
plot(test$Rating,test$Price)
abline(lm(Price~Rating,data=test))
plot(test$Year,test$Price)
abline(lm(Price~Year,data=test))

