library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(modeest)

data<- read.csv("C:/Users/kkonar2/Downloads/archive/netflix_titles.csv",na.strings = c("","NA"))

colSums(is.na(data))

df<-na.omit(data)
df1<- df %>%
  separate(date_added,c("Month"))
df1<-df1[!is.na(df1$Month),]
df1$Month[df1$Month==""]<-mfv(df1$Month)

df1$Month<-factor(df1$Month,levels = month.name)

df2<-separate_rows(df1,country,show_id , convert = TRUE, sep = ', ')
country_count<-sort(table(df2$country),decreasing=TRUE)[1:10]
country_count<-data.frame(country_count)

g= ggplot(data=df1, aes(x = Month,fill=type))
g = g + geom_bar(fill = "Red", color = "Black")
g=g+ylab("Number of Movies/TV Shows")+ggtitle("Count of Netflix content released in terms of Month")
g

g= ggplot(data=country_count, aes(x = Var1, y=Freq))
g = g + geom_bar(stat = 'Identity',fill = "Red", color = "Black")
g = g + xlab("Country")+ylab("Number of Movies/TV Shows")+ggtitle("Top 10 Countries streaming the most Netflix Content")
g
