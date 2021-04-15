#library(usethis) 
#use_git_config(user.name = "DataScienceProjectsJapan", user.email = "wclaster@apu.ac.jp")
#install.packages("anytime)

library(tidyverse)
library(ggplot2)
library(RcppRoll)
library(anytime)

library(readr)
#prefectures <- read_csv("prefectures.csv")
prefectures <- read.csv(url("https://toyokeizai.net/sp/visual/tko/covid19/csv/prefectures.csv"))
#View(prefectures)
#Data from
#https://toyokeizai.net/sp/visual/tko/covid19/
totalPos <- prefectures %>% group_by(prefectureNameE) %>% summarise(Posi=sum(testedPositive)) %>% arrange(desc(Posi))


#does this work on tibble? Yes
totalPos$Posi
#|prefectureNameE=="Osaka"|prefectureNameE=="Fukuoka"|prefectureNameE=="Hokaido"
TokyoCases <- prefectures  %>% filter(prefectureNameE=="Tokyo")%>% select("testedPositive", "year","month","date")  
#Filtering with multiple values
#https://blog.exploratory.io/filter-data-with-dplyr-76cf5f1a258e
#top5=c("Tokyo","Osaka","Fukuoka","Sendai","Hokkaido")
top5=c("Tokyo","Osaka")
top5Cases <- prefectures%>%group_by(prefectureNameE)   %>%  filter(prefectureNameE %in% top5)%>%ungroup() %>% select("prefectureNameE","testedPositive", "year","month","date") 


#To get the vector you have to extract
TokyoCases["testedPositive"]
#or
TokyoCases$testedPositive
top5Cases$Date <- as.Date(paste(top5Cases$year,top5Cases$month,top5Cases$date,sep = "-"))


top5Casesgrouped <-  top5Cases %>% group_by(prefectureNameE) %>% select(prefectureNameE,testedPositive,Date) %>% arrange(prefectureNameE,Date)



TokyoCasesDaily <- TokyoCases$testedPositive[2:length(TokyoCases$testedPositive)]-TokyoCases$testedPositive[1:(length(TokyoCases$testedPositive)-1)]


top5newcases <- top5Casesgrouped$testedPositive[2:length(top5Casesgrouped$testedPositive)]-top5Casesgrouped$testedPositive[1:(length(top5Casesgrouped$testedPositive)-1)]


top5Casesgrouped$newCases <- c(0,top5newcases)

#remove the rows that switch from one prefecture to the next.
top5Casesgrouped <- top5Casesgrouped[top5Casesgrouped$newCases>=0,]


#Graphs
ggplot(as.data.frame(TokyoCasesDaily), aes(x=1:length(TokyoCasesDaily),y=TokyoCasesDaily))+geom_line()


#convert to date.
ISOdatetime(prefectures$year[1],prefectures$month[1],prefectures$date[1],hour = 12, min = 0, sec = 0, tz = "GMT")


#another way
#The easiest way to make a date in R is to use as.Date(x) where x is some date in yyyy-mm-dd format. For example,

dateString <- paste(TokyoCases$year[1],TokyoCases$month[1],TokyoCases$date[1],sep = "-")
as.Date(dateString) #seems to work even though input may not have 2 digits for month and day.
TokyoCases$Date <- as.Date(paste(TokyoCases$year,TokyoCases$month,TokyoCases$date,sep = "-"))
TokyoCases$TokyoCasesDaily <- c(0,TokyoCasesDaily)



TokyoCases %>%ggplot(aes(x = Date[2:length(TokyoCases$Date)], y=TokyoCasesDaily)) +geom_line() +labs(title = "Quantity Sold: Month Plot", x = "", y = "Sales",subtitle = "March through July tend to be most active") #+scale_y_continuous() #+theme_tq()

ggplot(TokyoCases, aes(x = Date, y=TokyoCasesDaily, color="blue")) +geom_point() +labs(title = "Tokyo daily new cases", x = "", y = "New cases",subtitle = "From Feb. 2020") +scale_x_date(date_breaks = "2 month", date_labels =  "%b %Y") #+scale_y_continuous() #+theme_tq()

ggplot(TokyoCases, aes(color="",x = Date, y=TokyoCasesDaily)) +geom_line() +labs(title = "Tokyo daily new cases", x = "", y = "New cases",subtitle = "From Feb. 2020") +scale_x_date(date_breaks = "2 month", date_labels =  "%b %Y") #+scale_y_continuous() #+theme_tq()
#plot(TokyoCases$Date,TokyoCases$testedPositive)  

#Moving Average
#See: https://blog.exploratory.io/introducing-time-series-analysis-with-dplyr-60683587cf8a



TokyoCases <- TokyoCases%>%mutate(moving_average = roll_mean(TokyoCasesDaily, 7, align="right", fill=0))

ggplot(TokyoCases, aes(x = Date, y=TokyoCasesDaily)) +geom_line(aes(),size=1)+geom_line(data = TokyoCases, aes(Date, moving_average), color="orange",size=2) +labs(title = "Tokyo daily new cases", x = "", y = "New cases",subtitle = "From Feb. 2020") +scale_x_date(date_breaks = "2 month", date_labels =  "%b %Y")

#View(TokyoCases)
#View(top5Casesgrouped)

ggplot(TokyoCases, aes(x = Date, y=TokyoCasesDaily)) + geom_point() + geom_smooth(span = 0.2)+labs(title = "Tokyo daily new cases", x = "", y = "New cases",subtitle = "7-day smoothing in blue")
library(forecast)
library(viridis)
#See some color options here
#https://www.r-graph-gallery.com/ggplot2-color.html
#+geom_forecast()
ggplot(top5Casesgrouped, aes(x = Date, y=newCases, colour = prefectureNameE)) + geom_point(size=.1)+ geom_smooth(aes(group = prefectureNameE),span = 0.2) +labs(title = "Japan Cities", x = "", y = "New cases",subtitle = "smoothed trend")+ylim(0,1800) +scale_color_manual(values=c("#69b3a2", "blue1", "brown2","black", "purple"))
  #scale_color_brewer(palette = "Paired") #scale_color_viridis(discrete=TRUE, option="plasma")
library(ts)
t <- as.ts(top5Casesgrouped)

oNew <- top5Casesgrouped %>% filter(prefectureNameE=="Osaka")

oNewCases <- oNew %>% select(newCases)
t <- as.ts(oNewCases$newCases)
autoplot(t)
ft <- forecast(t)
autoplot(ft)

Jdata$Date <- anytime::anydate(Jdata$Date)
ggplot(Jdata, aes(x = Date, y=newDeaths)) + geom_point() + geom_smooth(span = 0.2)+labs(title = "Japan daily new deaths", x = "", y = "New deaths",subtitle = "7-day smoothing in blue")
