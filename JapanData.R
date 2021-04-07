library(tidyverse)
library(ggplot2)
library(RcppRoll)

library(readr)
prefectures <- read_csv("prefectures.csv")
View(prefectures)
#Data from
#https://toyokeizai.net/sp/visual/tko/covid19/
totalPos <- prefectures %>% group_by(prefectureNameE) %>% summarise(Posi=sum(testedPositive)) %>% arrange(desc(Posi))

glimpse(totalPos)
str(totalPos)
#does this work on tibble? Yes
totalPos$Posi
#|prefectureNameE=="Osaka"|prefectureNameE=="Fukuoka"|prefectureNameE=="Hokaido"
TokyoCases <- prefectures  %>% filter(prefectureNameE=="Tokyo")%>% select("testedPositive", "year","month","date")  
View(TokyoCases)
str(TokyoCases)
str(as.vector(TokyoCases))
#To get the vector you have to extract
TokyoCases["testedPositive"]
#or
TokyoCases$testedPositive
TokyoCasesDaily <- TokyoCases$testedPositive[2:length(TokyoCases$testedPositive)]-TokyoCases$testedPositive[1:(length(TokyoCases$testedPositive)-1)]
TokyoCasesDaily

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

View(TokyoCases)


ggplot(TokyoCases, aes(x = Date, y=TokyoCasesDaily)) + geom_point() + geom_smooth(span = 0.2)+labs(title = "Tokyo daily new cases", x = "", y = "New cases",subtitle = "7-day smoothing in blue")
