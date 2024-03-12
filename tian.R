setwd("G:/wildfire")

library(mgcv)
require(splines, quietly=TRUE)
library(dplyr)
library(lubridate)
library(stringr)
library(tibble)
library(timeDate)

ans_out<-read.csv("data.csv")

holidays <- read.csv("2014-2017 节假日.csv")
ans_out <- ans_out %>% as_tibble()
holidays$calendar <- holidays$calendar %>% str_replace_all('/','-') %>% ymd()
ans_out$date <- ymd(ans_out$date)
ans_out <- ans_out %>% left_join(holidays, by=c("date"="calendar"))

attach(ans_out)

day<-weekdays(date)
day[day=='星期一']<-1
day[day=='星期二']<-2
day[day=='星期三']<-3
day[day=='星期四']<-4
day[day=='星期五']<-5
day[day=='星期六']<-6
day[day=='星期日']<-7

result0 <- gam(count ~ pm + ns(temp, 6) + day+ ns(calendar, 7*year1) + holidays + ns(humidity, 3),data=ans, family=quasipoisson(link="log"))
