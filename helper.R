setwd("C:/Users/sea00/Desktop/3-1/data_Management/project/data")
knitr::opts_chunk$set(echo=TRUE)

library(readr)
library(dplyr)
library(stringr)
library(tidyverse)
library(shiny)
library(magrittr)

#유학생
inter <-read.csv("법무부_유학생관리정보 데이터_20201231.csv")
place <- unite(inter, "체류지", c(체류지.시도, 체류지.시군구), sep=" ")
place<- subset(place, select=c(순번, 체류지))
inter<- merge(inter, place, by="순번")
inter<-mutate(inter, 나이=2021-생년+1)
inter<- subset(inter, select=-c(순번, 체류지.시군구, 생년))
rm(place)
inter<- inter%>% rename("국적"="국적명","학교"="학교명","시도"="체류지.시도")

#체류외국인
foreigner <-data.table::fread("체류외국인.csv",skip=2,encoding="UTF-8")
foreigner<- subset(foreigner, select=(-c(42:46)))
foreigner$총합계 <-readr::parse_number(foreigner$'총합계')

for3<- gather(foreigner, "체류자격", "인원수", 5:41)
for3 <- for3 %>% filter(국적!="총계")
for3$성별[for3$성별 =="총계"]<- "총"
for3$인원수<-readr::parse_number(for3$인원수)
for3[for3==""]<-0
for3[is.na(for3)] <- 0
for3<- for3%>% filter(인원수!=0)  
str(for3)

#등록외국인

regidence<- data.table::fread("등록외국인.csv", skip=1, encoding="UTF-8")
regidence<- regidence[1:768,]
regidence$시군구[regidence$시도 =="세종특별자치시"&regidence$시군구!="총계"]<- "세종특별자치시"
regidence[regidence==""]<-0
regidence[is.na(regidence)] <- 0
reg<- gather(regidence, "국적", "인원수", 5:196)
reg <- reg%>%filter(시군구!="총계")
reg$인원수<-readr::parse_number(reg$인원수 )
reg$총합계<-readr::parse_number(reg$총합계 )
reg<- reg%>% filter(인원수!=0)
reg$성별[reg$성별=="총계"]<- "총"
