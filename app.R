library(readr)
library(dplyr)
library(stringr)
library(tidyverse)
library(shiny)
library(magrittr)

setwd("C:/Users/sea00/Desktop/proj")

#외국인유학생
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
for3[for3==""]<-0
for3[is.na(for3)] <- 0
for3<- gather(foreigner, "체류자격", "인원수", 5:41)
for3 <- for3 %>% filter(국적!="총계")
for3$성별[for3$성별 =="총계"]<- "총"
for3$인원수<-readr::parse_number(for3$인원수)
for3<- for3%>% filter(인원수!=0)

#등록외국인
regidence<- data.table::fread("등록외국인.csv", skip=1, encoding="UTF-8")
regidence<- regidence[1:768,]
regidence$시군구[regidence$시도 =="세종특별자치시"&regidence$시군구!="총계"]<- "세종특별자치시"
regidence[regidence==""]<-0
regidence[is.na(regidence)] <- 0
reg<-gather(regidence, "국적", "인원수", 5:196)
reg<-reg%>%filter(시군구!="총계")
reg$성별[reg$성별=="총계"]<- "총"
reg$인원수<-readr::parse_number(reg$인원수 )
reg$총합계<-readr::parse_number(reg$총합계 )
reg<- reg%>% filter(인원수!=0)


#####

ui <- navbarPage("한국 속의 세계",
                 
                 tabPanel("체류외국인",
                          titlePanel("체류외국인 분포"),
                          br(),
                          br(),
                          
                          fluidRow(
                            column(5,
                                   selectInput("sex1",
                                               label="성별 선택",
                                               choices= c(sort(unique(for3$성별))),
                                               selected="총")),
                            column(5,
                                   selectInput("status",
                                               label="체류자격 선택",
                                               choices= c(sort(unique(for3$체류자격))),
                                               selected="B1(사증면제)")),
                            
                            column(8,
                                   plotOutput("chart5"))),
                          
                          br(),
                          br(),
                          
                          fluidRow(
                            column(5,
                                   selectInput("sex2",
                                               label="성별 선택",
                                               choices= c(sort(unique(for3$성별))),
                                               selected="총")),
                            column(5,
                                   selectInput("country2",
                                               label="국적 선택",
                                               choices= c(sort(unique(for3$국적))),
                                               selected="한국계중국인")),
                            
                            column(8,
                                   plotOutput("chart6"))),
                          
                          br(),
                          br()
                          
                          ),
                 
                 tabPanel("등록외국인",
                          titlePanel("등록외국인 분포"),
                          br(),
                          br(),
                          
                          fluidRow(
                            column(5,
                                   selectInput("sex3",
                                               label="성별 선택",
                                               choices= c(sort(unique(reg$성별))),
                                               selected="총")),
                            column(5,
                                   selectInput("region",
                                               label="시군구 선택",
                                               choices= c(sort(unique(reg$시군구))),
                                               selected="강릉시")),
                            
                            column(8,
                                   plotOutput("chart7"))),
                          
                          br(),
                          br(),
                          
                          fluidRow(
                            column(5,
                                   selectInput("sex4",
                                               label="성별 선택",
                                               choices= c(sort(unique(reg$성별))),
                                               selected="총")),
                            column(5,
                                   selectInput("country3",
                                               label="국적 선택",
                                               choices= c(sort(unique(reg$국적))),
                                               selected="중국")),
                            
                            column(8,
                                   plotOutput("chart8"))),
                          
                          br(),
                          br()
                          ),
                 
                 tabPanel("외국인유학생",
                          titlePanel("외국인 유학생 분포"),
                          br(),
                          br(),
                          
                          fluidRow(
                            column(5,
                                   selectInput("country",
                                               label="국적 선택",
                                               choices= c(sort(unique(inter$국적))),
                                               selected="가나")),
                            
                            column(8,
                                   plotOutput("chart1"))),
                          
                          br(),
                          br(),
                          
                          fluidRow(
                            column(5,
                                   selectInput("degree",
                                               label="체류자격 선택",
                                               choices= c(sort(unique(inter$체류자격))),
                                               selected="학사과정")),
                            
                            
                            column(8,
                                   plotOutput("chart2"))),
                          
                          br(),
                          br(),
                          
                          fluidRow(
                            column(5,
                                   selectInput("school",
                                               label="학교 선택",
                                               choices= c(sort(unique(inter$학교))),
                                               selected="성균관대학교")),
                            
                            column(8,
                                   plotOutput("chart3"))),
                          
                          br(),
                          br(),
                          
                          fluidRow(
                            column(5,
                                   selectInput("place",
                                               label="시도 선택",
                                               choices= c(sort(unique(inter$시도))),
                                               selected="서울특별시")),
                            
                            
                            column(8,
                                   plotOutput("chart4"))),
                          
                          br(),
                          br()
                          
                          
                 )
                          )


server<- function(input, output, session){
  
  countryInput<- reactive({
    slice(
      arrange(
        summarise(group_by(
          filter(inter,국적==input$country),
          학교),
          count=n()),
        -count),
      (1:10))
  })
  
  degreeInput<- reactive({
    slice(
      arrange(
        summarise(group_by(
          filter(inter,체류자격==input$degree),
          학교),
          count=n()),
        -count),
      (1:10))
  })
  
  schoolInput<- reactive({
    slice(
      arrange(
        summarise(group_by(
          filter(inter,학교==input$school),
          국적),
          count=n()),
        -count),
      (1:10))
  })
  
  placeInput<- reactive({
    slice(
      arrange(
        summarise(group_by(
          filter(inter,시도==input$place),
          국적),
          count=n()),
        -count),
      (1:10))
  })
  
  
  output$chart1 <- renderPlot({
    
    ggplot(data=countryInput(), aes(x=학교, y=count))+
      geom_bar(stat="identity")+
      theme_classic()+
      labs(title=paste(input$country,"유학생들이 가장 많은 학교는?"), x="학교", y="인원수")+
      theme(plot.title=element_text(size=24, face="bold",hjust=0.5),
            axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=15),
            axis.text.x=element_text(angle=30))+
      geom_col(fill="lightpink")+
      geom_text(aes(label=학교))+
      geom_text(aes(label=count),vjust=2)
    
  })
  
  output$chart2 <- renderPlot({
    
    ggplot(data=degreeInput(), aes(x=학교, y=count))+
      geom_bar(stat="identity")+
      theme_classic()+
      labs(title=paste(input$degree,"유학생들이 가장 많은 학교는?"), x="학교", y="인원수")+
      theme(plot.title=element_text(size=24, face="bold",hjust=0.5),
            axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=15),
            axis.text.x=element_text(angle=30))+
      geom_col(fill="lightgoldenrod")+
      geom_text(aes(label=학교))+
      geom_text(aes(label=count), vjust=2)
    
  })
  
  output$chart3 <- renderPlot({
    
    ggplot(data=schoolInput(), aes(x=국적, y=count))+
      geom_bar(stat="identity")+
      theme_classic()+
      labs(title=paste(input$school,"에 유학 온 학생들의 국적은?"), x="국적", y="인원수")+
      theme(plot.title=element_text(size=24, face="bold",hjust=0.5),
            axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=15),
            axis.text.x=element_blank())+
      geom_col(fill="lightgreen")+
      geom_text(aes(label=국적))+
      geom_text(aes(label=count), vjust=2)
    
  })
  
  output$chart4 <- renderPlot({
    
    ggplot(data=placeInput(), aes(x=국적, y=count))+
      geom_bar(stat="identity")+
      theme_classic()+
      labs(title=paste(input$place,"에 있는 유학생들의 국적은?"), x="국적", y="인원수")+
      theme(plot.title=element_text(size=24, face="bold",hjust=0.5),
            axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=15),
            axis.text.x=element_blank())+
      geom_col(fill="lightblue")+
      geom_text(aes(label=국적))+
      geom_text(aes(label=count), vjust=2)
  

  })
  
  country2Input<- reactive({
    slice(arrange(filter(for3, 성별==input$sex1, 체류자격==input$status),desc(인원수)),1:10)
  })
  
  output$chart5 <- renderPlot({
    
    ggplot(data=country2Input(), aes(x=국적, y=인원수))+
      geom_bar(stat="identity")+
      theme_classic()+
      labs(title=paste(input$status,"자격으로 체류하는",input$sex1,"외국인의 출신국가는?"), x="국적", y="인원수")+
      theme(plot.title=element_text(size=24, face="bold",hjust=0.5),
            axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=15),
            axis.text.x=element_blank())+
      geom_col(fill="lightpink")+
      geom_text(aes(label=국적))+
      geom_text(aes(label=인원수), vjust=2)
  })
  
  statusInput<- reactive({
    slice(arrange(filter(for3, 성별==input$sex2, 국적==input$country2),desc(인원수)),1:10)
  })
  
  output$chart6 <- renderPlot({
    
    ggplot(data=statusInput(), aes(x=체류자격, y=인원수))+
      geom_bar(stat="identity")+
      theme_classic()+
      labs(title=paste(input$country2,"에서 온",input$sex2,"외국인의 체류목적은?"), x="체류자격", y="인원수")+
      theme(plot.title=element_text(size=24, face="bold",hjust=0.5),
            axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=15),
            axis.text.x=element_blank())+
      geom_col(fill="lightgoldenrod")+
      geom_text(aes(label=체류자격))+
      geom_text(aes(label=인원수), vjust=2)   
  })
  
  country3Input<- reactive({
    slice(arrange(filter(reg, 성별==input$sex3, 시군구==input$region),desc(인원수)),1:10)
  })
  
  output$chart7 <- renderPlot({
    
    ggplot(data=country3Input(), aes(x=국적, y=인원수))+
      geom_bar(stat="identity")+
      theme_classic()+
      labs(title=paste(input$region,"에 거주하는",input$sex3,"외국인의 출신국가는?"), x="국적", y="인원수")+
      theme(plot.title=element_text(size=24, face="bold",hjust=0.5),
            axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=15),
            axis.text.x=element_blank())+
      geom_col(fill="lightgreen")+
      geom_text(aes(label=국적))+
      geom_text(aes(label=인원수), vjust=2)
  })
  
  regionInput<- reactive({
    slice(arrange(filter(reg, 성별==input$sex4, 국적==input$country3),desc(인원수)),1:10)
  })
  
  output$chart8 <- renderPlot({
    
    ggplot(data=regionInput(), aes(x=시군구, y=인원수))+
      geom_bar(stat="identity")+
      theme_classic()+
      labs(title=paste(input$country3,"에서 온",input$sex4,"외국인이 가장 많이 사는 시군구는?"), x="시군구", y="인원수")+
      theme(plot.title=element_text(size=24, face="bold",hjust=0.5),
            axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=15),
            axis.text.x=element_blank())+
      geom_col(fill="lightblue")+
      geom_text(aes(label=시군구))+
      geom_text(aes(label=인원수), vjust=2)
  })
  
  
}

shinyApp(ui, server)

