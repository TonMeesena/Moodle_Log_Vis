library(shiny)
library(ggplot2)
library(stringr)
library(dplyr)
library(scales)
library(lubridate)
library(htmlwidgets)
library(plotly)
library(quantmod)
library(tidyverse)

ui <- fluidPage(titlePanel(p("Moodle Track",style="color:#3474A7")),
                sidebarPanel(fileInput('datafile','Choose CSV file',accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv')),
                             dateRangeInput('daterange',"Date Range", start=Sys.Date()-30,end=Sys.Date()),
                             selectInput("timelen","Time Length",c("Day"=1,"Week"=7,"Month"=30)),
                             selectInput("student","Student name",""),
                             selectInput("event","Event context",""),
                             selectInput("act","Type of Action","")),
                mainPanel(plotlyOutput('plot')))

server <- function(input, output,session) {
  #Reset the data when the new data is being uploaded
  dataframe<-reactive({
    req(input$datafile) 
    
    data<-read.csv(input$datafile$datapath)
    
    #Time as date and as hour/minutes
    data$Time1<-gsub(",.*","",data$Time)
    data$Time2<-gsub(".*, ","",data$Time)
    
    data$Date<-as.Date(data$Time1, format="%d/%m/%y")
    data$Hour.Min<-as.Date(data$Time2, format="%H:%M")
    
    
    data$action<-word(data$Event.name,-1)
    data$action<-as.factor(data$action)
    
    
    #replace user full name with this user id 
    user.id<-as.numeric(gsub(".*?([0-9]+).*", "\\1", data$Description))
    
    data$Affected.user<-NULL
    
    data$User.full.name<-user.id
    
    
    return(data)
  })
  
  observe({
    updateSelectInput(session,"student",
                      choices=c("All", sort(as.character(unique(dataframe()$User.full.name)))))
  })
  
  
  #Set up the event context
  observe({
    updateSelectInput(session,"event",
                      choices=c("All", sort(as.character(unique(dataframe()$Event.context)))))
  })
  
  
  observe({
    student.name<-input$student
    updateSelectInput(session,"act",
                      choices=c("All","All Action in Different Curves", as.character(unique(dataframe()$action))))
    if (student.name!="All" & student.name!="All Action in Different Curves"){
      
      this.student<-as.character(unique( dataframe()[which(dataframe()$User.full.name==student.name),
                                                     'action'] ))
      updateSelectInput(session,"act",
                        choices=c("All", this.student ))
    }
    
  })
  
  
  
  
  
  
  output$plot<-renderPlotly({
    
    #scale
    timelength<-as.numeric(input$timelen)
    
    if(timelength==1){
      timelink<-"1 day"
    }else if(timelength==7){
      timelink<-"1 week"
    }else{
      timelink<-"1 month"
    }
    data<-dataframe()%>%filter(Date >=input$daterange[1]& Date<=input$daterange[2])
    
    
    #Filter student
    if (input$student!="All"){
      data.name<-data[which(data$User.full.name==input$student),]
    }else{
      data.name<-data
    }
    
    #Filter actions
    student.action<-input$act
    if (student.action!="All" & student.action!="All Action in Different Curves"){
      data.name<-data.name[which(data.name$action==student.action),]
    }
    
    
    #Filter event context 
    student.event <-input$event
    if (student.event!="All"){
      data.name<-data.name[which(data.name$Event.context==student.event),]
    }
    
    
    if (student.action=="All Action in Different Curves"){
      
      day.considered<-data.frame((table(cut(data.name$Date, 'day'))))
      day.considered<-data.frame(day.considered[,1])
      
      names(day.considered)<-c("Date")
      
      
      day.considered$Date<-as.Date(day.considered$Date)
      print(class(day.considered$Date))
      
      View(day.considered)
      
      
      
      
      all<-data.frame("Date"=c(), "Frequency"=c(),"Action"=c())
      
      student.action.unique<-unique(data.name$action[data.name$action!="All"&data.name$action!="All Action in Different Curves"])
      
      for (name.unique in student.action.unique){
        
        if(sum(data.name$action==name.unique)>0){
          
          tab <- table(cut(data.name$Date[which(data.name$action==name.unique)], 'day'))
          
          
          tab<-data.frame(Date=as.Date(names(tab)),
                          Frequency=as.vector(tab))
          
          tab<-left_join(day.considered,tab)
          
          
          
          tab$Frequency[which(is.na(tab$Frequency))]<-0
          
          tab$Action<-name.unique
          
          all<-rbind(all,tab)
          
          
        }
        
      }
      #,level=day.considered[,1]
      PlotPlot<-ggplot(data=all,aes(x=factor(Date), y=Frequency,color=factor(Action),group=factor(Action)) )+
        geom_line()+
        geom_point()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      
      
    }
    
    else{
      tab <- table(cut(data.name$Date, 'day'))
      
      ## Format
      aa<-data.frame(Date=format(as.Date(names(tab)), '%m/%d/%Y'),
                     Frequency=as.vector(tab))
      
      aa$groupplot<-1
      
      
      PlotPlot<-ggplot(data=aa,aes(x=factor(Date,level=aa$Date), y=Frequency,color=groupplot,group=groupplot)) +
        geom_line()+
        geom_point()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      
    }
    
    
    
    
    ggplotly(PlotPlot, dynamicTicks = TRUE)%>%    layout(xaxis = list(
      rangeslider = list(type = "date", thickness=0.3),
      rangeselector = list(
        buttons = list(list(step = "all", label = "All")))
    ))
    
    
    
  })
}

shinyApp(ui, server)




