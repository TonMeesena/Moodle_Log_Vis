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
library(DT)

ui <- fluidPage(titlePanel(p("Moodle Track",style="color:#3474A7")),
                sidebarPanel(fileInput('datafile','Choose CSV file',accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv')),
                             dateRangeInput('daterange',"Date Range", start=Sys.Date()-30,end=Sys.Date()),
                             selectInput("timelen","Time Length",c("Day"="day","Within a Day(Hour)"="hour")),
                             selectInput("student","Student name",""),
                             selectInput("event","Event context",""),
                             selectInput("act","Type of Action",""),
                            selectInput("sep","Seperate the Data By",c("None"="none","Student"="student", "Event"="event" ,"Action"="action"))),
                mainPanel(plotlyOutput('plot',height = "800px"),DT::dataTableOutput("mytable")))

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
    
    data$Hour<-as.POSIXct(data$Time2, format="%H:%M")
    
    
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
    
    #Input student name
    student.name<-input$student
    
    
    #Update the choices by all unique event names
    updateSelectInput(session,"event",
                      choices=c("All", sort(as.character(unique(dataframe()$Event.context)))))
    
    # If we are looking at one student
    if (student.name!="All" ){
      
      # Pick only events associated with this particular student
      this.student<-as.character(unique( dataframe()[which(dataframe()$User.full.name==student.name),
                                                     'Event.context'] ))
      
      #Update the event select input by having "All" and the events  associated with this student
      updateSelectInput(session,"event",
                        choices=c("All", this.student ))
      
    }
  })
  
  
  observe({
    student.name<-input$student
    student.event<-input$event
    updateSelectInput(session,"act",
                      choices=c("All", as.character(unique(dataframe()$action))))
    if (student.name!="All" ){
      
      
      
      
      if (student.event!="All" ){
        
        this.student<-as.character(unique( dataframe()[which(dataframe()$User.full.name==student.name && dataframe()$Event.context==student.event),
                                                       'action'] ))
        updateSelectInput(session,"act",
                          choices=c("All", this.student ))
      
     
      }else{
        this.student<-as.character(unique( dataframe()[which(dataframe()$User.full.name==student.name),
                                                       'action'] ))
        updateSelectInput(session,"act",
                          choices=c("All", this.student ))
        
        
      }
      
    }
    
  })
  
  
  
  
  
  dataframe2<-reactive({

    #scale
    data<-dataframe()%>%filter(Date >=input$daterange[1]& Date<=input$daterange[2])
    

    #Filter student
    if (input$student!="All"){
      data.name<-data[which(data$User.full.name==input$student),]
    }else{
      data.name<-data
    }
    
    #Filter actions
    student.action<-input$act
    if (student.action!="All" ){
      data.name<-data.name[which(data.name$action==student.action),]
    }
    
    
    #Filter event context 
    student.event <-input$event
    if (student.event!="All"){
      data.name<-data.name[which(data.name$Event.context==student.event),]
    }
    
    return(data.name)
    
  })
  
  
  
  
  
  
  output$plot<-renderPlotly({
    
    data.name<-dataframe2()
    
    #Do not plot if there is no data
    req(nrow(data.name) > 0)
    
    
    sep_by<-input$sep
    student.action<-input$act
    
    if(input$timelen=="day"){
    if (sep_by=="action"){
      
      
      day.considered<-data.frame((table(cut(data.name$Date, 'day'))))
      day.considered<-data.frame(day.considered[,1])
      
      names(day.considered)<-c("Date")
      
      day.considered$Date<-as.Date(day.considered$Date)
    
      all<-data.frame("Date"=c(), "Frequency"=c(),"Action"=c())
      
      student.action.unique<-unique(data.name$action[data.name$action!="All"])
      
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
      
      all$Date<-as.Date(all$Date, '%m/%d/%Y')
      
      PlotPlot<-ggplot(data=all,aes(x=Date, y=Frequency,color=factor(Action),group=factor(Action)) )+
        geom_line()+
        geom_point()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
    }
    else if(sep_by=="event"){
      
      day.considered<-data.frame((table(cut(data.name$Date, 'day'))))
      day.considered<-data.frame(day.considered[,1])
      
      names(day.considered)<-c("Date")
      
      day.considered$Date<-as.Date(day.considered$Date)
      
      all<-data.frame("Date"=c(), "Frequency"=c(),"Event"=c())
      
      student.event.unique<-unique(data.name$Event.context[data.name$Event.context!="All"])
      
      for (name.unique in student.event.unique){
        
        if(sum(data.name$Event.context==name.unique)>0){
          
          tab <- table(cut(data.name$Date[which(data.name$Event.context==name.unique)], 'day'))
          
          tab<-data.frame(Date=as.Date(names(tab)),
                          Frequency=as.vector(tab))
          
          tab<-left_join(day.considered,tab)
          
          
          tab$Frequency[which(is.na(tab$Frequency))]<-0
          
          tab$Event<-name.unique
          
          all<-rbind(all,tab)
        }
        
      }
      all$Date<-as.Date(all$Date, '%m/%d/%Y')
      
      PlotPlot<-ggplot(data=all,aes(x=Date, y=Frequency,color=factor(Event),group=factor(Event)) )+
        geom_line()+
        geom_point()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      
    }
    
    else if(sep_by=="student"){
      
      
      day.considered<-data.frame((table(cut(data.name$Date, 'day'))))
      day.considered<-data.frame(day.considered[,1])
      
      names(day.considered)<-c("Date")
      
      day.considered$Date<-as.Date(day.considered$Date)
      
      all<-data.frame("Date"=c(), "Frequency"=c(),"Student"=c())
      
      student.student.unique<-unique(data.name$User.full.name[data.name$User.full.name!="All"])
      
      for (name.unique in student.student.unique){
        
        if(sum(data.name$User.full.name==name.unique)>0){
          
          tab <- table(cut(data.name$Date[which(data.name$User.full.name==name.unique)], 'day'))
          
          tab<-data.frame(Date=as.Date(names(tab)),
                          Frequency=as.vector(tab))
          
          tab<-left_join(day.considered,tab)
          
          
          tab$Frequency[which(is.na(tab$Frequency))]<-0
          
          tab$Student<-name.unique
          
          all<-rbind(all,tab)
        }
        
      }
      all$Date<-as.Date(all$Date, '%m/%d/%Y')
      
      PlotPlot<-ggplot(data=all,aes(x=Date, y=Frequency,color=factor(Student),group=factor(Student)) )+
        geom_line()+
        geom_point()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      
    }
    
    else{
      
      tab <- table(cut(data.name$Date, 'day'))
      
      ## Format
      all<-data.frame(Date=format(as.Date(names(tab)), '%m/%d/%Y'),
                     Frequency=as.vector(tab))
      
      all$groupplot<-"All"
      all$Date<-as.Date(all$Date, '%m/%d/%Y')
   
      PlotPlot<-ggplot(data=all,aes(x=Date, y=Frequency,color=groupplot,group=groupplot)) +
        geom_line()+
        geom_point()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      
    }
    
    }
    else{
      
      
      if (sep_by=="action"){
        
        
        day.considered<-data.frame(table(cut(data.name$Hour, 'hour')))
        day.considered<-data.frame(day.considered[,1])
        
        names(day.considered)<-c("Date")
        day.considered$Date<-format(day.considered$Date, format="%H%M%S")
        
        
        all<-data.frame("Date"=c(), "Frequency"=c(),"Action"=c())
        
        student.action.unique<-unique(data.name$action[data.name$action!="All"])
        
        for (name.unique in student.action.unique){
          
          if(sum(data.name$action==name.unique)>0){
            
            tab <- table(cut(data.name$Hour[which(data.name$action==name.unique)], 'hour'))
            
            tab<-data.frame(Date=names(tab),
                            Frequency=as.vector(tab))
            
            tab$Date<-format(tab$Date, format="%H%M%S")
            
            
            
            tab<-left_join(day.considered,tab)
            
            
            tab$Frequency[which(is.na(tab$Frequency))]<-0
            
            tab$Action<-name.unique
            
            all<-rbind(all,tab)
          }
          
        }
        
        all$Date<-format(all$Date, format="%H%M%S")
        
        PlotPlot<-ggplot(data=all,aes(x=Date, y=Frequency,color=factor(Action),group=factor(Action)) )+
          geom_line()+
          geom_point()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        
      }
      else if(sep_by=="event"){
        
        day.considered<-data.frame(table(cut(data.name$Hour, 'hour')))
        day.considered<-data.frame(day.considered[,1])
        
        names(day.considered)<-c("Date")
        day.considered$Date<-format(day.considered$Date, format="%H%M%S")
        
        all<-data.frame("Date"=c(), "Frequency"=c(),"Event"=c())
        
        student.event.unique<-unique(data.name$Event.context[data.name$Event.context!="All"])
        
        for (name.unique in student.event.unique){
          
          if(sum(data.name$Event.context==name.unique)>0){
            
            tab <- table(cut(data.name$Hour[which(data.name$Event.context==name.unique)], 'hour'))
            
            tab<-data.frame(Date=names(tab),
                            Frequency=as.vector(tab))
            
            tab$Date<-format(tab$Date, format="%H%M%S")
            
            
            
            tab<-left_join(day.considered,tab)
            
            
            tab$Frequency[which(is.na(tab$Frequency))]<-0
            
            tab$Event<-name.unique
            
            all<-rbind(all,tab)
          }
          
        }
        all$Date<-format(all$Date, format="%H%M%S")
        
        PlotPlot<-ggplot(data=all,aes(x=Date, y=Frequency,color=factor(Event),group=factor(Event)) )+
          geom_line()+
          geom_point()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        
      }
      
      else if(sep_by=="student"){
        
        
        day.considered<-data.frame(table(cut(data.name$Hour, 'hour')))
        day.considered<-data.frame(day.considered[,1])
        
        names(day.considered)<-c("Date")
        day.considered$Date<-format(day.considered$Date, format="%H%M%S")
    
        
        all<-data.frame("Date"=c(), "Frequency"=c(),"Student"=c())
        
        student.student.unique<-unique(data.name$User.full.name[data.name$User.full.name!="All"])
        
   
        
        for (name.unique in student.student.unique){
          
          if(sum(data.name$User.full.name==name.unique)>0){
            
            tab <- table(cut(data.name$Hour[which(data.name$User.full.name==name.unique)], 'hour'))
            
            tab<-data.frame(Date=names(tab),
                            Frequency=as.vector(tab))
            
            tab$Date<-format(tab$Date, format="%H%M%S")
            
            
          
            tab<-left_join(day.considered,tab)
            
            
            tab$Frequency[which(is.na(tab$Frequency))]<-0
            
            tab$Student<-name.unique
            
            all<-rbind(all,tab)
     
          }
          
        }
        all$Date<-format(all$Date, format="%H%M%S")
        
        PlotPlot<-ggplot(data=all,aes(x=Date, y=Frequency,color=factor(Student),group=factor(Student)) )+
          geom_line()+
          geom_point()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        
      }
      
      else{
        
        all<-data.frame(table(cut(data.name$Hour, 'hour')))
        names(all)<-c('Date','Frequency')
        
        
        all$groupplot<-"All"
        all$Date<-format(all$Date, format="%H%M%S")
        
        
        PlotPlot<-ggplot(data=all,aes(x=Date, y=Frequency,color=groupplot,group=groupplot)) +
          geom_line()+
          geom_point()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        
        
      }
      
    


      
    }
    
    
    
    ggplotly(PlotPlot, dynamicTicks = TRUE)   %>%    layout(height = 600,xaxis = list(
      rangeslider = list(type = "date", thickness=0.3),
      rangeselector = list(
        buttons = list(list(step = "all", label = "All")))
    ))
    
  
    
    
  })
  
  
  
  
  
  
  
  #Plot table 
  output$mytable = DT::renderDataTable({
    dataframe2()[,1:5]
  })
  
}

shinyApp(ui, server)




