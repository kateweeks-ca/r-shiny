#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#import necessary libraries
library(readr)
library(shiny)
library(ggplot2)
library(shinythemes)
library(DT)

lifeSpan <- read_tsv('allcountries.tsv')
lifeSpan <- data.frame(lifeSpan)


ui <-shinyUI(fluidPage(theme = shinytheme("yeti"),
  pageWithSidebar(
  headerPanel("A Life Span App"),
  sidebarPanel(
  selectInput("continent","Choose a continent", choices = c("All",unique(lifeSpan$continent))),
  selectInput("country","Choose an country", choices = c("All",unique(lifeSpan$country))),
  selectInput("yaxis", "Choose a y variable", choices = colnames(lifeSpan[,3:6])),
  selectInput("xaxis", "Choose a x variable", choices = colnames(lifeSpan[,3:6])),
  selectInput("color", "Choose a color variable", choices = colnames(lifeSpan[,1:6])),
  selectInput("size", "Choose a size variable", choices = c('pop', 'lifeExp', 'gdpPercap')),
  actionButton("goButton", "Update")
  ),
  mainPanel(
  tabsetPanel(
  tabPanel('Plot', plotOutput("plot1")),
  tabPanel('Appendix',DT::dataTableOutput("table1"))
  ))
)
))


server <- shinyServer(function(input,output, session){
  
  data1 <- reactive({
    
    if(input$continent == "All"){
      
      lifeSpan
    }else{
      
      lifeSpan[which(lifeSpan$continent == input$continent),]
    }
  })
  
  data2 <- eventReactive(input$goButton,{
    
    if (input$country == "All"){
      
      lifeSpan

      
    }else{
      
      lifeSpan[which(lifeSpan$country == input$country),]

      
      
    }
  })
  
  observe({
    
    
    if(input$continent != "All"){
      
      updateSelectInput(session,"country","Choose a country", choices = c("All",unique(data1()$country)))

      
    }
    
    else if(input$country != 'All'){
      
      updateSelectInput(session,"continent","Choose a continent", choices = c('All',unique(data2()$continent)))

      
    }
    
    else if (input$continent == "All" & input$country == "All"){
      
      updateSelectInput(session,"country","Choose a country", choices = c('All',unique(lifeSpan$country)))
      updateSelectInput(session,"continent","Choose a continent", choices = c('All',unique(lifeSpan$continent)))

      
    }
  })
  
  
  data3 <- eventReactive( input$goButton,{
    req(input$goButton)
    req(input$goButton)
    if(input$country == "All"){
      
      data1()

      
    }else if (input$continent == "All"){
      
      data2()

      
    }else if (input$country == "All" & input$continent == "All"){
      
      lifeSpan

    }
    else{
      
      lifeSpan[which(lifeSpan$country== input$country & lifeSpan$continent == input$continent),]

      
      
      }
  })
  
tuna<- eventReactive(input$goButton, {
    input$xaxis
    
  })
 
reno <- eventReactive(input$goButton,{
  input$yaxis
  })


henry <- eventReactive(input$goButton,{
  input$color
})

claudia <- eventReactive(input$goButton,{
  input$size
})
  
  output$table1 <- DT::renderDataTable({
    req(input$goButton)
    data3()
  })
  
  

  output$plot1 <- renderPlot({
    x <- tuna()
    y <- reno()
    z <- henry()
    zz <- claudia()
    myTitle <- 'xvar vs yvar'
    myTitle <- gsub('xvar', x, myTitle)
    myTitle <- gsub('yvar', y, myTitle)
    p <- ggplot(data3(),aes(x=data3()[,x], y=data3()[,y], color=data3()[,z], size=data3()[,zz])) + geom_point()
    p + labs(x = tuna(), y = reno(), title=myTitle, color=henry(),size=claudia()) + theme(plot.title = element_text(hjust = 0.5, size=20))
    
    
    
  })

 
})





shinyApp(ui,server)