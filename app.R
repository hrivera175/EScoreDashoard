library(shiny)
library(shinydashboard)
library(shinyapps)
library(ggplot2)
library(data.table)
library(rCharts)
library(googleVis)
library(shinyjs)
options(RCHART_LIB = 'nvd3')

dat <- read.csv('atd3 copy.csv',header=T)


dat4 <- dat[,c(1,3,22)]
dat4$e_score <- dat4$e_score*100
dat4$diff <- (100 - dat4$e_score)
dat5 <- melt(dat4, id=c('email','date'))
dat5 <- dat5[order(dat5$email,dat5$date,dat5$variable),]
dat5$ymin[dat5$variable=='e_score'] <- 0
dat5 <- setDT(dat5)[dat5$variable=='diff', ymin:=1-value, by=list(email,date)]

dat5 <- setDT(dat5)[dat5$variable=='e_score', ymax := value, by=list(email,date)]
dat5 <- setDT(dat5)[dat5$variable=='diff', ymax:=value+ymin, by=list(email,date)]
dat5 <- data.frame(dat5)

dat5 <- subset(dat5, email =='211sandiego.org' | email =='211tampabay.org' |email == 'ltech.com' |email == 'evergage.com')
dat5 <- setDT(dat5)[ variable == 'e_score', ymin:=0,by=list(email,date,variable)]
dat5 <- setDT(dat5)[ variable=='diff' , ymin:= dat5$value[1] - dat5$ymin[1],by=list(email,date)]
dat5$date <- as.character(dat5$date)

dat2 <- subset(dat, email =='211sandiego.org' | email =='211tampabay.org'| email =='ltech.com' | email =='evergage.com')
dat2$date <- as.character(as.Date(dat2$date,'%Y-%m-%d'))
dat2$email <- as.character(dat2$email)
dat7 <- data.frame(dat5)



##############################################
######     SETTING UP THE LOGIN PAGE    ######
##############################################
Logged <- FALSE;
LoginPass <- 0; #0: not attempted, -1: failed, 1: passed

login <- box(title = "Login",textInput("userName", "Username (user)"),
             passwordInput("passwd", "Password (test)"),
             br(),actionButton("Login", "Log in"))

loginfail <- box(title = "Login",textInput("userName", "Username"),
                 passwordInput("passwd", "Password"),
                 p("Username or password incorrect"),
                 br(),actionButton("Login", "Log in"))




header  <-     dashboardHeader(title="Engagement Score")
sidebar <-     dashboardSidebar(uiOutput("secondSelection"))
sidebar2 <-    dashboardSidebar()
body     <-    dashboardBody(uiOutput('body'))

mainbody <-    div(fluidRow(
                           
      tabBox(
        title = " ",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "800px", width='400px',
        tabPanel("AE & Customer Selection",
                 br(), "Select an AE to view their Customer's Engagement Score",
                 value=1, br(),
                 selectInput('Select2', 'AE', choices = c('Daniel','Jake','Aaron','Steven','Chris','Noah')),
                 selectInput('Select', 'Customer:', choices = unique(as.character(dat5$email))),
                 #uiOutput('thirdSelection'),
                 br()
                ),
        tabPanel("Customer Dashboard", " ", value = 2,
                 
    fluidRow(
      box(title='Engagement Score', 'text', width=4, height=450, showOutput("distPlot",lib= 'nvd3')),
      #box(htmlOutput("distPlot")),
      box(plotOutput('overtime'),width=8, height = 450),
    fluidRow(
       valueBoxOutput("approvalBox"),
       valueBoxOutput("approvalBox2"),
       valueBoxOutput("approvalBox3"),
       valueBoxOutput("approvalBox4"),
       valueBoxOutput("approvalBox5")
            )
          )
       )
    )))

ui <-dashboardPage(header,sidebar,body)

server <- shinyServer(function(input, output,session) {
  
  USER <<- reactiveValues(Logged = Logged, LoginPass = LoginPass)
  observe({
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          username <- isolate(input$userName)
          password <- isolate(input$passwd)
          #Id.username <- which(my_username == Username)
          if (username == "user" & password == "test") {
            USER$Logged <<- TRUE
            USER$LoginPass <<- 1
          }
          USER$LoginPass <<- -1
        }
      }
    }
  })
  
  output$body <- renderUI({
    if (USER$Logged == TRUE) {
      mainbody
    }
    else {
      if(USER$LoginPass >= 0) {
        login
      }
      else {
        loginfail
      }
    }
  })

  
  output$secondSelection <- renderUI({
    if (!is.null(input$tabset1) && input$tabset1 == 2) {
      selectInput("User", "Date:", choices = unique(as.character(dat5[dat5$email==input$Select,date])))
      
      }
  })

  #output$thirdSelection <- renderUI({
    #selectInput("Select", "Customer:", choices = unique(as.character(dat5[dat5$owner==input$Select2,email])))
  #})
  
  
  output$distPlot <- renderChart({
    dat1 <- subset(dat5, (email %in% input$Select & date %in% input$User))
    p5 <- nPlot(value ~ variable, data = dat1 , type = 'pieChart')
    p5$chart(donut = TRUE, color=c('#33FF00','#CCCCCC'), margin=list(left=10,right = 10),width = 320,height=300,showLegend=F)
    p5$set(dom = 'distPlot')
    return(p5)
      }) 
  
  #output$distPlot <- renderGvis({
    #dat7 <- subset(dat7, (email %in% input$Select & date %in% input$User))
    #return(gvisPieChart(data=dat7,options=list(width=500,height=500,title='Engagement Score',legend='none',colors="['green', 'red']",pieSliceText='label',pieHole=0.5),chartid="doughnut"))
    #plot(d)
    #})
  
  output$overtime <- renderPlot({
    ggplot(data = subset(dat5, variable== 'e_score' & email %in% input$Select & date <= input$User), aes(date,value,group = email)) +
      geom_line() +
      geom_point() +
      ggtitle(paste('Engagement Score Over Time up until:', input$User, sep=' ')) +
      xlab("Date") +
      ylab("Engagement Score")
    
  })
  
  output$approvalBox <- renderValueBox({
    valueBox(
      if((paste0(subset(dat2, dat2$email==input$Select & dat2$date == input$User,select=c('c_diff')),' in Hrs') == "numeric(0) in Hrs")
      
        |(paste0(subset(dat2, dat2$email==input$Select & dat2$date == input$User,select=c('c_diff')),' in Hrs')=="NA in Hrs")) 
      {
      paste0('No Activity')
      } else {
      paste0(subset(dat2, dat2$email==input$Select & dat2$date == input$User,select=c('c_diff')),' in Hrs') }
      ,"Your Avg Response Time",
      icon=icon('clock-o'), color='blue') })
  
  output$approvalBox2 <- renderValueBox({
    valueBox(
      if((paste0(subset(dat2, dat2$email==input$Select & dat2$date == input$User,select=c('nc_diff')),' in Hrs') == "numeric(0) in Hrs")
         
         |(paste0(subset(dat2, dat2$email==input$Select & dat2$date == input$User,select=c('nc_diff')),' in Hrs')=="NA in Hrs")) 
      {
        paste0('No Activity')
      } else {
        paste0(subset(dat2, dat2$email==input$Select & dat2$date == input$User,select=c('nc_diff')),' in Hrs') }
      ,"Customer's Avg Response Time",
      icon=icon('clock-o'), color='blue') })
  
  output$approvalBox3 <- renderValueBox({
    valueBox(
      if((paste0(subset(dat2, dat2$email==input$Select & dat2$date == input$User,select=c('ratio')),' in Hrs') == "numeric(0) in Hrs")
         
         |(paste0(subset(dat2, dat2$email==input$Select & dat2$date == input$User,select=c('ratio')),' in Hrs')=="NA in Hrs")) 
      {
        paste0('No Activity')
      } else {
        paste0(subset(dat2, dat2$email==input$Select & dat2$date == input$User,select=c('ratio')),'') }
      ,"Reply Ratio",
      icon=icon('refresh'), color='blue')
  })
  
  output$approvalBox4 <- renderValueBox({
    valueBox(
      if((paste0(subset(dat2, dat2$email==input$Select & dat2$date == input$User,select=c('s_cirrus')),' in Hrs') == "integer(0) in Hrs")
         
         |(paste0(subset(dat2, dat2$email==input$Select & dat2$date == input$User,select=c('s_cirrus')),' in Hrs')=="NA in Hrs")) 
      {
        paste0('No Activity')
      } else {
        paste0(subset(dat2, dat2$email==input$Select & dat2$date == input$User,select=c('s_cirrus')),' email(s)') }
      ,"Your Gross Email Volume",
      icon=icon('envelope'), color='blue')
  })
  
  output$approvalBox5 <- renderValueBox({
    valueBox(
      if((paste0(subset(dat2, dat2$email==input$Select & dat2$date == input$User,select=c('s_noncirrus')),' in Hrs') == "integer(0) in Hrs")
         
         |(paste0(subset(dat2, dat2$email==input$Select & dat2$date == input$User,select=c('s_noncirrus')),' in Hrs')=="NA in Hrs")) 
      {
        paste0('No Activity')
      } else {
        paste0(subset(dat2, dat2$email==input$Select & dat2$date == input$User,select=c('s_noncirrus')),' email(s)') }
      ,"Customer's Gross Email Volume",
      icon=icon('envelope'), color='blue')
  })
  
  
})
  shinyApp(ui = ui, server = server)
  