library(shiny)
library(shinythemes)
source("listfiles.R")
#setwd("C:/Users/user/Documents/Working - Copy") 
#source("maxcorrelation.R")
ui <- fluidPage(
  theme = shinytheme("yeti"),
  #theme = "bootstrap1.css",
  #the following UI must be in a sidebar panel
  titlePanel(
    h1("Stock Analysis", align = "left")),
  
  sidebarLayout(
    sidebarPanel(
      #headerPanel("Stocks"),
      uiOutput("stocknames",align="center"),
      actionButton("calculate","Calculate winning model"),
      actionButton("nifty","Compare with Nifty")
    ),
    mainPanel(
      #actionButton("calculate","Calculate"),
      #actionButton("plothemall","Plot Hemall"),
      #verbatimTextOutput("text"),
      #******************************************************************
      #textOutput("text"),
      #plotOutput("disp"),s
      #plotOutput("display"),
      
      htmlOutput("disp"),
      plotOutput("display",height="80px"),
      #htmlOutput("display"),
      textOutput("winner"),
      textOutput("nifty")
 #     textOutput("error")
   #   textOutput("cor")
    #  , plotOutput("alllinestrend")
      #p("POLYNOMIAL TRENDING - ")
      
    )
  ),
  
  #THE FOLLOWING THINGS ARE FOR RESEARCH PURPOSE ONLY

  #actionButton("calculate","calculate"),
  #verbatimTextOutput("text"),
  #verbatimTextOutput("winner"),
  
  #actionButton("plothemall","plothemall"),
  
  #THE FOLLOWING PLOT SHOULD COME IN TAB1
  #plotOutput("polytrend"),
  
  #THE FOLLOWING PLOT SHOULD COME IN TAB2
  #plotOutput("stltrend"),
  
  #THE FOLLOWING PLOT SHOULD COME IN TAB3
  #plotOutput("actualtrend"),
  
  #THE FOLLOWING PLOT SHOULD COME IN TAB4
  #plotOutput("alllinestrend"),

 #************************************************************** 
  tabsetPanel(position="left",
              tabPanel(title="Polynomial",
                       #value ="output$tab1",
                       plotOutput("polytrend")),
            tabPanel(title="Correlation"
                 #  htmlOutput("cor")
                       #value="tab2",
                       ),
              tabPanel(title="STL",
                       #value="tab2",
                       plotOutput("stltrend")),
              tabPanel(title="Actual",plotOutput("actualtrend")),
            #  tabPanel(title="All",plotOutput("alllinestrend")),
              #tabPanel(title="All", h1("check check check")), 
              
              id="tabs", selected = NULL
  )

)
