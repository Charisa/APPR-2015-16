library(shiny)

shinyUI(fluidPage(
  titlePanel("Indexes by Year"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("select", label = h5(strong("Select Index"), style = 'color:darkgreen'),  
                  choices = list("Traffic Index" = 1, "Quality of Life Index" = 2,
                                 "Pollution Index" = 3, "Health Care Index" = 4,
                                 "Crime Index" = 5, "Rent Index" = 6, "Consumer Price Index" = 7,
                                 "Groceries Index" = 8, "Restaurant Price Index" = 9), selected = 2),
      sliderInput("slider1", label = h5(strong("Year"), style = "color:blue"),
                  min = 2012, max = 2015, step = 1, value = 2015)
      
    ),
  mainPanel(
    tabsetPanel(
      tabPanel("Map", plotOutput("map")), 
      tabPanel("Summary", verbatimTextOutput("summary")), 
      tabPanel("Table", tableOutput("table"))
    ))),
  
  
                    
  fluidRow(
    column(3, 
           numericInput("num", label = h5("Numeric Input"), value = 100)))
  
  
))



















#library(shiny)

#shinyUI(fluidPage(
  
 # titlePanel("Slovenske občine"),
  
#  tabsetPanel(
 #     tabPanel("Velikost družine",
  #             DT::dataTableOutput("druzine")),
      
   #   tabPanel("Število naselij",
    #           sidebarPanel(
     #             uiOutput("pokrajine")
      #          ),
       #        mainPanel(plotOutput("naselja"))),
      
  #    tabPanel("Zemljevid",
   #            plotOutput("zemljevid")),
      
    #  tabPanel("Število naselij in površina",
     #          plotOutput("povrsina"))
    #)
#))
