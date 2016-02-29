library(shiny)

shinyUI(fluidPage(
  titlePanel("Indexes by Year"),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Map", selectInput("select", label = h5(strong("Select Index"), style = 'color:darkgreen'),
                                  choices = list("Traffic Index", "Quality of Life Index",
                                                 "Pollution Index", "Health Care Index",
                                                 "Crime Index", "Rent Index", "Consumer Price Index",
                                                 "Groceries Index", "Restaurant Price Index"), 
                                  selected = "Quality of Life Index")), 
      
      sliderInput("slider", label = h5(strong("Year"), style = "color:blue"),
                  min = 2012, max = 2015, step = 1, value = 2015, sep = "") , plotOutput("map")), 
    tabPanel("Summary", verbatimTextOutput("summary")), 
    tabPanel("Table", tableOutput("table"))
  )))



















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
