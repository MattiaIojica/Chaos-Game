if (!require("shiny")) install.packages("shiny")
if (!require("shinyBS")) install.packages("shinyBS")

library(shiny)
library(shinyBS)

shinyUI(fluidPage(
   
  tags$title("Chaos Game"),

  h1("Chaos Game in 2 dimensiuni", align = "center", style = "font-size: 50px"),

  h4("Iojica Mattia, Rosca Alexandru",align = "center"),

  br(),

  fluidRow(
    column(4,
    wellPanel(


#dropbox figuri
    selectizeInput('shape', h5(tags$b('Figura')), 
        choices = c('Triunghi' = 'tri',
                     'Patrat' = 'sqr',
                     'Pentagon' = 'pent',
                     'Hexagon' = 'hex')
    ),


#triunghi
    conditionalPanel(
      condition = "input.shape=='tri'",
        sliderInput("dist.tri",
                    label = h5(tags$b("Distanta pana la urmatorul punct:")),
                    min = 0.01, max = .99, value = .50, step=.01),
      div(tags$b("Triunghi"), ": valoare default = 0.50 (1/2)",
          style = "font-size: 9.5pt;color:black",align="center")
      ),

#patrat
    conditionalPanel(
      condition = "input.shape=='sqr'",
        sliderInput("dist.sqr",
                    label = h5(tags$b("Distanta pana la urmatorul punct:")),
                    min = 0.01, max = .99, value = .67, step=.01),
      div(tags$b("Patrat"), ": valoare default = 0.67 (2/3)",
          style = "font-size: 9.5pt;color:black",align="center")
      ),

#pentagon
    conditionalPanel(
      condition = "input.shape=='pent'",
        sliderInput("dist.pent",
                    label = h5(tags$b("Distanta pana la urmatorul punct:")),
                    min = 0.01, max = .99, value = .63, step=.01),
      div(tags$b("Pentagon"), ": valoare default = 0.63 (5/8)",
          style = "font-size: 9.5pt;color:black",align="center")
      ),

#hexagon
    conditionalPanel(
      condition = "input.shape=='hex'",
        sliderInput("dist.hex",
                    label = h5(tags$b("Distanta pana la urmatorul punct:")),
                    min = 0.01, max = .99, value = .67, step=.01),
      div(tags$b("Hexagon"), ": valoare default = 0.67 (2/3)",
          style = "font-size: 9.5pt;color:black",align="center")
      ),

    br(),

    conditionalPanel(
      condition = NULL,
      textInput("time.between", "Timp intre iteratii (ms)", 500)
    ),
   
   #slide nr puncte pentru fiecare iteratie
    conditionalPanel(
      condition = NULL,
        sliderInput("skipped.points",
                    label = h5(tags$b("Numar puncte pentru fiecare pas:")),
                    min = 1, max = 100, value = 1, step = NULL)
      ),

    br(),

#text sub nr de puncte
    conditionalPanel(condition= NULL,
        div(uiOutput("my.app")),
        div("Apasa butonul ",span(HTML("&#9654"),
        style = "font-size:12pt;color:#999999;"), "pentru a anima",br(),

      span("Trage manual de punct la orice moment."),br(),
      span("Apasa",tags$b("Random"),"pentru a da o pozitie aleatore lui n."),
      align="center",style="font-size:8.5pt; color:teal")
    ),
    
    br(),
    
  #buton random
    div(bsButton("gen", label="Random"),align="right"),
    div("Apasa", tags$b("Random")," pentru un punct random de pornire",
        style = "font-size: 9.5pt;color:teal", align="right"),

    br(),
    ) #sidebarPanel
    ), #column-4

    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    column(8,
      tabsetPanel(type = "tabs",id = "tabselected",
        tabPanel("Chaos Game",value=1,
          fluidRow(
            column(12,
                   div(
                   div(plotOutput("initPlot"),style="width:500px",inline="TRUE"),align="center"),
             HTML("<hr style='height: 2px; color: #BDBDBD; background-color: #D9D9D9; border: none;'>")
            ), # column-12
            fluidRow(
              column(10, offset=1,
              p("
                Aceasta diagrama
                Aceasta secventa afiseaza pas cu pas progresia jocului.
                La fiecare pas se va marca punctul rosu ales aleatoriu.",br(),
                "Apasa pe butonul", span(HTML("&#9654"),style="font-size:10pt;color:#999999;")
                ," sau trage punctul din slide pentru a anima diagrama.",
                style="color:#0066CC"
                )
              ) # column-10
            ) # fluidRow
          ) # fluidRow
        ) # tabPanel
       )# tabsetPanel
    )# column-8
  ) # fluidRow
 )# fluidPage
)# shinyUI
