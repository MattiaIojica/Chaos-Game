if (!require("shiny")) install.packages("shiny")
if (!require("shinyBS")) install.packages("shinyBS")
if (!require("shape")) install.packages("shape")

library(shiny)
library(shinyBS)

shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        overflow-x: hidden;
      }
      ")
      )
  ),

  tags$title("Chaos Game"),

  h1("Chaos Game in 2 dimensiuni", align = "center", style = "font-size: 50px"),

  h4("Iojica Mattia, Rosca Alexandru",align = "center"),

  # br(),

  fluidRow(
    column(4,
    wellPanel(

    #dropbox forme
    selectizeInput(inputId = 'shape', h5(tags$b('Forma')), 
        choices = c('Triunghi' = 'tri',
                     'Patrat' = 'sqr',
                     'Pentagon' = 'pent',
                     'Hexagon' = 'hex')
    ),
    hr(),


    #triunghi
    conditionalPanel(
      condition = "input.shape=='tri'",
        sliderInput(inputId = "dist.tri",
                    label = h5(tags$b("Distanta pana la urmatorul punct:")),
                    min = 0.01, max = .99, value = .50, step=.01),
      div(tags$b("Triunghi"), ": valoare default = 0.50 (1/2)",
          style = "font-size: 9.5pt;color:black",align="center")
      ),

    #patrat
    conditionalPanel(
      condition = "input.shape=='sqr'",
        sliderInput(inputId = "dist.sqr",
                    label = h5(tags$b("Distanta pana la urmatorul punct:")),
                    min = 0.01, max = .99, value = .67, step=.01),
      div(tags$b("Patrat"), ": valoare default = 0.67 (2/3)",
          style = "font-size: 9.5pt;color:black",align="center")
      ),

    #pentagon
    conditionalPanel(
      condition = "input.shape=='pent'",
        sliderInput(inputId = "dist.pent",
                    label = h5(tags$b("Distanta pana la urmatorul punct:")),
                    min = 0.01, max = .99, value = .63, step=.01),
      div(tags$b("Pentagon"), ": valoare default = 0.63 (5/8)",
          style = "font-size: 9.5pt;color:black",align="center")
      ),

    #hexagon
    conditionalPanel(
      condition = "input.shape=='hex'",
        sliderInput(inputId = "dist.hex",
                    label = h5(tags$b("Distanta pana la urmatorul punct:")),
                    min = 0.01, max = .99, value = .67, step=.01),
      div(tags$b("Hexagon"), ": valoare default = 0.67 (2/3)",
          style = "font-size: 9.5pt;color:black",align="center")
      ),

    # br(),
    hr(),

    conditionalPanel(
      condition = NULL,
      textInput(inputId = "time.between", "Timp intre iteratii (ms)", 500)
    ),
    hr(),

    #slide nr puncte pentru fiecare iteratie
    conditionalPanel(
      condition = NULL,
        sliderInput(inputId = "skipped.points",
                    label = h5(tags$b("Numar puncte adaugate dupa fiecare pas:")),
                    min = 1, max = 100, value = 1, step = NULL)
      ),

    # br(),
    hr(),

    #text sub nr de puncte
    conditionalPanel(condition= NULL,
        div(uiOutput("my.app")),
        div("Apasa butonul ",span(HTML("&#9654"),
        style = "font-size:12pt;color:#999999;"), "pentru a anima",br(),

      span("Trage manual de punct la orice moment."),br(),
      span("Apasa",tags$b("Random"),"pentru a da o pozitie aleatore lui n."),
      align="center",style="font-size:10pt; color:#22283a")
    ),
    
    br(),
    
    #buton random
    div(bsButton(inputId = "gen", label="Random"),align="right"),

    )
    ),


    column(8,
      tabsetPanel(type = "tabs",id = "tabselected",
        tabPanel("Chaos Game",value=1,
          fluidRow(
            column(12,
                   div(
                   div(plotOutput("app.start"),style="width:500px",inline="TRUE"),align="center"),
            #linie orizontala sub joc
             HTML("<hr style='height: 2px; color: #BDBDBD; background-color: #D9D9D9; border: none;'>")
            ), 
            fluidRow(
              column(10, offset=1,
              p("
                Aceasta diagrama afiseaza pas cu pas progresul jocului.
                La fiecare pas se va marca punctul rosu ales aleatoriu.",br(),
                "Apasa pe butonul", span(HTML("&#9654"),style="font-size:10pt;color:#999999;")
                ," sau trage punctul din slide pentru a anima diagrama.",
                style="color:#22283a"
                )
              ) 
            ) 
          ) 
        ) 
       )
    )
  )
 )
)
