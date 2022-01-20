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

    selectizeInput('shape', h5(tags$b('Figura')), choices = c('Triunghi' = 'tri',
                     'Patrat' = 'sqr',
                     'Pentagon' = 'pent',
                     'Hexagon' = 'hex')
    ),

    conditionalPanel(
      condition = "input.shape=='tri'",
        sliderInput("dist.tri",
                    label = h5(tags$b("Distanta pana la urmatorul punct:")),
                    min = 0.01, max = .99, value = .50, step=.01),
      div(tags$b("Triunghi"), ": valoare default = 0.50 (1/2)",
          style = "font-size: 9.5pt;color:black",align="center")
      ),

    conditionalPanel(
      condition = "input.shape=='sqr'",
        sliderInput("dist.sqr",
                    label = h5(tags$b("Distanta pana la urmatorul punct:")),
                    min = 0.01, max = .99, value = .67, step=.01),
      div(tags$b("Patrat"), ": valoare default = 0.67 (2/3)",
          style = "font-size: 9.5pt;color:black",align="center")
      ),

    conditionalPanel(
      condition = "input.shape=='pent'",
        sliderInput("dist.pent",
                    label = h5(tags$b("Distanta pana la urmatorul punct:")),
                    min = 0.01, max = .99, value = .63, step=.01),
      div(tags$b("Pentagon"), ": valoare default = 0.63 (5/8)",
          style = "font-size: 9.5pt;color:black",align="center")
      ),

    conditionalPanel(
      condition = "input.shape=='hex'",
        sliderInput("dist.hex",
                    label = h5(tags$b("Distanta pana la urmatorul punct:")),
                    min = 0.01, max = .99, value = .67, step=.01),
      div(tags$b("Hexagon"), ": valoare default = 0.67 (2/3)",
          style = "font-size: 9.5pt;color:teal",align="center")
      ),

    br(),
    
    conditionalPanel(condition="input.tabselected==1",
                            div(uiOutput("my.init")),
                            div("Apasa butonul ",
                         span(HTML("&#9654"),style=
                                "font-size:10pt;color:#999999;"), "pentru a anima",br(),
                         span("Trage manual de punct la orice moment."),br(),
                         span("Apasa",tags$b("Random"),"pentru a da o pozitie aleatore lui n."),
                         align="center",style="font-size:8.5pt;color:teal")
    ),
    
    conditionalPanel(condition="input.tabselected==2",
                     div(uiOutput("my.extend")),
                     div("Apasa butonul ",
                         span(HTML("&#9654"),style=
                                "font-size:10pt;color:#999999;"), "pentru a anima",br(),
                         span("Trage manual de punct la orice moment."),br(),
                         span("Apasa",tags$b("Random"),"pentru a da o pozitie aleatore lui n."),
                         align="center",style="font-size:8.5pt;color:teal")
    ),

    conditionalPanel(condition="input.tabselected==3",
                     div(uiOutput("my.pts")),
                     div("Apasa butonul ",
                         span(HTML("&#9654"),style=
                                "font-size:10pt;color:#999999;"), "pentru a anima",br(),
                         span("Trage manual de punct la orice moment."),br(),
                         span("Apasa",tags$b("Random"),"pentru a da o pozitie aleatore lui n."),
                         align="center",style="font-size:8.5pt;color:teal")
    ),
    
    br(),
    
        div(bsButton("gen", label="Random"),align="right"),
        div("Apasa", tags$b("Random")," pentru un punct random de pornire",
            style = "font-size: 9.5pt;color:teal",align="right"),

        br(),

        

    ) #sidebarPanel
    ), #column-4

    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    column(8,
      tabsetPanel(type = "tabs",id = "tabselected",
##############
# tabPanel 1 #
##############
        tabPanel("Secventa Initiala",value=1,
          fluidRow(
            column(12,
                   div(
                   div(plotOutput("initPlot"),style="width:500px",inline="TRUE"),align="center"),
             HTML("<hr style='height: 2px; color: #BDBDBD; background-color: #D9D9D9; border: none;'>")
            ), # column-12
            fluidRow(
              column(10, offset=1,
              p("
                Aceasta secventa afiseaza pas cu pas progresia jocului.
                La fiecare pas se va marca punctul rosu ales aleatoriu.
                Apasa pe butonul", span(HTML("&#9654"),style="font-size:10pt;color:#999999;")
                ," sau trage punctul din slide pentru a anima diagrama.",
                style="color:#0066CC"
                )
              ) # column-10
            ) # fluidRow
        ) # fluidRow
      ), # tabPanel

##############
# tabPanel 2 #
##############
        tabPanel("Secventa Avansata",value=2,
          fluidRow(
            column(12,
                   div(
                   div(
                 plotOutput("extendPlot"),style="width:500px",inline="TRUE"),align="center"),
              HTML("<hr style='height: 2px; color: #BDBDBD; background-color: #D9D9D9; border: none;'>")
            ), # column-12
            fluidRow(
              column(10, offset=1,
                     p("
                Compara diagrama de mai sus cand n = 100 si diagrama din 'Secventa Initiala' cand
                n = 100 - acestea vor fi identice, astfel aratand ca aceasta diagrama este o continuare
                a celei initiale. Avanseaza 'Numarul de puncte' din slide la n = 1000 si muta pe 'Secventa Finala'.",
                style="color:#0066CC"
                )
              ) # column-10
            ) # fluidRow
        ) # fluidRow
      ), # tabPanel

##############
# tabPanel 3 #
##############
        tabPanel("Secventa Finala",value=3,
          fluidRow(
            column(12,
                   div(
                   div(
                 plotOutput("compPlot"),style="width:500px",inline="TRUE"),align="center"),
             HTML("<hr style='height: 2px; color: #BDBDBD; background-color: #D9D9D9; border: none;'>")

                 ), # column-12
              fluidRow(
                column(10, offset=1,
                       p("
                Compara diagrama de mai sus cand n = 1000 si diagrama din 'Secventa Avansata' cand
                n = 1000 - acestea vor fi identice, astfel aratand inca o data ca aceasta diagrama este o continuare
                a celei initiale. Punctele din aceasta diagrama au dimensiune mai mica pentru a arata cu acuratete diagrama completa",
                style="color:#0066CC"
                )
                ) # column-10
              ) #fluidRow
          ) #fluidRow
        )  # close tabPanel-Complete
      )# tabsetPanel
    )# column-8
  ) # fluidRow
 )# fluidPage
)# shinyUI
