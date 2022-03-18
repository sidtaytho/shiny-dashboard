##########################################
####   Shiny ui                       ####
##########################################
library(shinyWidgets)
library(shiny)
library(shinyWidgets)
library(shiny)
library(plotly)
library(shinythemes)
library(DT)
library(rsconnect)
# ------------------
# Main title section
# ------------------

ui <- navbarPage(
  "District Data",
  theme = shinytheme("flatly"),
  tabPanel(
    "Main",
    # App title ----
    titlePanel(div(
      windowTitle = "GraduatEmploymentSG",
      img(src = "shiny_dash_header.png", width = "100%", class = "bg"),
    )),

    tags$br(),


    ##########################################
    ####  Panel: Main>Summary             ####
    ##########################################

    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Summary",
        ################################################
        #### Panel: Main>Summary>Tables & Pie Chart ####
        ################################################

        # ------------------
        # ranking $ pie chart section
        # ------------------

        sidebarLayout(
          sidebarPanel(
            h3("Data by District"),
            tags$br(),
            selectInput(
              "checkDistrict",
              "Select District",
              unique(data$dist_name)
            )
          ,
            selectInput(
              "compareDistrict",
              "Select District",
              unique(data$dist_name)
            )
          ),
          mainPanel(
            tabsetPanel(
              type = "tabs",
              tabPanel("Districts", tableOutput("datahead")),
              tabPanel("Union Membership", plotOutput(outputId = "piePlot"))
            ),
            tags$br(),
            tags$br(),
          )
        ),
        tags$hr(),


        sidebarLayout(
          sidebarPanel(
            # ------------------
            # Data overview filters
            # ------------------

            h3("Data Overview"),
            tags$br(),
            setSliderColor(c("#2c3e50 ", "#2c3e50"), c(1, 2)),
            sliderInput(
              "ageRange",
              label = "Age Range",
              min = 0,
              max = 100,
              value = c(0,100)
            ),
            selectInput(
              "itemsDistrict",
              "Select District",
              unique(data$dist_name),
            ),

            #checkboxGroupInput("checkYear", label = "Select Year",
            #                  choices = list("2013", "2014", "2015", "2016", "2017", "2018"),
            #                 selected = list("2013", "2014", "2015", "2016", "2017", "2018"), inline = TRUE),

            actionButton("actionDT", "Filter", class = "btn btn-warning"),
          ),
          mainPanel(
            h3("Browse All"),
            tags$br(),
            dataTableOutput("myTable"),
            tags$br(),
            tags$br(),
          )
        ),
        tags$hr(),
      ),


      ################################################
      #### Panel: Main>Plots                      ####
      ################################################

      tabPanel(
        "Visual Comparison",

        # --------------------
        # density plot section
        # --------------------

        sidebarLayout(
          sidebarPanel(
            h3("Density Plot Panel"),
            tags$br(),
            selectInput(
              "selectvar",
              label = "Choose a variable to display",
              choices = c(
                "School Pride" = "school_pride",
                "School Safety" = "school_safety"
              ),
              selected = "basic monthly mean"
            ),

            checkboxGroupInput(
              "checkGroup",
              label = "Select District",
              choices = list(
                "Abilene USD" = "Abilene USD",
                "Andover USD" = "Andover USD",
                "Atchison USD" = "Atchison USD"
              ),
              selected = list(
                "Abilene USD" = "Abilene USD",
                "Andover USD" = "Andover USD",
                "Atchison USD" = "Atchison USD"              )
            ),
          ),
          mainPanel(
            h3("Distribution"),
            plotlyOutput(outputId = "densityPlot"),
            tags$br(),
            tags$br()
          )
        ),
        tags$hr(),

        # --------------------
        # bar plot section
        # --------------------
        sidebarLayout(
          sidebarPanel(
            h3("Bar Plot Panel"),
            tags$br(),
            radioButtons(
              "radio",
              label = "Select District",
              choices = list(
                "Abilene USD" = "Abilene USD",
                "Andover USD" = "Andover USD",
                "Atchison USD" = "Atchison USD"
              ),
              selected = "Abilene USD"
            ),
            tags$hr()
          ),
          mainPanel(
            h3("Overall Satisfaction by Role"),
            plotlyOutput(outputId = "uniPlot"),
            tags$br(),
            tags$br()
          )
        ),
        tags$hr(),

        # --------------------
        # box plot section
        # --------------------
        sidebarLayout(
          sidebarPanel(
            h3("Box Plot Panel"),
            tags$br(),
            checkboxGroupInput(
              "checkGroupbox",
              label = "Select District",
              choices = list(
                "Abilene USD" = "Abilene USD",
                "Andover USD" = "Andover USD",
                "Atchison USD" = "Atchison USD"
              ),
              selected = list(
                "Abilene USD" = "Abilene USD",
                "Andover USD" = "Andover USD",
                "Atchison USD" = "Atchison USD"
              )
            ),

            tags$hr()
          ),
          mainPanel(
            h3("Overall Satisfaction by District"),
            plotlyOutput(outputId = "boxPlot"),
            tags$br(),
            tags$br(),
            tags$br(),
          )
        ),

        tags$hr(),

        # --------------------
        # Scatter plot section
        # --------------------


        fluidPage(fluidRow(
          h3("Overall Satisfaction by Age in District: 223"),
          align = "center",
          plotlyOutput(outputId = "scatPlot", width = "100%"),
          div(style = "height:400px")
        )),

        tags$br(),
        tags$br(),
        tags$hr(),

      ),


      ################################################
      #### Panel: Main>Details                    ####
      ################################################

      tabPanel(
        "Details By District, Role, Union Status",
        h3("Details by District, Role, Union Status", align = "center"),
        br(),
        div(style = "display:vertical-align:center;center-align",
            fluidRow(
              column(
                4,
                selectInput(
                  "detailDistrict",
                  label = "Select District",
                  choices = unique(data$dist_name),
                  selected = "",
                  width = 400
                ),
              ),
              column(
                4,
                selectInput(
                  "detailRole",
                  "Select Role",
                  choices = "",
                  selected = "",
                  width = 400
                )
              ),
              column(4,
                     column(
                       8,
                       selectInput(
                         "detailUnion",
                         "Select Union Status",
                         choices = "",
                         selected = "",
                         width = 400
                       )
                     ),
                     column(
                       4,
                       tags$br(),
                       actionButton("detailFilter", "Filter", class = "btn btn-warning btn-sm")
                     ))
            )),

        tags$br(),
        tags$br(),
        tags$hr(),
        tags$br(),

        fluidRow(
          column(4, tableOutput("detailTable")),
          column(4, h5("Years Teaching x Overall Satisfaction", align="center"), plotOutput(outputId = "detailPlot", height = "300px")),
          column(4, h5("Years Teaching x School Safety", align="center"), plotOutput(outputId = "detailPlotem", height = "300px"))
        ),

        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$hr(),
        tags$br()
      )
    )
  ),



  ################################################
  #### Panel: Report                   ####
  ################################################

  tabPanel("Initial Report",
           fluidPage(htmlOutput("report"))),

  ################################################
  #### Panel: Index                           ####
  ################################################
  tabPanel("Index",
           fluidPage(htmlOutput("index")))
)
