##########################################
####   Main Libraries                 ####
##########################################
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(knitr)
library(kableExtra)
library(ggthemes)
library(plotly)

library(rsconnect)
library(shinythemes)

##########################################
####   Attaching datasets             ####
##########################################

data <- data_anon
saveRDS(data, file = "data_anon.rds")
data <- readRDS("data_anon.rds")

## Setting datatables view

opts <- list(
  language = list(url = "//cdn.datatables.net/plug-ins/1.10.19/i18n/English.json"),
  pageLength = 30,
  searchHighlight = TRUE,
  orderClasses = TRUE,
  columnDefs = list(list(
    targets = c(1, 6), searchable = FALSE
  ))
)


##########################################
####   Shiny server                   ####
##########################################

server <- function(session, input, output) {
  ################################################
  #### Panel: Main>Summary>Tables & Pie Chart ####
  ################################################

  # ----------------
  # Summary section
  # ----------------


  output$datahead <- renderPrint({
    data %>%
      filter(dist_name == c(input$checkDistrict, input$compareDistrict)) %>%
      group_by(dist_name) %>%
      select(dist_name,
             overall_satisfaction,
             school_safety) %>%
      summarise_all(funs(mean)) %>%
      arrange(desc(overall_satisfaction)) %>%
      kable(
        "html",
        col.names = c(
          "District",
          "School Pride",
          "School Safety"
        )
      ) %>%
      kable_styling(c("striped", "hover"), full_width = T)

  })

  # ----------------
  # pie plot section
  # ----------------

  output$piePlot <- renderPlot({
    colmap <-
      c(
        "#bdb2ff",
        # DISTRICT
        "#ffc6ff",
        # STATE
        "#fffffc",
        # SMU
        "#33658A",
        # SIT
        "#3a506b",
        # SUTD
        "#577590",
        # SUSS
        "#43aa8b",
        # NIE
        "#90be6d",
        # SP
        "#f9c74f",
        # NP
        "#f8961e",
        # TP
        "#f3722c",
        # NAYANG POLY
        "#f94144",
        # RP
        "#ffadad",
        # NAFA DEG
        "#ffd6a5",
        # LAS DEG
        "#fdffb6",
        # NAFA DIP
        "#caffbf",
        # NAFA DEG
        "#a8dadc"  # ITE
      )

    data %>%
      filter(dist_name == input$checkDistrict) %>%
      group_by(union_membership) %>%
      count(union_membership) %>%
      ggplot(aes(x = "", y = n, fill = union_membership)) +
      geom_bar(
        stat = "identity",
        width = 1,
        color = "black",
        size = 1
      ) +
      theme_void() +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5, size = 14)) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c(colmap)) +
      labs(title = "Union Membership")

  })

  # ------------------
  # data table section
  # ------------------

  # filter the checkgroup input:

  districtGroup <- reactive({
    input$actionDT
    isolate(return(data[data$dist_name %in% input$itemsDistrict, ]))
  })


  filtered_DT <- reactive({
    input$actionDT
    isolate({
      minAge <- input$ageRange[1]
      maxAge <- input$ageRange[2]
    })

    districtGroup() %>%
      filter(age > minAge,
             age < maxAge)
  })

  # render DT:

  output$myTable <- renderDataTable({
    filtered_DT() %>%
      datatable(
        .,
        rownames = FALSE,
        class = "table",
        options = list(pageLength = 10, scrollX = T)
      )

  })


  ################################################
  #### Panel: Main>Plots                      ####
  ################################################

  # --------------------
  # density plot section
  # --------------------

  # filter the checkgroup input:

  dent <-  reactive({
    return(data[data$dist_name %in% input$checkGroup, ])

  })

  # render density plot

  output$densityPlot <- renderPlotly({
    colmap <- c("#2c3e50",
                "#e67e22",
                "#f1c40f",
                "#e74c3c",
                "#F97F51",
                "#27ae60")

    ggplotly(
      ggplot(data = dent(), aes_string(x = input$selectvar)) +
        geom_density(aes(fill = role), size = 1, alpha=0.75) +
        theme(legend.position = "bottom") + labs(x = input$selectvar) +
        theme_hc() +
        theme(
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
    ) %>% layout(legend = list(orientation = "h",
                               y = 0, x = 0))

  })


  # ----------------
  # bar plot section
  # ----------------

  # filter the input and group data:

  output$uniPlot <- renderPlotly({
    monthly <- data %>%
      filter(dist_name == input$radio) %>%
      group_by(role, overall_satisfaction) %>%
      summarise_at(.vars = names(.)[7:8], .funs = c(mean = "mean"))

    # render bar plot

    colmap <-
      c(
        "#2c3e50",
        "#e67e22",
        "#f1c40f",
        "#e74c3c",
        "#F97F51",
        "#27ae60",
        "#2980b9",
        "#86BBD8",
        "#8e44ad",
        "#95a5a6",
        "#f39c12",
        "#d35400",
        "#c0392b",
        "#bdc3c7",
        "#D6A2E8",
        "#25CCF7",
        "#16a085"
      )

    p <- monthly %>%
      data.frame() %>%
      ggplot(.,
             aes(
               x = reorder(role, overall_satisfaction),
               y = overall_satisfaction,
               fill = role
             )) +
      geom_bar(
        stat = "identity",
        width = 0.5,
        color = "black",
        size = 1
      ) +
      scale_fill_manual(values = colmap) +
      theme_hc() +
      theme(
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )

    p <- ggplotly(p + coord_flip(), tooltip = ("overall_satisfaction"))
    hide_legend(p)

  })

  # ----------------
  # box plot section
  # ----------------




  # filter the checkgroup input:

  uniMedian <-  reactive({
    return(data[data$dist_name%in%input$checkGroupbox, ])
  })

  # render box plot

  output$boxPlot <- renderPlotly({

    colmap <- c("#2c3e50",
                "#e67e22",
                "#f1c40f",
                "#e74c3c",
                "#F97F51",
                "#27ae60")

    p <-
      ggplot(data = uniMedian(),
             aes(x = dist_name, y = overall_satisfaction, fill = dist_name)) +
      geom_boxplot(color = "black",
                   size = 1,
                   width = 0.3) +
      scale_fill_manual(values = colmap) +
      theme_hc() +
      theme(
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )

    p <- ggplotly(p + coord_flip(), tooltip = ("overall_satisfaction"))
    hide_legend(p)

  })


  # ----------------
  # scatter plot section
  # ----------------

  output$scatPlot <- renderPlotly({
    colmap <- c("#2c3e50",
                "#e67e22",
                "#f1c40f",
                "#e74c3c",
                "#F97F51",
                "#27ae60")

    data <- data %>% filter(dist_code=="223")

    p <-
      ggplot(
        data,
        aes(
          x = age,
          y = overall_satisfaction,
          color = role
        )
      ) +
      geom_point(size = 3, alpha = 0.7) +
      scale_colour_manual(values = colmap) +
      theme_hc() +
      theme(
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )

    ggplotly(
      p,
      tooltip = c(
        "dist_code",
        "age",
        "overall_satisfaction",
        "role"
      ),
      height = 800
    )

  })


  ################################################
  #### Panel: Main>Details                    ####
  ################################################


  observeEvent(
    input$detailDistrict,
    updateSelectInput(
      session,
      "detailRole",
      "Select Role",
      choices = unique(data$role[data$dist_name == input$detailDistrict])
    )
  )
  observeEvent(
    input$detailRole,
    updateSelectInput(
      session,
      "detailUnion",
      "Select Union Status",
      choices = unique(data$union_membership[data$role == input$detailRole &
                                     data$dist_name == input$detailDistrict])
    )
  )

  detailTB <- eventReactive(input$detailUnion,
                            {
                              data %>%
                                filter(
                                  role == input$detailRole &
                                    dist_name == input$detailDistrict &
                                    union_membership == input$detailUnion
                                ) %>%
                                select(c(
                                  "years_teaching",
                                  "overall_satisfaction",
                                  "school_safety"
                                ))

                            })

  output$detailTable <- renderPrint({
    input$detailFilter

    isolate({
      detailTB() %>%
        data.frame() %>%
        kable("html",
              col.names = c("Years Teaching", "Overall Satisfaction",
                            "School Safety")) %>%
        kable_styling(c("striped", "hover"), full_width = F)
    })
  })

  # median income plot:

  output$detailPlot <- renderPlot({
    input$detailFilter

    isolate({
      ggplot(detailTB(), aes(x = years_teaching, y = overall_satisfaction)) +
        geom_smooth(
          mapping = aes(linetype = "r2"),
          method = "lm",
          formula = y ~ x + log(x),
          se = FALSE,
          color = "#bdd5ea",
          linetype = "dashed",
          size = 2,
          alpha = 0.5
        ) +
        geom_line(aes(y = overall_satisfaction),
                  size = 2,
                  color = "#2c3e50") +
        geom_point(
          aes(x = years_teaching, y = overall_satisfaction),
          size = 7,
          shape = 21,
          colour = "white",
          fill = "#fca311",
          stroke = 5
        ) +

        theme_hc() +
        theme(
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
    })

  })

  output$detailPlotem <- renderPlot({
    input$detailFilter

    isolate({
      ggplot(detailTB(), aes(x = years_teaching, y = school_safety)) +
        geom_smooth(
          mapping = aes(linetype = "r2"),
          method = "lm",
          formula = y ~ x + log(x),
          se = FALSE,
          color = "#bdd5ea",
          linetype = "dashed",
          size = 2,
          alpha = 0.5
        ) +
        geom_line(aes(y = school_safety),
                  size = 2,
                  color = "#2c3e50") +
        geom_point(
          aes(x = years_teaching, y = school_safety),
          size = 7,
          shape = 21,
          colour = "white",
          fill = "#fca311",
          stroke = 5
        ) +

        theme_hc() +
        theme(
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )
    })

  })

  ################################################
  #### Panel: CONFIDENTIAL                   ####
  ################################################

  getPageDoc <- function() {
    return(includeHTML("ktp.report.beta03.html"))
  }
  output$report <- renderUI({
    getPageDoc()
  })


  ################################################
  #### Panel: INITAL REPORT                           ####
  ################################################

  getPageAbo <- function() {
    return(includeHTML("index.beta01.html"))
  }
  output$index <- renderUI({
    getPageAbo()
  })


}

