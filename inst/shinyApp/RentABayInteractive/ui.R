library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinythemes)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    theme = shinytheme("spacelab"),

    useShinyjs(),

    # Application title
    titlePanel("RentABay - Bayesian Repeated Elastic Net Technique for User-Guided Feature Selection"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h4("Data"),
            fileInput("train_data", "load training data", multiple = FALSE, accept = c(".csv")),
            fileInput("train_labels", "load training labels", multiple = FALSE, accept = c(".csv")),
            hr(),
            h4("Status"),
            disabled(
                prettyToggle(
                    inputId = "status_data",
                    label_on = "data loaded",
                    icon_on = icon("check"),
                    status_on = "info",
                    status_off = "warning",
                    label_off = "no data loaded yet",
                    icon_off = icon("remove")
                )
            ),
            disabled(
                prettyToggle(
                    inputId = "status_likelihood",
                    label_on = "likelihood set",
                    icon_on = icon("check"),
                    status_on = "info",
                    status_off = "warning",
                    label_off = "no likelihood set",
                    icon_off = icon("remove")
                )
            ),
            disabled(
                prettyToggle(
                    inputId = "status_prior",
                    label_on = "prior set",
                    icon_on = icon("check"),
                    status_on = "info",
                    status_off = "warning",
                    label_off = "no prior set",
                    icon_off = icon("remove")
                )
            ),
            disabled(
                prettyToggle(
                    inputId = "status_featureselection",
                    label_on = "optimal features calculated",
                    icon_on = icon("check"),
                    status_on = "info",
                    status_off = "warning",
                    label_off = "no optimal features calculated",
                    icon_off = icon("remove")
                )
            ),
            hr(),
            h4("RentABay"),
            disabled(
                actionButton("run_RentABay", "run feature selection")
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(id = "tabs",
                tabPanel("input",
                    h2("training data matrix"),
                    DT::dataTableOutput("data")
                ),
                tabPanel("likelihood parameters",
                    h2("model parameters for RENT model"),
                    fluidRow(
                        column(6,
                            h4("elastic net regularization parameters"),
                            sliderInput("alpha", withMathJax('$$\\alpha$$'), min = 0, max = 1, step = 0.1, value = 0.1),
                            sliderTextInput("lambda", withMathJax('$$\\lambda$$'), choices = c(1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3))
                        ),
                        column(6,
                            h4("RENT model parameters"),
                            sliderTextInput("K", withMathJax('$$K$$'), choices = c(10,20,50,100,200,500,1000)),
                            sliderInput("testsize", "testsize", min = 0.1, max = 0.9, step = 0.05, value = 0.25)
                        )
                    ),
                    fluidRow(
                        column(12,
                               actionButton("confirmParam", "confirm"),
                               align = "center"
                        )
                    ),
                    hr(),
                    fluidRow(
                        column(12,
                               h4("parameter overview"),
                               uiOutput("params"),
                               style = "background-color: #aaaaaa;"
                        )
                    )
                ),
                tabPanel("prior parameters",
                    h2("constraint builder for prior model"),
                    fluidRow(
                        column(6,
                               fluidRow(
                                   column(12,
                                          h4("define constraint"),
                                          align="center"
                                    )
                               ),
                               hr(),
                               fluidRow(
                                   column(4,
                                          h4("shape")
                                   ),
                                   column(8,
                                          sliderTextInput("rho", "constraint shape", choices = c(0.1,1,10,100,1000))
                                   )
                               ),
                               hr(),
                               fluidRow(
                                   column(4,
                                          h4("size constraint")
                                   ),
                                   column(4,
                                          sliderInput("maxsize", "maximum size", min = 0, max = 10, value = 10, step = 1),
                                   ),
                                   column(4,
                                          actionButton("add_maxsize", "add size constraint")
                                   )
                               ),
                               hr(),
                               fluidRow(
                                   column(4,
                                          h4("link constraint")
                                   ),
                                   column(4,
                                          actionButton("add_must", "add must-link")
                                   ),
                                   column(4,
                                          actionButton("add_cannot", "add cannot-link")
                                   ),

                               )
                        ),
                        column(6,
                            fluidRow(
                                   column(12,
                                          h4("select features"),
                                          align="center"
                                   )
                            ),
                            hr(),
                            fluidRow(
                                column(12,
                                       DT::dataTableOutput("features")
                                )
                            )
                        )
                    ),
                    hr(),
                    fluidRow(
                        column(12,
                            h4("constraint overview"),
                            DT::dataTableOutput("constraints"),
                            style = "background-color: #aaaaaa;"
                        )
                    )
                ),
                tabPanel("feature selection",
                    h2("results from RentABay feature selection"),
                    textOutput("selected_features"),
                    DT::dataTableOutput("probabilities")
                )
            )
        )
    )
))
