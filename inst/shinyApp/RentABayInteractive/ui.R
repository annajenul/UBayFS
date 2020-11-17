library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinythemes)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    theme = shinytheme("readable"),

    useShinyjs(),
    withMathJax(),

    # Application title
    titlePanel("RentABay - Bayesian Repeated Elastic Net Technique for User-Guided Feature Selection"),

    # Sidebar with a slider input for number of bins
    fluidPage(
        fluidRow(
            column(2,
                h4("status"),
                disabled(
                    prettyToggle(
                        inputId = "status_data",
                        label_on = "data ok",
                        icon_on = icon("check"),
                        status_on = "info",
                        status_off = "warning",
                        label_off = "no data loaded",
                        icon_off = icon("remove")
                    )
                ),
                disabled(
                    prettyToggle(
                        inputId = "status_likelihood",
                        label_on = "likelihood setting ok",
                        icon_on = icon("check"),
                        status_on = "info",
                        status_off = "warning",
                        label_off = "no likelihood setting",
                        icon_off = icon("remove")
                    )
                ),
                disabled(
                    prettyToggle(
                        inputId = "status_prior",
                        label_on = "prior setting ok",
                        icon_on = icon("check"),
                        status_on = "info",
                        status_off = "warning",
                        label_off = "no prior setting",
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
            column(10,
                tabsetPanel(id = "tabs",
                    tabPanel("input",
                        fluidRow(
                            column(6,
                                fileInput("train_data", "load training data", multiple = FALSE, accept = c(".csv")),
                                align = "center",
                                style='border: 1px solid black'
                            ),
                            column(6,
                                fileInput("train_labels", "load training labels", multiple = FALSE, accept = c(".csv")),
                                align = "center",
                                style='border: 1px solid black'
                            )
                        )
                    ),
                    tabPanel("data matrix",
                             fluidRow(
                                 column(10,
                                        h4("data overview"),
                                        DT::dataTableOutput("data",
                                                            width = "90%"),
                                        style='border: 1px solid black',
                                        align = 'center'
                                 )
                             )
                    ),
                    tabPanel("likelihood parameters",
                        fluidRow(
                            column(6,
                                   column(12,
                                          h4("ensemble parameters"),
                                          align="left"
                                   ),
                                   selectizeInput("method", "select elementary filter(s)",
                                               choices = c("Laplacian score", "Fisher score", "mRMR", "elastic net"),
                                               multiple = TRUE),
                                   checkboxInput("ranking", "ranking", value = TRUE),
                                   sliderInput("n_feats", "number of features", min = 1, max = 10, value = 10, step = 1),
                                   sliderTextInput("K", withMathJax('$$K$$'), choices = c(10,20,50,100,200,500,1000)),
                                   sliderInput("testsize", "testsize", min = 0.1, max = 0.9, step = 0.05, value = 0.25),
                                   style='border: 1px solid black',
                                   align = 'center'
                            ),
                            column(6,
                                   column(12,
                                          h4("single model parameters"),
                                          align="left"
                                   ),
                                    tabsetPanel(id = "single_model_parameters",
                                        tabPanel("elastic net",
                                                sliderInput("enet.alpha", withMathJax('$$\\alpha$$'), min = 0, max = 1, step = 0.1, value = 0.1),
                                                sliderTextInput("enet.lambda", withMathJax('$$\\lambda$$'), choices = c(1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3))
                                        )
                                    ),
                                   style='border: 1px solid black',
                                   align = 'center'
                            )
                        ),
                        hr(),
                        fluidRow(
                            column(12,
                                   disabled(actionButton("confirmParam", "confirm")),
                                   align = "center"
                            )
                        )
                    ),
                    tabPanel("likelihood counts",
                             fluidRow(
                                 column(10,
                                        h4("count overview"),
                                        DT::dataTableOutput("counts",
                                                            width = "90%"),
                                        style='border: 1px solid black',
                                        align = 'center'
                                 )
                             )
                    ),
                    tabPanel("prior parameters",
                        fluidRow(
                            column(8,
                                   fluidRow(
                                       column(4,
                                              h4("size constraint")
                                       ),
                                       column(4,
                                              sliderInput("maxsize", "max. size", min = 0, max = 10, value = 10, step = 1),
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
                                              DT::dataTableOutput("features",
                                                                  width = "90%")
                                       ),
                                       column(4,
                                            fluidRow(
                                              actionButton("add_must", "add must-link")
                                            ),
                                            fluidRow(
                                              actionButton("add_cannot", "add cannot-link")
                                            )
                                       )
                                   ),
                                   style='border: 1px solid black',
                                   align = 'center'
                            ),
                            column(4,
                                   fluidRow(
                                       column(12,
                                              sliderTextInput("rho", "constraint shape $$\\rho$$", choices = c(0.1,1,10,100,1000)),
                                              plotOutput("rho_plot", height = "200px")
                                       )
                                   ),
                                   style='border: 1px solid black',
                                   align = 'center'
                            )
                        )
                    ),
                    tabPanel("prior constraints",
                             fluidRow(
                                 column(10,
                                        h4("constraint overview"),
                                        uiOutput("constraints"),
                                        style='border: 1px solid black',
                                        align = 'center'
                                 )
                             )
                    ),
                    tabPanel("feature selection",
                             fluidRow(
                                 column(10,
                                        h4("feature selection results"),
                                        DT::dataTableOutput("feature_results"),
                                        style='border: 1px solid black',
                                        align = 'center'
                                 )
                             )
                    )
                )
            )
        )
    )
))
