library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinythemes)
library(DT)
library(UBay)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    theme = shinytheme("flatly"),

    useShinyjs(),
    withMathJax(),

    # Application title
    titlePanel("A User-Guided Bayesian Framework for Ensemble Feature Selection in Life Science Applications (UBayFS)"),

    # Sidebar with a slider input for number of bins
    fluidPage(
        fluidRow(
            column(2,
                h4("status"),
                disabled(
                    prettyToggle(
                        inputId = "status_data",
                        label_on = "data loaded",
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
                        label_on = "ensemble trained",
                        icon_on = icon("check"),
                        status_on = "info",
                        status_off = "warning",
                        label_off = "no ensemble trained",
                        icon_off = icon("remove")
                    )
                ),
                disabled(
                    prettyToggle(
                        inputId = "status_weighting",
                        label_on = "weights set",
                        icon_on = icon("check"),
                        status_on = "info",
                        status_off = "warning",
                        label_off = "no weights set",
                        icon_off = icon("remove")
                    )
                ),
                disabled(
                    prettyToggle(
                        inputId = "status_prior",
                        label_on = "constraints set",
                        icon_on = icon("check"),
                        status_on = "info",
                        status_off = "warning",
                        label_off = "no constraints set",
                        icon_off = icon("remove")
                    )
                ),
                disabled(
                    prettyToggle(
                        inputId = "status_featureselection",
                        label_on = "features selected",
                        icon_on = icon("check"),
                        status_on = "info",
                        status_off = "warning",
                        label_off = "no features selected",
                        icon_off = icon("remove")
                    )
                ),
                materialSwitch("showInput",
                             label = "input mode",
                             value = TRUE,
                             status = "info"),
                #downloadButton("saveConfig", "save configuration"),
                #actionButton("loadConfig", "load configuration"),
                style='background-color: #222222; color: white; padding: 10px; border-radius: 30px; margin: 10px'
            ),

        # Show a plot of the generated distribution
            column(8,
                style = 'padding: 10px',
                tabsetPanel(id = "tabs",
                    tabPanel("data",
                        # tab input
                        fluidRow(
                            column(12,
                                   h4("input data"),
                                   align = "center"
                            ),
                            conditionalPanel(
                                condition = "input.showInput == true",
                                column(3,
                                    tabsetPanel(type = "pills",
                                        tabPanel("upload",
                                            h5("load training data"),
                                            fileInput("train_data", NULL, multiple = FALSE, accept = c(".csv")),
                                            h5("load training labels"),
                                            fileInput("train_labels", NULL, multiple = FALSE, accept = c(".csv")),
                                            checkboxInput("input_rownames", "rownames provided?", value = TRUE),
                                            checkboxInput("input_colnames", "colnames provided?", value = TRUE),
                                            checkboxInput("input_blocks", "feature blocks provided?", value = TRUE)
                                        ),
                                        tabPanel("demo",
                                            h5("Wisconsin breast cancer"),
                                            actionButton("demo_data","load data")
                                        )
                                    ),
                                    style='border: 1px solid black; border-radius: 30px; padding: 10px; margin: 10px',
                                    align = "left"
                                )
                            ),
                            column(8,
                                        DT::dataTableOutput("data",
                                                            width = "90%"),
                                        #style='border: 1px solid black; padding: 10px',
                                        align = 'center'
                            )
                        )
                    ),
                    tabPanel("likelihood",
                        fluidRow(
                            column(12,
                                   h4("ensemble feature selector (likelihood)"),
                                   align = "center"
                            ),
                            conditionalPanel(
                                condition = "input.showInput == true",
                                column(3,
                                   pickerInput("method", "select elementary filter(s)",
                                               choices = c("mRMR","Laplacian score","Fisher score","elastic net"),
                                               selected = "mRMR",
                                               multiple = TRUE),
                                   sliderTextInput("M", withMathJax('$$M$$'), choices = c(10,20,50,100,200,500,1000)),
                                   sliderInput("tt_split", "train-test-split", min = 0.5, max = 0.9, step = 0.05, value = 0.75),
                                   sliderInput("n_feats", "number of features", min = 1, max = 10, value = 10, step = 1),
                                   disabled(actionButton("confirmParam", "confirm")),
                                   style='border: 1px solid black; border-radius: 30px; padding: 10px; margin: 10px',
                                   align = 'center'
                                )
                            ),
                            column(8,
                                   plotOutput("count_hist"),
                                   #style='border: 1px solid black; padding: 10px',
                                   align = 'center'
                             )
                        )
                    ),
                    tabPanel("weights",
                             fluidRow(
                                 column(12,
                                        h4("prior feature weights (block weights)"),
                                        align = "center"
                                 ),
                                 conditionalPanel(
                                     condition = "input.showInput == true",
                                     column(3,
                                            DT::dataTableOutput("blocks"),
                                            style='border: 1px solid black; border-radius: 30px; padding: 10px; margin: 10px'
                                     )
                                 ),
                                column(8,
                                       plotOutput("weight_hist"),
                                       #style='border: 1px solid black; padding: 10px',
                                       align = 'center'
                                 )
                             )
                    ),
                    tabPanel("constraints",
                        fluidRow(
                            column(12,
                                   h4("prior constraints"),
                                   align = "center"
                            ),
                            conditionalPanel(
                                condition = "input.showInput == true",
                                column(3,
                                    tabsetPanel(type = "pills",
                                                  tabPanel("size constraint",
                                                        sliderInput("maxsize", "$$s_{max}$$", min = 0, max = 10, value = 10, step = 1),
                                                        actionButton("add_maxsize", "add size constraint"),
                                                  ),
                                                 tabPanel("link constraint",
                                                        DT::dataTableOutput("features"),
                                                        actionButton("add_must", "add must-link"),
                                                        actionButton("add_cannot", "add cannot-link"),
                                                )
                                    ),
                                     hr(),
                                     sliderTextInput("rho", "$$\\rho$$", choices = c(0.01,0.1,1,10,100,Inf)),
                                     plotOutput("rho_plot", height = "200px"),
                                    style='border: 1px solid black; border-radius: 30px; padding: 10px; margin: 10px',
                                    align = 'center'
                                )
                            ),
                            column(8,
                                        uiOutput("constraints"),
                                        #style='border: 1px solid black; padding: 10px',
                                        align = 'center'
                             )
                        )
                    ),
                    tabPanel("feature selection",
                             fluidRow(
                                 column(12,
                                        h4("feature selection"),
                                        align = "center"
                                 ),
                                 conditionalPanel(
                                     condition = "input.showInput == true",
                                     column(3,
                                        disabled(
                                         actionButton("run_UBay", "run UBayFS")
                                        ),
                                        style='border: 1px solid black; border-radius: 30px; padding: 10px; margin: 10px',
                                        align = 'center'
                                     )
                                 ),
                                 column(8,
                                        DT::dataTableOutput("feature_results"),
                                        plotOutput("result_barplot", height = "200px"),
                                        #style='border: 1px solid black; padding: 10px',
                                        align = 'center'
                                )
                             )
                    )
                )
            )
        )
    )
))
