library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinythemes)
library(DT)
library(UBayFS)

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
                fluidRow(
                    downloadButton("save_model", "save model"),
                    actionButton("load_model", "load model"),
                    align = "center"
                ),
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
                                    tabsetPanel(
                                        id = "input_type",
                                        type = "pills",
                                        tabPanel("upload",
                                            h5("load training data"),
                                            fileInput("upload_data", NULL, multiple = FALSE, accept = c(".csv")),
                                            h5("load training labels"),
                                            fileInput("upload_labels", NULL, multiple = FALSE, accept = c(".csv")),
                                            checkboxInput("input_rownames", "rownames provided?", value = TRUE),
                                            checkboxInput("input_colnames", "colnames provided?", value = TRUE),
                                            checkboxInput("input_blocks", "feature blocks provided?", value = TRUE)
                                        ),
                                        tabPanel("demo",
                                            h5("Wisconsin breast cancer"),
                                            fluidRow(
                                                actionButton("demo_data","load data"),
                                                align = "center",
                                            )
                                        )
                                    ),
                                    style='border: 1px solid black; border-radius: 30px; padding: 10px; margin: 10px',
                                    align = "left"
                                )
                            ),
                            uiOutput("output_data")
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
                                   uiOutput("likelihood_sliders"),
                                   disabled(actionButton("confirmParam", "build ensemble")),
                                   style='border: 1px solid black; border-radius: 30px; padding: 10px; margin: 10px',
                                   align = 'center'
                                )
                            ),
                            uiOutput("output_likelihood")
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
                                            uiOutput("blocktable"),
                                            style='border: 1px solid black; border-radius: 30px; padding: 10px; margin: 10px;
                                            height: 400px; overflow-y: scroll; overflow-x: hidden'
                                     )
                                 ),
                                uiOutput("output_weights")
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
                                                        disabled(actionButton("add_maxsize", "add size constraint")),
                                                  ),
                                                 tabPanel("link constraint",
                                                        DT::dataTableOutput("features"),
                                                        disabled(actionButton("add_must", "add must-link")),
                                                        disabled(actionButton("add_cannot", "add cannot-link")),
                                                )
                                    ),
                                     hr(),
                                     sliderTextInput("rho", "$$\\rho$$", choices = c(0.01,0.1,1,10,100,Inf), selected = 1),
                                     plotOutput("rho_plot", height = "200px"),
                                    style='border: 1px solid black; border-radius: 30px; padding: 10px; margin: 10px',
                                    align = 'center'
                                )
                            ),
                            uiOutput("output_constraints")

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
                                        uiOutput("fs_sliders"),
                                        disabled(
                                         actionButton("run_UBay", "run UBayFS")
                                        ),
                                        style='border: 1px solid black; border-radius: 30px; padding: 10px; margin: 10px',
                                        align = 'center'
                                     )
                                 ),
                                 uiOutput("output_fs")
                            )
                    )
                )
            )
        )
    )
))
