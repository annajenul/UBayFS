library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(DT)
library(UBayFS)

# Define UI for application that draws a histogram
shinyUI(fluidPage(


    useShinyjs(),
    withMathJax(),

    tags$head(
        tags$style("body{color: #2E75B6, font-family: Constantia}
                   label{color: #2E75B6, font-family: Constantia}"),
        tags$title("UBayFS")
    ),

    # Application title
    div(
        style = "margin-top: 10px;
                 margin-right: 10px",
        fluidRow(

            column(1,
                   img(src = "logo.png", width = "100px"),
            ),
            column(11,
                   h2("A User-Guided Bayesian Framework for Ensemble Feature Selection"),
                   align = "right",
                   style = "background-color: #2E75B6;
                            color:white;
                            font-family: Constantia;
                            padding-top: 10px;
                            padding-bottom: 10px;
                            padding-right: 20px;
                            border-radius: 30px;
                            height: 100px;
                            top: 0")
        )
    ),

    # Sidebar with a slider input for number of bins
    div(
        fluidRow(
        # Show a plot of the generated distribution
            column(1,
                   dropdownButton(
                       label = NULL,
                       icon = icon("tools"),
                       circle = TRUE,
                       materialSwitch("showInput",
                                      label = "input mode",
                                      value = TRUE,
                                      status = "info"),
                       actionButton("load_model", "load model", style = "width: 150px"),
                       downloadButton("save_model", "save model", style = "width: 150px"),
                       actionButton("stop_app", "stop app", style = "width: 150px"),
                       actionButton("refresh_app", "refresh app", style = "width: 150px"),
                       margin = "10px",
                       width = "200px"
                   ),
                  style = "padding: 30px"
            ),
            column(11,
                style = 'padding: 10px',
                verticalTabsetPanel(id = "tabs",
                                    color = "#2E75B6",
                                    contentWidth = 10,
                    verticalTabPanel(
                        box_height = "60px",
                        title = NULL,
                        icon = uiOutput("lab_data_tab"),

                        # tab input
                        fluidRow(
                            column(12,
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
                                            checkboxInput("input_blocks", "feature blocks provided?", value = TRUE),
                                            bsTooltip("input_rownames", "Are rownames provided in column 1 of your input files?"),
                                            bsTooltip("input_colnames", "Are colnames provided in row 1 of your input files?"),
                                            bsTooltip("input_blocks", "Are block names provided in row 1 (without rownames) or row 2 (with rownames) of your input files?"),
                                            actionButton("load_csv", "load data")
                                        ),
                                        tabPanel("demo",
                                            h5("Breast Cancer Wisconsin"),
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
                    verticalTabPanel(
                        box_height = "60px",
                        title = NULL,
                        icon = uiOutput("lab_ensemble_tab"),
                        fluidRow(
                            column(12,
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
                    verticalTabPanel(
                            box_height = "60px",
                             title = NULL,
                             icon = uiOutput("lab_weights_tab"),
                             fluidRow(
                                 column(12,
                                        align = "center"
                                 ),
                                 conditionalPanel(
                                     condition = "input.showInput == true",

                                     column(3,
                                            id = "blocktable_container",
                                            uiOutput("blocktable"),
                                            style='border: 1px solid black; border-radius: 30px; padding: 10px; margin: 10px;
                                            height: 400px; overflow-y: scroll; overflow-x: hidden'
                                     ),
                                     tabPanel("set_weights",
                                              actionButton("setweights", "set weights")
                                 )),
                                uiOutput("output_weights")
                             )
                    ),
                    verticalTabPanel(
                        box_height = "60px",
                        title = NULL,
                        icon = uiOutput("lab_constraints_tab"),
                        fluidRow(
                            column(12,
                                   align = "center"
                            ),
                            conditionalPanel(
                                condition = "input.showInput == true",
                                column(3,
                                    tabsetPanel(type = "pills",
                                                  tabPanel("size constraint",
                                                        sliderInput("maxsize", "$$s_{max}$$", min = 0, max = 10, value = 10, step = 1),
                                                        bsTooltip("maxsize", "Select the maximum number of features to be selected (max-size constraint)"),
                                                        disabled(actionButton("add_maxsize", "add size constraint")),
                                                  ),
                                                 tabPanel("link constraint",
                                                        DT::dataTableOutput("features"),
                                                        bsTooltip("features", "Select the features to be linked by must-link / cannot link constraint"),
                                                        disabled(actionButton("add_must", "add must-link")),
                                                        disabled(actionButton("add_cannot", "add cannot-link")),
                                                )
                                    ),
                                     hr(),
                                     sliderTextInput("rho", "$$\\rho$$", choices = c(0.01,0.1,1,10,100,Inf), selected = 1),
                                    bsTooltip("rho", "Select the relaxation parameter $$\\rho$, specifying the level of penalization if the constraint is violated"),
                                     plotOutput("rho_plot", height = "200px"),
                                    style='border: 1px solid black; border-radius: 30px; padding: 10px; margin: 10px',
                                    align = 'center'
                                )
                            ),
                            uiOutput("output_constraints")

                        )
                    ),
                    verticalTabPanel(
                        id = "block_constraints_tab",
                        box_height = "60px",
                        title = NULL,
                        icon = uiOutput("lab_block_constraints_tab"),
                        fluidRow(
                            column(12,
                                   align = "center"
                            ),
                            conditionalPanel(
                                condition = "input.showInput == true",
                                column(3,
                                       tabsetPanel(type = "pills",
                                                   tabPanel("size constraint",
                                                            sliderInput("block_maxsize", "$$s_{max}$$", min = 0, max = 10, value = 10, step = 1),
                                                            bsTooltip("block_maxsize", "Select the maximum number of blocks to be selected (max-size constraint)"),
                                                            disabled(actionButton("add_block_maxsize", "add size constraint")),
                                                   ),
                                                   tabPanel("link constraint",
                                                            DT::dataTableOutput("blocks"),
                                                            bsTooltip("blocks", "Select the blocks to be linked by must-link / cannot link constraint"),
                                                            disabled(actionButton("add_block_must", "add must-link")),
                                                            disabled(actionButton("add_block_cannot", "add cannot-link")),
                                                   )
                                       ),
                                       hr(),
                                       sliderTextInput("rho_block", "$$\\rho$$", choices = c(0.01,0.1,1,10,100,Inf), selected = 1),
                                       bsTooltip("rho_block", "Select the relaxation parameter $$\\rho$, specifying the level of penalization if the constraint is violated"),
                                       plotOutput("rho_block_plot", height = "200px"),
                                       style='border: 1px solid black; border-radius: 30px; padding: 10px; margin: 10px',
                                       align = 'center'
                                )
                            ),
                            uiOutput("output_block_constraints")
                        )
                    ),
                    verticalTabPanel(
                            box_height = "60px",
                             title = NULL,
                             icon = uiOutput("lab_results_tab"),
                             fluidRow(
                                 column(12,
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
    ),
    div(style = "height: 130px"),
    div(
        column(6,
            div(strong("Anna Jenul"), "<", a("anna.jenul@nmbu.no", href = "mailto:anna.jenul@nmbu.no"),">"),
            div(strong("Stefan Schrunner"), "<", a("stefan.schrunner@nmbu.no", href = "mailto:stefan.schrunner@nmbu.no"),">"),
            align = "left",
            style = "padding-top: 10px"
        ),
        column(6,
            img(src = "logo_nmbu.png", height = "80px"),img(src = "logo_ceheads.png", height = "100px"),
            align = "right"
        ),
        style = "z-index: 1000;
                 position:fixed;
                 bottom:0;
                 left:0;
                 width: 100%;
                 height: 130px;
                 background-color: white;
                 padding-top: 15px;
                 padding-bottom: 15px;
                 padding-left: 0px;
                 border-top: 1px solid black"
    )
))
