library(shiny)
library(shinyWidgets)
library(shinyjs)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    useShinyjs(),

    # Application title
    titlePanel("RentABay - Bayesian Repeated Elastic Net Technique for User-Guided Feature Selection"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h2("Data"),
            fileInput("train_data", "load training data", multiple = FALSE, accept = c(".csv")),
            fileInput("train_labels", "load training labels", multiple = FALSE, accept = c(".csv")),
            hr(),
            h2("Status"),
            disabled(
                prettyToggle(
                    inputId = "status_data",
                    label_on = "Data loaded",
                    icon_on = icon("check"),
                    status_on = "info",
                    status_off = "warning",
                    label_off = "No data loaded yet",
                    icon_off = icon("remove")
                )
            ),
            disabled(
                prettyToggle(
                    inputId = "status_likelihood",
                    label_on = "RENT parameters set",
                    icon_on = icon("check"),
                    status_on = "info",
                    status_off = "warning",
                    label_off = "No RENT parameters set",
                    icon_off = icon("remove")
                )
            ),
            disabled(
                prettyToggle(
                    inputId = "status_prior",
                    label_on = "Prior constraints set",
                    icon_on = icon("check"),
                    status_on = "info",
                    status_off = "warning",
                    label_off = "No prior constraints set",
                    icon_off = icon("remove")
                )
            ),
            disabled(
                prettyToggle(
                    inputId = "status_featureselection",
                    label_on = "Feature selection calculated",
                    icon_on = icon("check"),
                    status_on = "info",
                    status_off = "warning",
                    label_off = "No feature selection calculated",
                    icon_off = icon("remove")
                )
            ),
            hr(),
            h2("RentABay"),
            disabled(
                actionButton("run_RentABay", "run feature selection")
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(id = "tabs",
                tabPanel("Input data",
                    textOutput("file"),
                    DT::dataTableOutput("data")
                ),
                tabPanel("RENT parameter selector",
                    sliderInput("alpha", "alpha", min = 0, max = 1, step = 0.1, value = 0.1),
                    sliderInput("lambda", "lambda", min = 0, max = 100, step = 1, value = 1),
                    sliderInput("K", "K", min = 1, max = 100, step = 1, value = 10),
                    sliderInput("testsize", "testsize", min = 0.1, max = 0.9, step = 0.05, value = 0.25)
                ),
                tabPanel("Prior constraint selector",
                    sliderTextInput("rho", "constraint shape", choices = c(0.1,1,10,100,1000)),
                    sliderInput("maxsize", "maximum size", min = 0, max = 10, value = 10, step = 1),
                    actionButton("add_maxsize", "add size constraint"),
                    DT::dataTableOutput("features"),
                    actionButton("add_must", "add must-link"),
                    actionButton("add_cannot", "add cannot-link"),
                    DT::dataTableOutput("constraints")
                ),
                tabPanel("Feature selection",
                    textOutput("selected_features")
                )
            )
        )
    )
))
