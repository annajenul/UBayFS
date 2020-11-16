library(shiny)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    # INITIAL SETTINGS
    A <- reactiveVal(NULL)
    b <- reactiveVal(NULL)
    rho <- reactiveVal(NULL)
    sel_fs <- reactiveVal(NULL)

    addConstraint <- function(A, b, rho){
        if(is.null(A())){
            newA <- A
            newb <- b
            newrho <- rho

            colnames(newA) <- paste0("A-",names_feats())
        }
        else{
            newA <- rbind(A(), A)
            newb <- c(b(), b)
            newrho <- c(rho(), rho)
        }
        A(newA)
        b(newb)
        rho(newrho)

        proxy = dataTableProxy('features')
        selectRows(proxy, c())
    }

    # FILE INPUT HANDLING
    train_data <- reactive({
        if(is.null(input$train_data)){
            NULL
        }
        else{
            read.csv(input$train_data$datapath)
        }
    })

    train_labels <- reactive({
        if(is.null(input$train_labels)){
            NULL
        }
        else{
            read.csv(input$train_labels$datapath)
        }
    })



    n_feats <- reactive({
        ncol(train_data())
    })

    names_feats <- reactive({
        colnames(train_data())
    })

    # CONSTRAINT INPUT HANDLING
    observeEvent(input$add_maxsize, {
        addConstraint(A = matrix(1, nrow = 1, ncol = n_feats()),
                      b = input$maxsize,
                      rho = input$rho)
    })

    observeEvent(input$add_must, {
        newA = matrix(0, nrow = 2, ncol = n_feats())
        newA[1,input$features_rows_selected] <- c(1,-1)
        newA[2,input$features_rows_selected] <- c(-1,1)
        newb = c(0,0)

        addConstraint(A = newA,
                      b = newb,
                      rho = rep(input$rho, 2))
    })

    observeEvent(input$add_cannot, {
        addConstraint(A = matrix((1:n_feats()) %in% input$features_rows_selected, nrow = 1, ncol = n_feats()),
                      b = 1,
                      rho = input$rho)
    })

    # FEATURE SELECTION
    observeEvent(input$run_RentABay, {
      mod <- RentABay::build.model(train_data(),
                                   train_labels(),
                                   reg.param = list(alpha = input$alpha, lambda = input$lambda),
                                   K = input$K,
                                   testsize = input$testsize,
                                   A = A(),
                                   b = b(),
                                   rho = rho())
      sel_fs(RentABay::selectFeatures(mod))
    })

    # STATUS SETTINGS
    data_complete <- reactive({
      ifelse(is.null(input$train_data) | is.null(input$train_labels), FALSE, TRUE)
    })

    likelihood_complete <- reactive({
      ifelse(!data_complete() | is.null(input$alpha) | is.null(input$lambda) | is.null(input$K) | is.null(input$testsize), FALSE, TRUE)
    })

    prior_complete <- reactive({
      ifelse(!data_complete() | is.null(A()) | is.null(b()) | is.null(rho()), FALSE, TRUE)
    })

    parameters_complete <- reactive({
      ifelse(prior_complete() & likelihood_complete(), TRUE, FALSE)
    })

    featureselection_complete <- reactive({
      ifelse(!parameters_complete() | is.null(sel_fs()), FALSE, TRUE)
    })

    observeEvent(data_complete(), {
      if(data_complete()){
        updatePrettyToggle(session, "status_data", "Data loaded", value = TRUE)
        showTab("tabs", target = "Input data")
        showTab("tabs", target = "RENT parameter selector")
        showTab("tabs", target = "Prior constraint selector")
      }
      else{
        hideTab("tabs", target = "Input data")
        hideTab("tabs", target = "RENT parameter selector")
        hideTab("tabs", target = "Prior constraint selector")
      }
    })

    observeEvent(likelihood_complete(), {
      if(likelihood_complete()){
        updatePrettyToggle(session, "status_likelihood", "RENT parameters set", value = TRUE)
      }
    })

    observeEvent(prior_complete(), {
      if(prior_complete()){
        updatePrettyToggle(session, "status_prior", "Prior constraints set", value = TRUE)
      }
    })

    observeEvent(parameters_complete(), {
      if(parameters_complete()){
        enable("run_RentABay")
      }
      else{
        disable("run_RentABay")
      }
    })

    observeEvent(featureselection_complete(), {
      if(featureselection_complete()){
        updatePrettyToggle(session, "status_featureselection", "Feature selection calculated", value = TRUE)
        showTab("tabs", target = "Feature selection")
        print(paste0("HERE:", sel_fs()))
      }
      else{
        hideTab("tabs", target = "Feature selection")
      }
    })


    # OUTPUT HANDLING
    output$file <- renderText({
        ifelse(is.null(input$train_data),
               "No file selected",
               "File selected")
    })

    output$data <- DT::renderDataTable({
        cbind(train_data(), train_labels())
    })

    output$features <- DT::renderDataTable({
        data.frame(names = colnames(train_data()),
                   t(round(apply(train_data(), 2, summary), 2)))
    })

    output$constraints <- DT::renderDataTable({
        cbind(A(), b = b(), rho = rho())
    })

    output$selected_features <- renderText({
        paste0(names_feats()[which(sel_fs == 1)], collapse = ",")
    })
})
