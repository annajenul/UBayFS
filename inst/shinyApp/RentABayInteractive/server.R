library(shiny)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    # INITIAL SETTINGS
    A <- reactiveVal(NULL)
    b <- reactiveVal(NULL)
    rho <- reactiveVal(NULL)

    alpha <- reactiveVal(NULL)
    lambda <- reactiveVal(NULL)
    K <- reactiveVal(NULL)
    testsize <- reactiveVal(NULL)

    optim_probs <- reactiveVal(NULL)
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

    # LIKELIHOOD INPUT HANDLING
    observeEvent(input$confirmParam, {
      alpha(input$alpha)
      lambda(input$lambda)
      K(input$K)
      testsize(input$testsize)
    })

    # CONSTRAINT INPUT HANDLING
    observeEvent(input$add_maxsize, {
        addConstraint(A = matrix(1, nrow = 1, ncol = n_feats()),
                      b = input$maxsize,
                      rho = input$rho)
    })

    observeEvent(input$add_must, {
        sel <- input$features_rows_selected

        if(length(sel) > 1){
          pairs <- expand.grid(sel, sel) # all pairs
          pairs <- pairs[-which(pairs[,1] == pairs[,2]),] # delete main diagonal

          newA <- t(apply(pairs, 1, function(x){return(((1:n_feats()) == x[1]) - ((1:n_feats()) == x[2]))}))

          addConstraint(A = newA,
                        b = rep(0, nrow(newA)),
                        rho = rep(input$rho, nrow(newA)))
        }
    })

    observeEvent(input$add_cannot, {
        sel <- input$features_rows_selected

        if(length(sel) > 1){
          pairs <- expand.grid(sel, sel) # all pairs
          pairs <- pairs[-which(pairs[,1] <= pairs[,2]),] # delete main diagonal & lower triangle

          newA = t(apply(pairs, 1, function(x){return(((1:n_feats()) == x[1]) + ((1:n_feats()) == x[2]))}))
          addConstraint(A = newA,
                        b = rep(1, nrow(newA)),
                        rho = rep(input$rho, nrow(newA)))
        }
    })

    # FEATURE SELECTION
    observeEvent(input$run_RentABay, {
      withProgress(message = 'preparing model', value = 0, {
        incProgress(0, detail = paste("Building RENT model"))
        mod <- RentABay::build.model(train_data(),
                                     train_labels(),
                                     reg.param = list(alpha = input$alpha, lambda = input$lambda),
                                     K = input$K,
                                     testsize = input$testsize,
                                     A = A(),
                                     b = b(),
                                     rho = rho(),
                                     verbose = FALSE)
        incProgress(0, detail = paste("optimizing posterior"))
        fs <- RentABay::selectFeatures(mod)
      })
      sel_fs(fs@solution)
      optim_probs(posterior(sel_fs), mod$likelihood.params, mod$prior.params)
    })

    # STATUS SETTINGS
    data_complete <- reactive({
      ifelse(is.null(input$train_data) | is.null(input$train_labels), FALSE, TRUE)
    })

    likelihood_complete <- reactive({
      ifelse(!data_complete() | is.null(alpha()) | is.null(lambda()) | is.null(K()) | is.null(testsize()), FALSE, TRUE)
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
        updatePrettyToggle(session, "status_data", "data loaded", value = TRUE)
        showTab("tabs", target = "input")
        showTab("tabs", target = "likelihood parameters")
        showTab("tabs", target = "prior parameters")
      }
      else{
        hideTab("tabs", target = "input")
        hideTab("tabs", target = "likelihood parameters")
        hideTab("tabs", target = "prior parameters")
      }
    })

    observeEvent(likelihood_complete(), {
      if(likelihood_complete()){
        updatePrettyToggle(session, "status_likelihood", "likelihood set", value = TRUE)
      }
    })

    observeEvent(prior_complete(), {
      if(prior_complete()){
        updatePrettyToggle(session, "status_prior", "prior set", value = TRUE)
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
        updatePrettyToggle(session, "status_featureselection", "optimal features calculated", value = TRUE)
        showTab("tabs", target = "feature selection")
      }
      else{
        hideTab("tabs", target = "feature selection")
      }
    })


    # OUTPUT HANDLING
    output$data <- DT::renderDataTable(
      datatable(
        cbind(train_data(), train_labels()),
        filter = 'top',
        options = list(pageLength = 20, sDom = '<"top">rt<"bottom">ip'),
        selection = "none"
      )
    )

    output$features <- DT::renderDataTable(
      datatable(
        data.frame(
            min = round(apply(train_data(), 2, min), 2),
            mean = round(apply(train_data(), 2, mean), 2),
            median = round(apply(train_data(), 2, median), 2),
            max = round(apply(train_data(), 2, max), 2)
        ),
        filter = 'top',
        options = list(pageLength = 10)
      )
    )

    output$params <- renderUI(
      if(likelihood_complete()){
        withMathJax(
          sprintf('$$\\alpha= %.03f,~ \\lambda= %.03f$$
                  $$K= %.03f,~ testsize= %.03f$$',
                  input$alpha,input$lambda,input$K,input$testsize)
        )
      }
      else{
        paste0("No parameters set yet")
      }
    )


    output$constraints <- DT::renderDataTable(
      datatable(
        cbind(A = A(), b = b(), rho = rho()),
        options = list(autoWidth = TRUE, pageLength = 10, sDom = '<"top">rt<"bottom">ip'),
        rownames = FALSE,
        selection = "none"
      )
    )

    output$selected_features <- renderText({
        paste0(names_feats()[which(sel_fs == 1)], collapse = ",")
    })

    output$probabilities <- DT::renderDataTable({
        optim_probs()
    })
})
