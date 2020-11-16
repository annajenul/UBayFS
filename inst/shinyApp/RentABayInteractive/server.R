library(shiny)
library(DT)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    # help functions
    addConstraint <- function(A, b, rho){
        if(is.null(A())){
            newA <- A
            newb <- b
            newrho <- rho

            colnames(newA) <- names_feats()
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

    FStoString <- function(vec){
      return(paste0("{",
                    paste0(names_feats()[which(vec == 1)], collapse = ","),
                    "}"))
    }

    rho_fct <- function(x, rho){
      eterm <- exp(-rho * x)
      y <- eterm / (1+eterm)
      return(y)
    }

    # INITIAL SETTINGS
    A <- reactiveVal(NULL)
    b <- reactiveVal(NULL)
    rho <- reactiveVal(NULL)

    model <- reactiveVal(NULL)

    optim_fs <- reactiveVal(NULL)


    # FILE INPUT HANDLING
    train_data <- reactive({
        if(is.null(input$train_data)){
            NULL
        }
        else{
            read.csv(input$train_data$datapath, row.names = 1)
        }
    })

    train_labels <- reactive({
        if(is.null(input$train_labels)){
            NULL
        }
        else{
            unlist(read.csv(input$train_labels$datapath, row.names = 1))
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
      withProgress(min = 0, max = 1, value = 0, message = "building RENT model", {
        model(RentABay::build.model(train_data(),
                                     train_labels(),
                                     reg.param = list(alpha = input$alpha, lambda = input$lambda),
                                     K = input$K,
                                     testsize = input$testsize,
                                     A = NULL,
                                     b = NULL,
                                     rho = NULL,
                                     verbose = FALSE))
      })
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
      model(set_prior_params(model(), A(), b(), rho()))

      withProgress(min = 0, max = 1, value = 0, message = "optimizing posterior function", {
        fs <- RentABay::selectFeatures(model())
      })

      optim_fs(fs@solution)
    })

    # STATUS SETTINGS
    data_complete <- reactive({
      ifelse(is.null(input$train_data) | is.null(input$train_labels), FALSE, TRUE)
    })

    likelihood_complete <- reactive({
      ifelse(!data_complete() | is.null(model()), FALSE, TRUE)
    })

    prior_complete <- reactive({
      ifelse(!data_complete() | is.null(A()) | is.null(b()) | is.null(rho()), FALSE, TRUE)
    })

    parameters_complete <- reactive({
      ifelse(prior_complete() & likelihood_complete(), TRUE, FALSE)
    })

    featureselection_complete <- reactive({
      ifelse(!parameters_complete() | is.null(optim_fs()), FALSE, TRUE)
    })

    observeEvent(data_complete(), {
      if(data_complete()){
        updatePrettyToggle(session, "status_data", "input ok", value = TRUE)
        showTab("tabs", target = "input")
        showTab("tabs", target = "likelihood parameters")
        showTab("tabs", target = "prior parameters")
        updateSliderInput(session, "maxsize", value = min(10, n_feats()), max = n_feats(), step = 1)
      }
      else{
        hideTab("tabs", target = "input")
        hideTab("tabs", target = "likelihood parameters")
        hideTab("tabs", target = "prior parameters")
      }
    })

    observeEvent(likelihood_complete(), {
      if(likelihood_complete()){
        updatePrettyToggle(session, "status_likelihood", "likelihood setting ok", value = TRUE)
      }
    })

    observeEvent(prior_complete(), {
      if(prior_complete()){
        updatePrettyToggle(session, "status_prior", "prior setting ok", value = TRUE)
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
        options = list(pageLength = 10, sDom = '<"top">rt<"bottom">ip', scrollX = TRUE),
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
        options = list(pageLength = 5, scrollX = TRUE)
      )
    )

    output$params <- renderUI(
      if(likelihood_complete()){
        withMathJax(
          paste0("$$\\alpha= ",
                input$alpha,
                ",~\\lambda=",
                input$lambda,
                "$$",
                "\n",
                "$$K= ",
                input$K,
                ",~\\text{testsize}= ",
                input$testsize,
                "$$")
        )
      }
      else{
        paste0("no parameters set")
      }
    )

    output$constraints <- renderUI({
      if(is.null(A()) | is.null(b())){
        paste0("no constraints set")
      }
      else{
        matA <- paste0(apply(A(), 1, function(x){paste0(x, collapse = " & ")}), collapse = "\\\\")
        matb <- paste0(b(), collapse = "\\\\")

        withMathJax(
          paste0("$$\\begin{pmatrix}",
          matA,
          "\\end{pmatrix} x
          \\leq
          \\begin{pmatrix}",
          matb,
          "\\end{pmatrix}$$")
        )
      }
    })



    output$feature_results <- DT::renderDataTable(
      datatable(
        data.frame(set = apply(optim_fs(), 1, FStoString),
                   cardinality = apply(optim_fs(), 1, sum),
                   posterior = round(apply(optim_fs(), 1, posterior, likelihood.params = model()$likelihood.params, prior.params = model()$prior.params),4),
                   likelihood = round(apply(optim_fs(), 1, likelihood, likelihood.params = model()$likelihood.params),4),
                   prior = round(apply(optim_fs(), 1, prior, prior.params = model()$prior.params),4)),
        options = list(autoWidth = TRUE, pageLength = 10, sDom = '<"top">rt<"bottom">ip', scrollX = TRUE),
        selection = "none"
      )
    )

    output$rho_plot <- renderPlot({
      x <- seq(-1,1,by = 0.01)
      ggplot2::ggplot(data = data.frame(x = x, y = rho_fct(x, input$rho)), aes(x = x, y = y, group = 1)) +
        geom_line() +
        xlab(label = "ax-b") +
        ylab(label = "prior prob.")
    })
})
