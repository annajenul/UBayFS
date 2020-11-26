library(shiny)
library(DT)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    options(shiny.maxRequestSize=20*1024^2) # maximum upload size

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

    tableInput = function(FUN, len) {
      inputs = character(len)
      for (i in seq_len(len)) {
        inputs[i] = as.character(FUN(paste0("blockweight", i), label = NULL, value = 1, width = '50px'))
      }
      inputs
    }

    # INITIAL SETTINGS
    A <- reactiveVal(NULL)
    b <- reactiveVal(NULL)
    rho <- reactiveVal(NULL)

    model <- reactiveVal(NULL)

    optim_fs <- reactiveVal(NULL)

    blocks <- reactiveValues(names = NULL, vector = NULL)

    # FILE INPUT HANDLING
    train_data <- reactive({
        if(is.null(input$train_data)){
            NULL
        }
        else{
          header <- ifelse(input$input_colnames, TRUE, FALSE)
          if(input$input_rownames){
            rownames = 1
          }
          else{
            rownames = NULL
          }

          dat <- read.csv(input$train_data$datapath, header = header, row.names = rownames)

          if(input$input_blocks){
            blocks$vector <- unlist(dat[1,])
            blocks$names <- unique(blocks$vector)
            dat <- dat[-1,]
          }
          dat
        }
    })

    train_labels <- reactive({
        if(is.null(input$train_labels)){
            NULL
        }
        else{
          header <- ifelse(input$input_colnames, TRUE, FALSE)
          if(input$input_rownames){
            rownames = 1
          }
          else{
            rownames = NULL
          }
          lab <- unlist(read.csv(input$train_labels$datapath, header = header, row.names = rownames))
        }
        if(!is.null(train_data())){
          names(lab) <- rownames(train_data())
        }
        lab
    })

    n_feats <- reactive({
        ncol(train_data())
    })

    names_feats <- reactive({
        colnames(train_data())
    })

    # LIKELIHOOD INPUT HANDLING
    observeEvent(input$confirmParam, {
      withProgress(min = 0, max = 1, value = 0, message = "building elementary models", {
        model(UBay::build.model(train_data(),
                                     train_labels(),
                                     reg.param = list(alpha = input$enet.alpha, lambda = input$enet.lambda),
                                     K = input$K,
                                     testsize = input$testsize,
                                     A = NULL,
                                     b = NULL,
                                     rho = NULL,
                                     verbose = FALSE,
                                     method = input$method,
                                     ranking = input$ranking,
                                     nr_features = input$n_feats))
      })
    })

    observe({
      if("elastic net" %in% input$method){
        showTab("single_model_parameters", target = "elastic net")
      }
      else{
        hideTab("single_model_parameters", target = "elastic net")
      }

      if(length(input$method) > 0){
        enable("confirmParam")
      }
      else{disable("confirmParam")}
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

    observeEvent(input$add_xor, {
      sel <- input$features_rows_selected

      if(length(sel) > 1){
        pairs <- expand.grid(sel, sel) # all pairs
        pairs <- pairs[-which(pairs[,1] <= pairs[,2]),] # delete main diagonal & lower triangle

        newA = t(apply(pairs, 1, function(x){return(((1:n_feats()) == x[1]) + ((1:n_feats()) == x[2]))}))
        newA = rbind(newA, t(apply(pairs, 1, function(x){return(-((1:n_feats()) == x[1]) - ((1:n_feats()) == x[2]))})))
        addConstraint(A = newA,
                      b = rep(c(1,-1), each = nrow(newA) / 2),
                      rho = rep(input$rho, nrow(newA)))
      }
    })

    observeEvent(input$add_alo, {
      sel <- input$features_rows_selected

      if(length(sel) > 1){
        pairs <- expand.grid(sel, sel) # all pairs
        pairs <- pairs[-which(pairs[,1] <= pairs[,2]),] # delete main diagonal & lower triangle

        newA = t(apply(pairs, 1, function(x){return(-((1:n_feats()) == x[1]) - ((1:n_feats()) == x[2]))}))
        addConstraint(A = newA,
                      b = rep(-1, nrow(newA)),
                      rho = rep(input$rho, nrow(newA)))
      }
    })

    observeEvent(input$add_block, {
      sel <- input$features_rows_selected

      input_names <- names(input)[which(grepl("blockweight", names(input)))]
      blockweights <- c()
      for(n in input_names){
        blockweights <- c(blockweights, input$paste0(n))
      }
      print(input_names)
      print(blockweights)

      stop()
      weight_order <- sapply(names(blockweights), function(x){return(strsplit(x,split = "blockweight")[[1]][2])})
      blockweights <- unlist(blockweights)[order(weight_order)]

      print(blockweights)
      newA = - (blockweights[sapply(blocks$vector, function(x){return(which(blockweights == x))})])
      addConstraint(A = newA,
                      b = rep(1, nrow(newA)),
                      rho = rep(input$rho, nrow(newA)))
    })

    # FEATURE SELECTION
    observeEvent(input$run_UBay, {
      model(UBay::set_prior_params(model(), A(), b(), rho()))

      withProgress(min = 0, max = 1, value = 0, message = "optimizing posterior function", {
        fs <- UBay::selectFeatures(model())
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
        showTab("tabs", target = "data matrix")
        showTab("tabs", target = "likelihood parameters")
        showTab("tabs", target = "prior parameters")
        updateSliderInput(session, "maxsize", value = min(10, n_feats()), max = n_feats(), step = 1)
        updateSliderInput(session, "n_feats", value = min(10, n_feats()), max = n_feats(), step = 1)
      }
      else{
        hideTab("tabs", target = "data matrix")
        hideTab("tabs", target = "likelihood parameters")
        hideTab("tabs", target = "prior parameters")
      }
    })

    observeEvent(likelihood_complete(), {
      if(likelihood_complete()){
        updatePrettyToggle(session, "status_likelihood", "likelihood setting ok", value = TRUE)
        showTab("tabs", target = "likelihood counts")
      }
      else{
        hideTab("tabs", target = "likelihood counts")
      }
    })

    observeEvent(prior_complete(), {
      if(prior_complete()){
        updatePrettyToggle(session, "status_prior", "prior setting ok", value = TRUE)
        showTab("tabs", target = "prior constraints")
      }
      else{
        hideTab("tabs", target = "prior constraints")
      }
    })

    observeEvent(parameters_complete(), {
      if(parameters_complete()){
        enable("run_UBay")
      }
      else{
        disable("run_UBay")
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
      if(data_complete()){
        datatable(
          data.frame(label = train_labels(), train_data()),
          filter = 'top',
          extensions = "FixedColumns",
          options = list(paging = FALSE, sDom = '<"top">rt<"bottom">ip', scrollX = TRUE, scrollY = "400px", fixedColumns = list(leftColumns = 2)),
          selection = "none"
        )
      }
    )

    output$features <- DT::renderDataTable(
      datatable(
        data.frame(
            feature = colnames(train_data())
        ),
        options = list(paging = FALSE, scrollX = TRUE, scrollY = "200px")
      )
    )

    output$counts <- DT::renderDataTable(
      if(!is.null(model())){
        datatable(
          rbind(model()$likelihood.params$full_counts,
                sum = apply(model()$likelihood.params$full_counts, 2, sum)),
          options = list(paging = FALSE, sDom = '<"top">rt<"bottom">ip', scrollX = TRUE, scrollY = "400px"),
          selection = "none"
        )
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
                   posterior = round(apply(optim_fs(), 1, UBay::posterior, likelihood.params = model()$likelihood.params, prior.params = model()$prior.params),4),
                   likelihood = round(apply(optim_fs(), 1, UBay::likelihood, likelihood.params = model()$likelihood.params),4),
                   prior = round(apply(optim_fs(), 1, UBay::prior, prior.params = model()$prior.params),4)),
        options = list(paging = FALSE, sDom = '<"top">rt<"bottom">ip', scrollX = TRUE, scrollY = "200px"),
        selection = "none"
      )
    )

    output$blocks <- DT::renderDataTable(
      datatable(
        data.frame(weight = tableInput(textInput,length(blocks$names)),
                   block = blocks$names,
                   features = sapply(blocks$names, function(x){return(paste0(names(train_data())[blocks$vector == x], collapse = ","))})
                   ),
        escape = FALSE,
        options = list(paging = FALSE, sDom = '<"top">rt<"bottom">ip', scrollX = TRUE, scrollY = "300px"),
        selection = "none",
        rownames = FALSE
      )
    )

    output$rho_plot <- renderPlot({
      x <- seq(-1,1,by = 0.01)
      ggplot2::ggplot(data = data.frame(x = x, y = rho_fct(x, input$rho)), aes(x = x, y = y, group = 1)) +
        geom_line() +
        xlab(label = "ax-b") +
        ylab(label = "prior prob.")
    })

    output$textbox <- renderText({
      input$blockweight1
    })
})
