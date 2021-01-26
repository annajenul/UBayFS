library(shiny)
library(DT)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    options(shiny.maxRequestSize=20*1024^2) # maximum upload size

    # help functions
    addConstraint <- function(params){
        if(is.null(A())){
            newA <- params$A
            newb <- params$b
            newrho <- params$rho

            colnames(newA) <- names_feats()
        }
        else{
            newA <- rbind(A(), params$A)
            newb <- c(b(), params$b)
            newrho <- c(rho(), params$rho)
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

    # === INITIAL SETTINGS ===
    A <- reactiveVal(NULL)
    b <- reactiveVal(NULL)
    rho <- reactiveVal(NULL)

    model <- reactiveVal(NULL)

    blocks <- reactiveValues(names = NULL, vector = NULL, weights = NULL)

    train_data <- reactiveVal(NULL)
    train_labels <- reactiveVal(NULL)

    # === FILE INPUT HANDLING ===
    observeEvent(input$train_data,{
        if(!is.null(input$train_data)){
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
          else{
            blocks$vector <- 1:ncol(dat)
            blocks$names <- 1:ncol(dat)
          }
          train_data(dat)
        }
    })

    observeEvent(input$train_labels, {
       if(!is.null(input$train_labels)){
          header <- ifelse(input$input_colnames, TRUE, FALSE)
          if(input$input_rownames){
            rownames = 1
          }
          else{
            rownames = NULL
          }
          lab <- unlist(read.csv(input$train_labels$datapath, header = header, row.names = rownames))
          if(!is.null(train_data())){
            names(lab) <- rownames(train_data())
          }
          train_labels(lab)
        }
    })

    observeEvent(input$demo_data,{
      dat <- load_wisconsin()
      train_data(dat$data)
      train_labels(dat$labels)
      blocks$vector <- 1:ncol(train_data())
      blocks$names <- 1:ncol(train_data())
    }
    )

    n_feats <- reactive({
        ncol(train_data())
    })

    names_feats <- reactive({
        colnames(train_data())
    })

    # === LIKELIHOOD INPUT HANDLING ===
    observeEvent(input$confirmParam, {
      withProgress(min = 0, max = 1, value = 0, message = "building elementary models", {
        model(UBay::build.model(train_data(),
                                     train_labels(),
                                     M = input$M,
                                     tt_split = input$tt_split,
                                     A = NULL,
                                     b = NULL,
                                     rho = NULL,
                                     method = input$method,
                                     nr_features = input$n_feats,
                                     shiny = TRUE))
      })
    })

    observe({
      if(length(input$method) > 0){
        enable("confirmParam")
      }
      else{disable("confirmParam")}
    })

    # === CONSTRAINT INPUT HANDLING ===
    observeEvent(input$add_maxsize, {
        addConstraint(build_constraints("max_size", list(input$maxsize), num_features = n_feats(), rho = input$rho))
    })

    observeEvent(input$add_must, {
        sel <- input$features_rows_selected

        if(length(sel) > 1){
          addConstraint(build_constraints("must_link", list(sel), num_features = n_feats(), rho = input$rho))
        }
    })

    observeEvent(input$add_cannot, {
        sel <- input$features_rows_selected

        if(length(sel) > 1){
          addConstraint(build_constraints("cannot_link", list(sel), num_features = n_feats(), rho = input$rho))
        }
    })

    observe({
      blockweights <- sapply(paste0("blockweight_", blocks$names), function(x){return(input[[x]])})

      if(!is.null(unlist(blockweights))){
        blockweights <- as.numeric(unlist(blockweights))
        blocks$weights = blockweights[blocks$vector]
        model(set_weight_params(model(), blocks$weights))
      }
    })

    # === FEATURE SELECTION ====
    observeEvent(input$run_UBay, {
      model(UBay::set_constraint_params(model(), A(), b(), as.numeric(rho())))

      withProgress(min = 0, max = 1, value = 0, message = "optimizing posterior function", {
        model(UBay::train_model(model()))
      })
    })

    # === STATUS SETTINGS ===
    data_complete <- reactive({
      ifelse(is.null(train_data()) | is.null(train_labels()), FALSE, TRUE)
    })

    likelihood_complete <- reactive({
      ifelse(!data_complete() | is.null(model()), FALSE, TRUE)
    })

    weighting_complete <- reactive({
      ifelse(!data_complete() | is.null(blocks$weights), FALSE, TRUE)
    })

    prior_complete <- reactive({
      ifelse(!data_complete() | is.null(A()) | is.null(b()) | is.null(rho()), FALSE, TRUE)
    })

    parameters_complete <- reactive({
      ifelse(prior_complete() & likelihood_complete(), TRUE, FALSE)
    })

    featureselection_complete <- reactive({
      ifelse(!parameters_complete() | is.null(model()$output), FALSE, TRUE)
    })

    observeEvent(data_complete(), {
      if(data_complete()){
        updatePrettyToggle(session, "status_data", "input ok", value = TRUE)
        showTab("tabs", target = "likelihood")
        showTab("tabs", target = "constraints")
        showTab("tabs", target = "weights")
        showTab("tabs", target = "feature selection")
        updateSliderInput(session, "maxsize", value = min(10, n_feats()), max = n_feats(), step = 1)
        updateSliderInput(session, "n_feats", value = min(10, n_feats()), max = n_feats(), step = 1)
      }
      else{
        hideTab("tabs", target = "likelihood")
        hideTab("tabs", target = "constraints")
        hideTab("tabs", target = "weights")
        hideTab("tabs", target = "feature selection")
      }
    })

    observeEvent(likelihood_complete(), {
      if(likelihood_complete()){
        updatePrettyToggle(session, "status_likelihood", "likelihood setting ok", value = TRUE)
      }
    })

    observeEvent(weighting_complete(), {
      if(weighting_complete()){
        updatePrettyToggle(session, "status_weighting", "weight setting ok", value = TRUE)
      }
    })

    observeEvent(prior_complete(), {
      if(prior_complete()){
        updatePrettyToggle(session, "status_prior", "constraint setting ok", value = TRUE)
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
      }
    })


    # === OUTPUT HANDLING ====
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

    output$count_hist <- renderPlot(
      if(!is.null(model()$ensemble.params)){
        ggplot2::ggplot(
          data = data.frame(features = factor(names_feats(), levels = names_feats()),
                            counts = model()$ensemble.params$output$counts),
          aes(x = features,
              y = counts))+
        geom_bar(stat = "identity", position = position_dodge(),
                 fill = "red")+
        theme(axis.text.x = element_text(angle = 90))
      }
    )

    output$weight_hist <- renderPlot(
      if(!is.null(blocks$weights)){
        ggplot2::ggplot(
          data = data.frame(features = factor(names_feats(), levels = names_feats()),
                            counts = as.numeric(blocks$weights)),
          aes(x = features,
              y = counts))+
          geom_bar(stat = "identity", position = position_dodge(),
                   fill = "blue")+
          theme(axis.text.x = element_text(angle = 90))
      }
    )

    output$constraints <- renderUI({
      if(!is.null(A()) & !is.null(b())){
        matA <- paste0(apply(A(), 1, function(x){paste0(x, collapse = " & ")}), collapse = "\\\\")
        matb <- paste0(b(), collapse = "\\\\")
        matrho <- paste0(ifelse(rho() == Inf, "\\infty", rho()), collapse = "\\\\")

        withMathJax(
          paste0("$$\\begin{pmatrix}",
          matA,
          "\\end{pmatrix} x
          \\leq
          \\begin{pmatrix}",
          matb,
          "\\end{pmatrix},~ \\\\",
          "\\rho = ",
          "\\begin{pmatrix}",
          matrho,
          "\\end{pmatrix}$$")
        )
      }
    })

    output$feature_results <- DT::renderDataTable(
      if(!is.null(model()$output$map)){
        datatable(
          data.frame(
            map = FStoString(model()$output$map),
            cardinality = sum(model()$output$map)),
          options = list(paging = FALSE, sDom = '<"top">rt<"bottom">ip', scrollX = TRUE, scrollY = "200px"),
          selection = "none"
        )
      }
    )

    output$blocks <- DT::renderDataTable(
      datatable(
        data.frame(weight = sapply(blocks$names, function(x){return(as.character(textInput(inputId = paste0("blockweight_", x),
                                                                                           label = NULL, value = 1, width = '50px')))}),
                   block = blocks$names,
                   features = sapply(blocks$names, function(x){return(paste0(names(train_data())[blocks$vector == x], collapse = ","))})
                   ),
        escape = FALSE,
        options = list(paging = FALSE, sDom = '<"top">rt<"bottom">ip', scrollX = TRUE, scrollY = "300px",
                       preDrawCallback = JS('function() {Shiny.unbindAll(this.api().table().node()); }'),
                       drawCallback = JS('function() {Shiny.bindAll(this.api().table().node()); } ') ),
        selection = "none",
        rownames = FALSE
      ),
      server = FALSE
    )

    output$rho_plot <- renderPlot({
      x <- seq(-10,10,by = 0.01)
      ggplot2::ggplot(data = data.frame(x = x, y = sapply(x, admissibility, A = matrix(1, nrow = 1, ncol = 1), b = 0, rho = input$rho, log = FALSE)),
                      aes(x = x, y = y, group = 1)) +
        geom_line() +
        xlab(label = "ax-b") +
        ylab(label = "prior prob.")
    })

    output$result_barplot <- renderPlot({
      if(!is.null(model()$output$map)){
        df <- data.frame(
          feature = factor(rep(names_feats(),3), levels = names_feats()),
          value = c(model()$output$map,
                    model()$ensemble.params$output$counts / max(model()$ensemble.params$output$counts),
                    model()$user.params$weights / max(model()$user.params$weights)),
          type = factor(rep(c("map", "ensemble", "prior"),each = length(names_feats())), levels = c("map", "ensemble", "prior"))
        )
        ggplot2::ggplot(data = df,
                      aes(x = feature, y = value, group = type, fill = type)) +
          geom_bar(stat = "identity", position = position_dodge())+
          theme(axis.text.x = element_text(angle = 90))
      }
    })

})
