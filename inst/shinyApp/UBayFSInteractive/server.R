library(shiny)
library(DT)
library(ggplot2)
library(tcltk)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    options(shiny.maxRequestSize=20*1024^2) # maximum upload size

    # help functions
    addConstraint <- function(params){
        if(is.null(model()$user.params$constraints$A)){
            newA <- params$A
            newb <- params$b
            newrho <- params$rho

            colnames(newA) <- names_feats()
        }
        else{
            newA <- rbind(model()$user.params$constraints$A, params$A)
            newb <- c(model()$user.params$constraints$b, params$b)
            newrho <- c(model()$user.params$constraints$rho, params$rho)
        }

        model(set_constraint_params(model(), A = newA, b = newb, rho = as.numeric(newrho)))

        proxy = dataTableProxy('features')
        selectRows(proxy, c())
    }

    addWeights <- function(weights){
      model(set_weight_params(model(), weights))
    }

    FStoString <- function(vec){
      return(paste0("{",
                    paste0(names_feats()[which(vec == 1)], collapse = ","),
                    "}"))
    }

    # error messages
    upload_error <- function(x){
      show_alert(
        title = "Error",
        text = "An error occured during file upload - please check file and try again.",
        type = 'error',
        btn_labels = "Ok"
      )
    }

    demo_error <- function(x){
      show_alert(
        title = "Error",
        text = "An error occured during loading of demo file - please check internet connection and try again.",
        type = 'error',
        btn_labels = "Ok"
      )
    }

    # === INITIAL SETTINGS ===

    model <- reactiveVal(NULL)

    blocks <- reactiveVal(NULL)

    output_width <- reactive({
      ifelse(input$showInput, 8, 12)
    })

    # === FILE INPUT HANDLING ===
    observeEvent(input$upload_data,{
      if(!is.null(input$upload_data)){
        tryCatch({
          header <- ifelse(input$input_colnames, TRUE, FALSE)
          if(input$input_rownames){
            rownames = 1
          }
          else{
            rownames = NULL
          }

          dat <- read.csv(input$upload_data$datapath, header = header, row.names = rownames)

          if(!header){
            colnames(dat) <- paste0("F", 1:ncol(dat))
          }

          if(input$input_blocks){
            blocks(unlist(dat[1,]))
            dat <- dat[-1,]
          }
          else{
            blocks(1:ncol(dat))
          }
          hideTab("input_type", target = "demo")
          model(append(model(), list(data = dat)))
        },
        error = function(cond){
          upload_error(cond)
        },
        warning = function(cond){
          upload_error(cond)
        }
        )
      }
    })

    observeEvent(input$upload_labels,{
      if(!is.null(input$upload_labels)){
        tryCatch({
          header <- ifelse(input$input_colnames, TRUE, FALSE)
          if(input$input_rownames){
            rownames = 1
          }
          else{
            rownames = NULL
          }
          lab <- unlist(read.csv(input$upload_labels$datapath, header = header, row.names = rownames))
          if(!is.null(model()$data)){
            names(lab) <- rownames(model()$data)
          }
          hideTab("input_type", target = "demo")
          model(append(model(), list(target = lab)))
        },
        error = function(cond){
          upload_error(cond)
        },
        warning = function(cond){
          upload_error(cond)
        }
        )
      }
    })

    observeEvent(input$demo_data,{
      dat <- load_wisconsin()

      colnames(dat$data) <- paste0("F", 1:ncol(dat$data))

      model(append(model(), list(data = dat$data)))
      model(append(model(), list(target = dat$labels)))

      hideTab("input_type", target = "upload")

      blocks(1:ncol(model()$data))
      }
    )

    n_feats <- reactive({
        ncol(model()$data)
    })

    names_feats <- reactive({
        colnames(model()$data)
    })

    output$save_model <- downloadHandler(
      filename <- function(){
        paste0("UBay_model_",Sys.Date(),".Rdata")
      },

      content <- function(file){
        isolate(UBay_model <<- model())
        save(UBay_model, file = file)
      }
    )

    observeEvent(input$load_model,{
      path = tk_choose.files(multi = FALSE, filter = t(c("RData",".Rdata")))
      if(!is.null(path) & length(path) > 0){
        env = new.env()
        load(path, env)
        model(env[["UBay_model"]])
        blocks(1:ncol(model()$data))
      }
    })

    # === LIKELIHOOD INPUT HANDLING ===
    observeEvent(input$confirmParam, {
      withProgress(min = 0, max = 1, value = 0, message = "building elementary models", {
        model(UBayFS::build.model(model()$data,
                                     model()$target,
                                     M = input$M,
                                     tt_split = input$tt_split,
                                     A = model()$user.params$constraints$A,
                                     b = model()$user.params$constraints$b,
                                     rho = model()$user.params$constraints$rho,
                                     method = input$method,
                                     nr_features = input$n_feats,
                                     shiny = TRUE))
      })
    })

    # === PRIOR INPUT HANDLING ===
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
      if(any(sapply(names(input), grepl, pattern = "blockweight"))){
        blockweights <- sapply(paste0("blockweight_", unique(blocks())), function(x){return(input[[x]])})
        addWeights(as.numeric(blockweights)[blocks()])
      }
    })

    # === FEATURE SELECTION ====
    observeEvent(input$popsize | input$maxiter, {
      if(!is.null(model())){
        model(set_optim_params(model(), popsize = input$popsize, maxiter = input$maxiter))
      }
    })

    observeEvent(input$run_UBay, {
      withProgress(min = 0, max = 1, value = 0, message = "optimizing posterior function", {
        model(UBayFS::train_model(model()))
      })
    })

    # === STATUS SETTINGS ===
    data_complete <- reactive({
      ifelse(is.null(model()$data) | is.null(model()$target), FALSE, TRUE)
    })

    likelihood_complete <- reactive({
      ifelse(!data_complete() | is.null(model()$ensemble.params$output$counts), FALSE, TRUE)
    })

    weighting_complete <- reactive({
      ifelse(!data_complete() | is.null(model()$user.params$weights), FALSE, TRUE)
    })

    prior_complete <- reactive({
      ifelse(!data_complete() | is.null(model()$user.params$constraints), FALSE, TRUE)
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
        enable("add_maxsize")
        enable("add_must")
        enable("add_cannot")
        updateSliderInput(session, "maxsize", min = 0, max = n_feats(), value = min(10, n_feats()))
      }
    })

    observe({
      if(data_complete() & length(input$method) > 0){
        enable("confirmParam")
      }
      else{disable("confirmParam")}
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
          data.frame(label = model()$target, model()$data),
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
            feature = names_feats()
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
      if(!is.null(model()$user.params$weights)){
        ggplot2::ggplot(
          data = data.frame(features = factor(names_feats(), levels = names_feats()),
                            counts = model()$user.params$weights),
          aes(x = features,
              y = counts))+
          geom_bar(stat = "identity", position = position_dodge(),
                   fill = "blue")+
          theme(axis.text.x = element_text(angle = 90))
      }
    )

    output$constraints <- renderUI({
      if(!is.null(model()$user.params$constraints$A)){
        matA <- paste0(apply(model()$user.params$constraints$A, 1, function(x){paste0(x, collapse = " & ")}), collapse = "\\\\")
        matb <- paste0(model()$user.params$constraints$b, collapse = "\\\\")
        matrho <- paste0(ifelse(model()$user.params$constraints$rho == Inf, "\\infty", model()$user.params$constraints$rho), collapse = "\\\\")

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



    output$blocktable <- renderUI({
      blockweights <- model()$user.params$weights
      lapply(unique(blocks()), function(block_no){
        block_name <- unique(blocks())[block_no]
        fluidRow(
          column(3,
                textInput(inputId = paste0("blockweight_", block_name),
                  label = NULL,
                  value = ifelse(is.null(blockweights[block_no]), 1, blockweights[block_no]),
                  width = '50px')
          ),
          column(9,
                 paste0(names_feats()[blocks() == block_name], collapse = ", "),
                 align = "left"
          )
        )
      })
    })

    output$rho_plot <- renderPlot({
      x <- seq(-10,10,by = 0.01)
      ggplot2::ggplot(data = data.frame(x = x, y = sapply(x, admissibility, A = matrix(1, nrow = 1, ncol = 1), b = 0, rho = input$rho, log = FALSE)),
                      aes(x = x, y = y, group = 1)) +
        geom_line() +
        xlab(label = "ax-b") +
        ylab(label = "prior prob.")
    })


    output$output_data <- renderUI({
      column(output_width(),
             DT::dataTableOutput("data",
                                 width = "90%"),
             align = 'center'
      )
    })

    output$output_likelihood <- renderUI({
      column(output_width(),
             plotOutput("count_hist", height = '300px'),
             align = 'center'
      )
    })

    output$likelihood_sliders <- renderUI({

      if(is.null(model()$ensemble.params$input$method)){
        sel_method = "mRMR"
      }
      else{
        sel_method <- model()$ensemble.params$input$method
      }

      sel_M <- ifelse(is.null(model()$ensemble.params$input$M), 10, model()$ensemble.params$input$M)
      sel_tt_split <- ifelse(is.null(model()$ensemble.params$input$tt_split), 0.75, model()$ensemble.params$input$tt_split)
      sel_nr_features <- ifelse(is.null(model()$ensemble.params$input$nr_features), 10, model()$ensemble.params$input$nr_features)
      max_nr_features <- ifelse(is.null(n_feats()), 10, n_feats())

      column(12,
        pickerInput("method", "select elementary filter(s)",
                  choices = c("mRMR",
                              "Laplacian score"),
                  selected = sel_method,
                  multiple = TRUE),
        sliderTextInput("M", withMathJax('$$M$$'), choices = c(10, 20, 30, 40, 50,
                                                               60, 70, 80, 90, 100,
                                                               150, 200, 250, 300, 350, 400, 450, 500,
                                                               600, 700, 800, 900, 1000),
                      selected = sel_M),
        sliderInput("n_feats", "number of features", min = 1, max = max_nr_features, step = 1, value = sel_nr_features),
        sliderInput("tt_split", "train-test-split", min = 0.5, max = 0.95, step = 0.05, value = sel_tt_split)
      )
    })

    output$output_weights <- renderUI({
      column(output_width(),
             plotOutput("weight_hist", height = '300px'),
             align = 'center'
      )
    })


    output$output_constraints <- renderUI({
      column(output_width(),
             uiOutput("constraints"),
             align = 'center'
      )
    })


    output$fs_sliders <- renderUI({

      sel_popsize <- ifelse(is.null(model()$optim.params$popsize), 1000, model()$optim.params$popsize)
      sel_maxiter <- ifelse(is.null(model()$optim.params$maxiter), 100, model()$optim.params$maxiter)

      column(12,
        sliderTextInput("popsize", withMathJax('$$q$$'), choices = c(10, 50, 100, 500, 1000, 5000, 10000),
                      selected = sel_popsize),
        sliderTextInput("maxiter", withMathJax('$$T$$'), choices = c(10, 20, 30, 40, 50,
                                                                     60, 70, 80, 90, 100,
                                                                     150, 200, 250, 300, 350, 400, 450, 500,
                                                                     600, 700, 800, 900, 1000),
                      selected = sel_maxiter)
      )
    })

    output$output_fs <- renderUI({
      column(output_width(),
             plotOutput("result_barplot", height = "300px"),
             DT::dataTableOutput("feature_results"),
             align = 'center'
      )
    })

    output$feature_results <- DT::renderDataTable(
      if(!is.null(model()$output$map)){
        datatable(
          data.frame(
            map = FStoString(model()$output$map),
            cardinality = sum(model()$output$map)),
          options = list(paging = FALSE, sDom = '<"top">rt<"bottom">ip', scrollX = TRUE, scrollY = "75px"),
          selection = "none"
        )
      }
    )

    output$result_barplot <- renderPlot({
      if(!is.null(model())){
        plot(model())
      }
    })

})
