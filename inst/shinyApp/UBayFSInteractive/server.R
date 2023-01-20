library(shiny)
library(shinyalert)
library(DT)
library(ggplot2)
library(RColorBrewer)
library(methods)
library(GSelection)
library(rpart)
library(glmnet)
library(caret)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  options(shiny.maxRequestSize=20*1024^2) # maximum upload size

  # help functions
  addConstraint <- function(params, block_constraint = FALSE){
    if(block_constraint){
      newA <- params$A
      newb <- params$b
      newrho <- params$rho


      model(UBayFS::setConstraints(model(),
                                   build.UBayconstraint(A = newA, b = newb, rho = as.numeric(newrho), block_matrix = block_matrix()),
                                   append = TRUE))

      proxy = dataTableProxy('blocks')
      selectRows(proxy, c())
    }
    else{
      newA <- params$A
      newb <- params$b
      newrho <- params$rho
      colnames(newA) <- names_feats()

      model(UBayFS::setConstraints(model(),
                                   build.UBayconstraint(A = newA, b = newb, rho = as.numeric(newrho)),
                                   append = TRUE))
      proxy = dataTableProxy('features')
      selectRows(proxy, c())
    }


  }

  addWeights <- function(weights){
    block_list = lapply(unique(blocks()), function(x){return(which(blocks() == x))})
    model(setWeights(model(), weights, block_list = block_list))
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

  build_error <- function(x){
    show_alert(
      title = "Error",
      text = "An error occured during application of UBayFS methods - please check data and parameters and try again.",
      type = 'error',
      btn_labels = "Ok"
    )
  }

  # feature selection functions

  decision_tree <- function(X, y, n, name = "tree"){
    rf_data = as.data.frame(cbind(y, X))
    colnames(rf_data) <- make.names(colnames(rf_data))
    tree = rpart::rpart(y~., data = rf_data)
    return(list(ranks= which(colnames(X) %in% names(tree$variable.importance)[1:n]),
                name = name))
  }

  rec_fe <- function(X,y,n, name="rfe"){
    if(is.factor(y)){
      control <- rfeControl(functions=rfFuncs, method = "cv", number = 2)
    }
    else{
      control <- rfeControl(functions=lmFuncs, method = "cv", number = 2)
    }
    results <- caret::rfe(X, y, sizes = n, rfeControl=control)
    return(list(ranks = which(colnames(X) %in% results$optVariables),
                name = name))
  }


  lasso <- function(X, y, n=NULL, name="lasso"){
    family = ifelse(is.factor(y), "binomial", "gaussian")
    cv.lasso <- cv.glmnet(as.matrix(X), y, intercept = FALSE, alpha = 1, family = family, nfolds=3)
    model <- glmnet(as.matrix(X), y, intercept = FALSE, alpha = 1, family = family,
                    lambda = cv.lasso$lambda.min)
    return(list(ranks = which(as.vector(model$beta) != 0),
                name = name))
  }


  hsic_lasso <- function(X, y, n, name="hsic"){
    ifelse(is.factor(y), {tl = as.numeric(as.integer(y)-1)}, {tl = y})
    results = feature.selection(X, tl, n)
    return(list(ranks = results$hsic_selected_feature_index,
                name = name))
  }

  # === INITIAL SETTINGS ===

  model <- reactiveVal(NULL)

  blocks <- reactiveVal(NULL)

  block_matrix <- reactive({
    t(sapply(unique(blocks()), function(x){return(1 * (x == blocks()))}))
  })

  output_width <- reactive({
    ifelse(input$showInput, 8, 12)
  })

  # === FILE INPUT HANDLING ===
  observeEvent(input$load_csv,{
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

        if(any(is.na(dat))){
          shinyalert("Caution: dataset must not contain missing values!", type= "warning")
        }
        if(nrow(dat) > 1000 | ncol(dat) > 100){
          showNotification("Caution: dataset might be too large to compute in the dashboard.", type= "warning")
        }

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

  observeEvent(input$load_csv,{
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
        if(any(is.na(lab))){
          shinyalert("Caution: target must not contain missing values!", type= "warning")
        }
        if(!is.null(model()$data)){
          names(lab) <- rownames(model()$data)
        }
        if(is.numeric(lab)){
          if(length(unique(lab)) == 2){
            lab = as.factor(lab)
          }
        }
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
    data(bcw)
    dat <- bcw

    model(append(model(), list(data = dat$data)))
    model(append(model(), list(target = dat$labels)))

    block_vec <- rep(0, ncol(dat$data))
    for(i in 1:length(dat$blocks)){
      block_vec[dat$blocks[[i]]] <- names(dat$blocks)[i]
    }
    blocks(block_vec)
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
      paste0("UBaymodel_",Sys.Date(),".Rdata")
    },

    content <- function(file){
      isolate(UBaymodel <<- model())
      save(UBaymodel, file = file)
    }
  )

  observeEvent(input$load_model,{
    path = tk_choose.files(multi = FALSE, filter = t(c("RData",".Rdata")))
    if(!is.null(path) & length(path) > 0){
      env = new.env()
      load(path, env)
      model(env[["UBaymodel"]])
      blocks(1:ncol(model()$data))
    }
  })

  # === LIKELIHOOD INPUT HANDLING ===
  observeEvent(input$confirmParam, {
    withProgress(min = 0, max = 1, value = 0, message = "building elementary models", {
      tryCatch({

        methods = c()
        for (i in input$method) {
          if(i %in% c("mRMR", "fisher", "laplace")){
            methods = c(methods, i)
          }
          else if(i == "rec_fe"){
            methods = c(methods, rec_fe)
          }
          else if(i == "hsic_lasso"){
            methods = c(methods, hsic_lasso)
          }
          else if(i == "lasso"){
            methods = c(methods, lasso)
          }
          else if(i == "decision_tree"){
            methods = c(methods, decision_tree)
          }
        }


        model(UBayFS::build.UBaymodel(model()$data,
                                      model()$target,
                                      M = input$M,
                                      tt_split = input$tt_split,
                                      constraints = NULL,
                                      method = methods,
                                      nr_features = input$n_feats,
                                      shiny = TRUE))
      },
      error = function(cond){build_error(cond)},
      warning = function(cond){build_error(cond)}
      )
    })
  })

  # === PRIOR INPUT HANDLING ===
  observeEvent(input$add_maxsize, {
    addConstraint(UBayFS::buildConstraints("max_size", list(input$maxsize), num_elements = n_feats(), rho = input$rho))
  })

  observeEvent(input$add_must, {
    sel <- input$features_rows_selected

    if(length(sel) > 1){
      addConstraint(UBayFS::buildConstraints("must_link", list(sel), num_elements = n_feats(), rho = input$rho))
    }
  })

  observeEvent(input$add_cannot, {
    sel <- input$features_rows_selected

    if(length(sel) > 1){
      addConstraint(UBayFS::buildConstraints("cannot_link", list(sel), num_elements = n_feats(), rho = input$rho))
    }
  })

  observeEvent(input$add_block_maxsize, {
    addConstraint(UBayFS::buildConstraints("max_size", list(input$block_maxsize), num_elements = nrow(block_matrix()), rho = input$rho_block), block_constraint = TRUE)
  })

  observeEvent(input$add_block_must, {
    sel <- input$blocks_rows_selected

    if(length(sel) > 1){
      addConstraint(UBayFS::buildConstraints("must_link", list(sel), num_elements = nrow(block_matrix()), rho = input$rho_block), block_constraint = TRUE)
    }
  })

  observeEvent(input$add_block_cannot, {
    sel <- input$blocks_rows_selected

    if(length(sel) > 1){
      addConstraint(UBayFS::buildConstraints("cannot_link", list(sel), num_elements = nrow(block_matrix()), rho = input$rho_block), block_constraint = TRUE)
    }
  })

  observeEvent(input$setweights, {
    if(!is.null(model())){
      if(is(model(), "UBaymodel")){
        if(any(sapply(names(input), grepl, pattern = "blockweight"))){
          blockweights <- sapply(paste0("blockweight_", unique(blocks())), function(x){return(input[[x]])})
          if(!any(is.na(as.integer(blockweights))) && all(blockweights>0)){addWeights(as.numeric(blockweights))}
          else if(any(blockweights<=0)){shinyalert("All weights must be >0", type="error")}

        }
      }
    }
  })

  # === FEATURE SELECTION ====
  optim_expressions <- reactive(
    list(input$optim_method,
         input$popsize,
         input$maxiter)
  )

  observeEvent(optim_expressions(), {
    if(!is.null(model())){
      optim_method = "GA"
    }
    if(is(model(), "UBaymodel")){
      model(setOptim(model(),
                     method = optim_method,
                     popsize = input$popsize,
                     maxiter = input$maxiter))
    }
  })

  observeEvent(input$run_UBay, {
    if(!is.null(model()$constraint.params)){
      feat_const <- which(sapply(model()$constraint.params, function(y){return(identical(y$block_matrix, diag(nrow = ncol(model()$data))))}))
      ms = model()$constraint.params[[feat_const]]$b[which(apply(model()$constraint.params[[feat_const]]$A == 1, 1, all))]

      if((!is.numeric(ms)) || (length(ms) == 0) || is.null(ms)){
        shinyalert("No max-size constraint among constraints! Add one.", type="error")
      }
      else if (ms > ncol(model()$data)){
        shinyalert("No max-size constraint among constraints! Add one.", type="error")
      }
      else{
        withProgress(min = 0, max = 1, value = 0, message = "optimizing posterior function", {
          tryCatch({
            model(UBayFS::train(model()))
          },
          error = function(cond){build_error(cond)},
          warning = function(cond){build_error(cond)}
          )
        })
      }
    }
    else{
      shinyalert("No max-size constraint among constraints! Add one.", type="error")
    }
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
    ifelse(!data_complete() | is.null(model()$constraint.params), FALSE, TRUE)
  })

  parameters_complete <- reactive({
    ifelse(prior_complete() & likelihood_complete(), TRUE, FALSE)
  })

  featureselection_complete <- reactive({
    ifelse(!parameters_complete() | is.null(model()$output), FALSE, TRUE)
  })

  observeEvent(data_complete(), {
    if(data_complete()){
      updatePrettyToggle(session, "status_data", value = TRUE)
      updateSliderInput(session, "maxsize", min = 0, max = n_feats(), value = min(10, n_feats()))
      updateSliderInput(session, "block_maxsize", min = 0, max = nrow(block_matrix()), value = min(10, nrow(block_matrix())))
    }
  })

  observe({
    if(data_complete() & length(input$method) > 0){
      enable("confirmParam")
      #show("blocktable_container")
    }
    else{
      disable("confirmParam")
    }

    if(data_complete()){
      disable("input_rownames")
      disable("input_rownames")
      disable("input_colnames")
      disable("input_blocks")

      disable("upload_data")
      disable("upload_labels")
      disable("demo_data")
    }
    else{
      enable("input_rownames")
      enable("input_rownames")
      enable("input_colnames")
      enable("input_blocks")

      enable("upload_data")
      enable("upload_labels")
      enable("demo_data")
    }
  })

  observeEvent(likelihood_complete(), {
    if(likelihood_complete()){
      updatePrettyToggle(session, "status_likelihood", value = TRUE)
      enable("add_maxsize")
      enable("add_must")
      enable("add_cannot")
      if(nrow(block_matrix()) < ncol(block_matrix())){
        enable("add_block_maxsize")
        enable("add_block_must")
        enable("add_block_cannot")
      }
    }
    else{
      disable("add_maxsize")
      disable("add_must")
      disable("add_cannot")
      disable("add_block_maxsize")
      disable("add_block_must")
      disable("add_block_cannot")
    }
  })

  observeEvent(weighting_complete(), {
    if(weighting_complete()){
      updatePrettyToggle(session, "status_weighting", value = TRUE)
    }
  })

  observeEvent(prior_complete(), {
    if(prior_complete()){
      updatePrettyToggle(session, "status_prior_features", value = TRUE)
      updatePrettyToggle(session, "status_prior_blocks", value = TRUE)
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
      updatePrettyToggle(session, "status_featureselection", value = TRUE)
    }
  })


  # === OUTPUT HANDLING ====
  output$data <- DT::renderDataTable(
    if(data_complete()){
      datatable(
        data.frame(label = model()$target, model()$data),
        filter = 'top',
        extensions = "FixedColumns",
        options = list(paging = FALSE, sDom = '<"top">rt<"bottom">ip', scrollX = TRUE, scrollY = "300px", fixedColumns = list(leftColumns = 2)),
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

  output$blocks <- DT::renderDataTable(
    datatable(
      data.frame(
        block = unique(blocks())
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
        geom_bar(stat = "identity", position = position_dodge(), fill = RColorBrewer::brewer.pal(3,"Blues")[2])+
        theme_classic()+
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
        geom_bar(stat = "identity", position = position_dodge(), , fill = RColorBrewer::brewer.pal(3,"Blues")[3])+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90))
    }
  )

  output$constraints <- renderUI({
    if(!is.null(model()$constraint.params)){
      ord_constraints <- which(sapply(model()$constraint.params, function(x){return(identical(x$block_matrix, diag(nrow = ncol(model()$data))))}))
      if(length(ord_constraints) == 1){
        const <- model()$constraint.params[[ord_constraints]]
        matA <- paste0(apply(const$A, 1, function(x){paste0(x, collapse = " & ")}), collapse = "\\\\")
        matb <- paste0(const$b, collapse = "\\\\")
        matrho <- paste0(ifelse(const$rho == Inf, "\\infty", const$rho), collapse = "\\\\")

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
    }
  })

  output$block_constraints <- renderUI({
    if(!is.null(model()$constraint.params)){
      block_constraints <- which(sapply(model()$constraint.params, function(x){return(!identical(x$block_matrix, diag(nrow = ncol(model()$data))))}))
      if(length(block_constraints) == 1){
        const <- model()$constraint.params[[block_constraints]]
        matA <- paste0(apply(const$A, 1, function(x){paste0(x, collapse = " & ")}), collapse = "\\\\")
        matb <- paste0(const$b, collapse = "\\\\")
        matrho <- paste0(ifelse(const$rho == Inf, "\\infty", const$rho), collapse = "\\\\")

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
    }
  })

  output$blocktable <- renderUI({
    blockweights <- model()$user.params$weights
    lapply(unique(blocks()), function(block_name){
      block_no <- which(blocks() == block_name)[1]
      fluidRow(
        column(4,
               textInput(inputId = paste0("blockweight_", block_name),
                         label = NULL,
                         value = ifelse(is.null(blockweights[block_no]), 1, blockweights[block_no]),
                         width = '100px')
        ),
        column(8,
               paste0(names_feats()[blocks() == block_name], collapse = ", "),
               align = "left"
        )
      )
    })
  })

  output$rho_plot <- renderPlot({
    x <- seq(-10,10,by = 0.01)
    ggplot2::ggplot(data = data.frame(x = -x, y = 1 - sapply(x, function(b){
      return(admissibility(
        state = 1,
        constraint_list = list(list(
          A = matrix(1, nrow = 1, ncol = 1),
          b = b,
          rho = input$rho,
          block_matrix = matrix(1, nrow = 1, ncol = 1))),
        log = FALSE))})),
      aes(x = x, y = y, group = 1)) +
      geom_line() +
      xlab(label = "ax-b") +
      ylab(label = "penality") +
      ylim(0, 1)
  })

  output$rho_block_plot <- renderPlot({
    x <- seq(-10,10,by = 0.01)
    ggplot2::ggplot(data = data.frame(x = -x, y = 1 - sapply(x, function(b){
      return(admissibility(
        state = 1,
        constraint_list = list(list(
          A = matrix(1, nrow = 1, ncol = 1),
          b = b,
          rho = input$rho_block,
          block_matrix = matrix(1, nrow = 1, ncol = 1))),
        log = FALSE))})),
      aes(x = x, y = y, group = 1)) +
      geom_line() +
      xlab(label = "ax-b") +
      ylab(label = "penalty") +
      ylim(0, 1)
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
           plotOutput("count_hist", height = '400px'),
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
                                   "laplace",
                                   "fisher",
                                   "hsic_lasso",
                                   "rec_fe",
                                   "lasso",
                                   "decision_tree"),
                       selected = sel_method,
                       multiple = TRUE),
           bsTooltip("method", "Select the feature selection method type(s) to be used for elementary feature selector"),
           sliderTextInput("M", withMathJax('$$M$$'), choices = c(10, 20, 30, 40, 50,
                                                                  60, 70, 80, 90, 100,
                                                                  150, 200, 250, 300, 350, 400, 450, 500,
                                                                  600, 700, 800, 900, 1000),
                           selected = sel_M),
           bsTooltip("M", "Select the number of elementary feature selector to be trained"),
           sliderInput("n_feats", "number of features", min = 1, max = max_nr_features, step = 1, value = sel_nr_features),
           bsTooltip("n_feats", "Select the number of features to be selected by each elementary feature selector"),
           sliderInput("tt_split", "train-test-split", min = 0.5, max = 0.95, step = 0.05, value = sel_tt_split),
           bsTooltip("tt_split", "Select size of the subset sampled to train each elementary feature selector")
    )
  })

  output$output_weights <- renderUI({
    column(output_width(),
           plotOutput("weight_hist", height = '400px'),
           align = 'center'
    )
  })


  output$output_constraints <- renderUI({
    column(output_width(),
           uiOutput("constraints"),
           align = 'center',
           style = "overflow-x: auto;"
    )
  })

  output$output_block_constraints <- renderUI({
    column(output_width(),
           uiOutput("block_constraints"),
           align = 'center',
           style = "overflow-x: auto;"
    )
  })


  output$fs_sliders <- renderUI({

    optim_method_choices <- c("Genetic Algorithm (GA)")
    optim_method_choices_short <- c("GA")
    popsize_choices <- c(10, 20, 50, 100, 500, 1000, 5000)
    maxiter_choices <- c(10, 20, 30, 40, 50,
                         60, 70, 80, 90, 100,
                         150, 200, 250, 300, 350, 400, 450, 500,
                         600, 700, 800, 900, 1000)


    sel_popsize <- ifelse(is.null(model()$optim.params$popsize), 1000, model()$optim.params$popsize)
    sel_maxiter <- ifelse(is.null(model()$optim.params$maxiter), 100, model()$optim.params$maxiter)
    sel_optim_method <- ifelse(is.null(model()$optim.params$method), optim_method_choices[1], model()$optim.params$method)
    if(sel_optim_method %in% optim_method_choices_short){
      sel_optim_method = optim_method_choices[which(optim_method_choices_short == sel_optim_method)]
    }

    column(12,
           pickerInput("optim_method", "select optimization/sampling method",
                       choices = optim_method_choices,
                       selected = sel_optim_method,
                       multiple = FALSE),
           bsTooltip("optim_method", "Select the method to be used for optimizing the feature set"),
           sliderTextInput("popsize", "sample size", choices = popsize_choices,
                           selected = sel_popsize),
           bsTooltip("popsize", "Select the full size of the initial population in the optimization/sampling algorithm"),
           sliderTextInput("maxiter", "no. iterations", choices = maxiter_choices,
                           selected = sel_maxiter),
           bsTooltip("maxiter", "Select the maximum number of iterations used in the optimization/sampling algorithm")
    )
  })

  output$output_fs <- renderUI({
    column(output_width(),
           column(8,
                  plotOutput("result_barplot", height = "400px"),
           ),
           column(4,
                  DT::dataTableOutput("feature_results")
           ),
           align = 'center'
    )
  })

  output$feature_results <- DT::renderDataTable(
    if(!is.null(model()$output$feature_set)){
      df <- data.frame(
        no = 1:sum(model()$output$feature_set),
        features = names_feats()[model()$output$feature_set[1,] == 1])
      colnames(df) <- c("no", "selected features")
      datatable(
        df,
        options = list(paging = FALSE, sDom = '<"top">rt<"bottom">ip', scrollX = TRUE, scrollY = "300px"),
        selection = "none",
        rownames = FALSE
      )
    }
  )

  output$result_barplot <- renderPlot({
    if(!is.null(model())){
      if(is(model(), "UBaymodel")){
        plot(model())
      }
    }
  })

  # vertical tabs
  output$lab_data_tab <- renderUI({
    div(align = "left",
        disabled(
          prettyToggle(
            inputId = "status_data",
            label_on = strong("data"),#"data loaded",
            icon_on = icon("thumbs-up"),
            status_on = "default",
            status_off = "default",
            label_off = strong("data"),#"no data loaded",
            icon_off = icon("hand-point-right"),
            animation = "smooth",
            bigger = TRUE,
            fill = FALSE,
            plain = TRUE
          )
        )
    )
  })

  output$lab_ensemble_tab <- renderUI({
    div(align = "left",
        disabled(
          prettyToggle(
            inputId = "status_likelihood",
            label_on = strong("likelihood"),#"ensemble trained",
            icon_on = icon("thumbs-up"),
            status_on = "default",
            status_off = "default",
            label_off = strong("likelihood"),#"no ensemble trained",
            icon_off = icon("hand-point-right"),
            animation = "smooth",
            bigger = TRUE,
            fill = FALSE,
            plain = TRUE
          )
        )
    )
  })

  output$lab_weights_tab <- renderUI({
    div(align = "left",
        disabled(
          prettyToggle(
            inputId = "status_weighting",
            label_on = strong("weights"),#"weights set",
            icon_on = icon("thumbs-up"),
            status_on = "default",
            status_off = "default",
            label_off = strong("weights"),#"no weights set",
            icon_off = icon("hand-point-right"),
            animation = "smooth",
            bigger = TRUE,
            fill = FALSE,
            plain = TRUE
          )
        )
    )
  })

  output$lab_constraints_tab <- renderUI({
    div(align = "left",
        disabled(
          prettyToggle(
            inputId = "status_prior_features",
            label_on = strong("constraints"),#"constraints set",
            icon_on = icon("thumbs-up"),
            status_on = "default",
            status_off = "default",
            label_off = strong("constraints"),#"no constraints set",
            icon_off = icon("hand-point-right"),
            animation = "smooth",
            bigger = TRUE,
            fill = FALSE,
            plain = TRUE
          )
        )
    )
  })

  output$lab_block_constraints_tab <- renderUI({
    div(align = "left",
        disabled(
          prettyToggle(
            inputId = "status_prior_blocks",
            label_on = strong("block constraints"),#"constraints set",
            icon_on = icon("thumbs-up"),
            status_on = "default",
            status_off = "default",
            label_off = strong("block constraints"),#"no constraints set",
            icon_off = icon("hand-point-right"),
            animation = "smooth",
            bigger = TRUE,
            fill = FALSE,
            plain = TRUE
          )
        )
    )
  })

  output$lab_results_tab <- renderUI({
    div(align = "left",
        disabled(
          prettyToggle(
            inputId = "status_featureselection",
            label_on = strong("feature selection"),#"features selected",
            icon_on = icon("thumbs-up"),
            status_on = "default",
            status_off = "default",
            label_off = strong("feature selection"),#"no features selected",
            icon_off = icon("hand-point-right"),
            animation = "smooth",
            bigger = TRUE,
            fill = FALSE,
            plain = TRUE
          )
        )
    )
  })

  observe({
    if (input$stop_app)
      stopApp()
  })
  observe({
    if (input$refresh_app)
      session$reload()
  })
})
