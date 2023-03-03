#' Print a UBayFS model
#' @description Print details of a `UBaymodel`
#' @param x a `UBaymodel` object created using \link{build.UBaymodel}
#' @param ... additional print parameters
#' @importFrom utils str
#' @importFrom methods is
#' @return prints model summary to the console, no return value
#' @export

print.UBaymodel <- function(x,...){

  if(!is(x, 'UBaymodel')){
    stop('Wrong class of x')
  }

  print('data')
  print(str(x$data))
  print(str(x$target))
  print('user.params')
  print(x$user.params)
  print('constraint.params')
  for(const in x$constraint.params){
    print(const)
  }
  print('ensemble.params')
  print(x$ensemble.params)
  print('optim.params')
  print(x$optim.params)
  if(!is.null(x$output)){
    printResults(x)
  }

}

#' Feature selection results
#' @describeIn print.UBaymodel Display and summarize the results of UBayFS after feature selection.
#' @param model a `UBaymodel` object created using \link{build.UBaymodel} after training
#' @export

printResults <- function(model){

  if(is.null(model$output$feature_set)){
    stop('Train model first!')
  }

  feature_sets = apply(model$output$feature_set, 1, function(x){
    paste0(which(x == 1), collapse = ',')
  })

  cat('=== feature sets ===\n')
  print(feature_sets)
  cat('                     \n')

  cat('=== output metrics ===\n')
  print(t(model$output$metrics))

  cat('                               \n')
  cat('=== feature set similarities ===\n')
  print(model$output$mutual_similarity)
}


#' Summarizes a `UBaymodel`
#' @describeIn print.UBaymodel A summary of a `UBaymodel`
#' @param object a `UBaymodel` object created using \link{build.UBaymodel}
#' @importFrom methods is
#' @export


summary.UBaymodel <- function(object,...){

  if(!is(object, 'UBaymodel')){
    stop('Wrong class of object')
  }

  cat(' UBayFS model summary \n',
      ' data: ', paste0(dim(object$data), collapse = 'x'), '\n',
      ' labels: ', paste0(levels(object$target), ': ', tabulate(object$target), collapse = ' '), '\n\n',
      ' === constraints === \n')

  if(length(object$constraint.params) > 0){
    group_num <- 0
    for(const in object$constraint.params){
      group_num <- group_num + 1
      cat(' ', rep('-',10), ' group ', group_num,' ', paste0(rep('-', 10)), '\n')
      summary(const)
    }
  }
      cat('\n',
      ' === prior weights === \n',
      ' weights: (', paste0(object$user.params$weights, collapse = ','),') \n\n',
      ' === likelihood === \n',
      ' ensemble counts: (', paste0(object$ensemble.params$output$counts, collapse = ','),') \n\n',
      ' === feature selection results === \n',
      ifelse(is.null(object$output$feature_set), 'no output produced yet',
             paste0(
               apply(object$output$feature_set, 1, function(x){
                 paste0(' ( ', paste0(which(x == 1), collapse = ','),' )')
               }),
               collapse = '\n')
      ),
      ' \n'
  )
}


#' Plot a UBayFS model
#' @describeIn print.UBaymodel A barplot of a `UBaymodel` containing prior weights, ensemble counts and the selected features.
#' @import ggplot2
#' @import gridExtra
#' @importFrom methods is
#' @export

plot.UBaymodel <- function(x,...){

  if(!is(x, 'UBaymodel')){
    stop('Wrong class of x')
  }

  # prepare data structure
  names_feats <- colnames(x$data)

  df <- data.frame(
    feature = factor(rep(names_feats,2), levels = names_feats),
    counts = c(x$ensemble.params$output$counts,
               x$user.params$weights),
    weights = factor(rep(c('ensemble', 'prior'), each = length(names_feats)), levels = c('ensemble', 'prior'))
  )

  # red borders of selected features
  if(!is.null(x$output)){
    if(nrow(x$output$feature_set) > 1){
      print('Warning: multiple optimal feature sets, plotting first feature set.')
    }
    feature_set <- x$output$feature_set[1,]
    df$selected <- factor(rep(feature_set, 2) == 1, levels = c(TRUE, FALSE))
    p <- ggplot2::ggplot(data = df,
                         aes(x = .data$feature,
                             y = .data$counts)) +
      scale_color_manual(values = c('TRUE' = 'red', 'FALSE' = NA))+
      geom_bar(aes(color = .data$selected), stat = 'identity', size = 1.5, width = 0.8)
  }else{
    p <- ggplot2::ggplot(data = df,
                         aes(x = .data$feature,
                             y = .data$counts))
  }

  # barplot
  p <- p +
    geom_bar(aes(fill = .data$weights), stat = 'identity', size = 1.5, width = 0.8) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(3,'Blues')[c(2,3)])+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90),
          legend.position = 'bottom')

  # constraint plot
  if(length(x$constraint.params) > 0){
    q <- ggplot()
    A <- matrix(nrow = 0, ncol = ncol(x$data))
    rho <- c()
    feat_const_inds <- c()
    for(l in 1:length(x$constraint.params)){
      if(identical(x$constraint.params[[l]]$block_matrix, diag(nrow = ncol(x$data)))){
        # if constraint group is not block constraint
        A_c = x$constraint.params[[l]]$A
        A = rbind(A, A_c[which(!duplicated(as.data.frame(abs(A_c)))),,drop=FALSE])
        rho = c(rho, x$constraint.params[[l]]$rho[which(!duplicated(as.data.frame(abs(A_c))))])
        feat_const_inds <- ((nrow(A) - nrow(A_c) + 1) : nrow(A))
      }
      else{
        A_b = x$constraint.params[[l]]$A %*% x$constraint.params[[l]]$block_matrix
        A = rbind(A, A_b[which(!duplicated(as.data.frame(abs(A_b)))),, drop=FALSE])
        rho = c(rho, x$constraint.params[[l]]$rho[which(!duplicated(as.data.frame(abs(A_b))))])
      }
    }

    df1 <- data.frame(feature = c(), constraint = c(), type = c(), level = c())
    for(i in 1:nrow(A)){
      df1 <- rbind(df1, data.frame(feature = factor(names_feats[which(A[i,] != 0)], levels = names_feats),
                                   constraint = i,
                                   type = ifelse(all(A[i,] == 1), 'max-size',
                                                 ifelse(any(A[i,] < 0), 'ML',
                                                        ifelse(all(A[i,] %in% c(0,1)), 'CL', 'other'))),
                                   rho = rho[i],
                                   level = ifelse(i %in% feat_const_inds, 'feature', 'block')
      ))
    }
    df1$rho = factor(df1$rho, levels = sort(unique(rho)))

    q <- ggplot(data = df1, aes(x = .data$feature, y = .data$constraint, group = .data$constraint, color = .data$rho))+
      geom_line(aes(linetype = .data$level), size = 1)+
      geom_point(aes(shape = .data$type), size = 4)+
      theme_classic()+
      scale_y_discrete()+
      scale_color_manual(values = RColorBrewer::brewer.pal(max(length(levels(df1$rho))+1,3), 'Greens')[-1])+
      scale_shape_manual(values = c('CL' = 15, 'ML' = 16, 'max-size' = 18, 'other' = 17))+
      scale_linetype_manual(values = c('feature' = 1, 'block' = 2))+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.line.x = element_blank(),
            legend.position = 'top')

    q <- ggplotGrob(q)
    p <- ggplotGrob(p)
    grid::grid.newpage()
    grid::grid.draw(rbind(q, p))
  }
  else{
    p
  }
}
