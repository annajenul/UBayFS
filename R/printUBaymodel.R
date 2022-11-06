#' Print a UBayFS model
#' @description Print details of a UBayFS model.
#' @param x a `UBaymodel` object created using \link{build.UBaymodel}
#' @param ... additional print parameters
#' @importFrom utils str
#' @importFrom methods is
#' @export

print.UBaymodel <- function(x,...){

  if(!is(x, "UBaymodel")){
    stop("Wrong class of x")
  }

  print("data")
  print(str(x$data))
  print(str(x$target))
  print("user.params")
  print(x$user.params)
  print("constraint.params")
  print(x$constraint.params)
  print("ensemble.params")
  print(x$ensemble.params)
  print("optim.params")
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
    stop("Train model first!")
  }

  feature_sets = apply(model$output$feature_set, 1, function(x){
    paste0(which(x == 1), collapse = ",")
  })

  cat("=== feature sets ===\n")
  print(feature_sets)
  cat("                     \n")

  cat("=== output metrics ===\n")
  print(t(model$output$metrics))

  cat("                               \n")
  cat("=== feature set similarities ===\n")
  print(model$output$mutual_similarity)
}


#' Summarizes a UBayFS model
#' @describeIn print.UBaymodel A summary of a UBayFS model
#' @param object a `UBaymodel` object created using \link{build.UBaymodel}
#' @importFrom methods is
#' @export


summary.UBaymodel <- function(object,...){

  if(!is(object, "UBaymodel")){
    stop("Wrong class of object")
  }

  cat(" UBayFS model summary \n",
      " data: ", paste0(dim(object$data), collapse = "x"), '\n',
      " labels: ", paste0(levels(object$target), ": ", tabulate(object$target), collapse = " "), "\n\n",
      " === prior constraints === \n",
      if(!is.null(object$constraint.params$constraints$A)){
        paste0(
          sapply(1:nrow(object$constraint.params$constraints$A), function(i){
            paste0(
              " constraint ",
              i,
              ": (",
              paste0(object$constraint.params$constraints$A[i,], collapse = ","),
              ") x <= ",
              object$constraint.params$constraints$b[i],
              "; rho = ",
              object$constraint.params$constraints$rho[i]
            )
          }),
          collapse = "\n")
      },
      "\n",
      if(!is.null(object$constraint.params$block_constraints$A)){
        paste0(
          sapply(1:nrow(object$constraint.params$block_constraints$A), function(i){
            paste0(
              " block constraint ",
              i,
              ": (",
              paste0(object$constraint.params$block_constraints$A[i,], collapse = ","),
              ") x <= ",
              object$constraint.params$block_constraints$b[i],
              "; rho = ",
              object$constraint.params$block_constraints$rho[i]
            )
          }),
          collapse = "\n")
      }else{"\n"},
      " \n",
      " === prior weights === \n",
      " weights: (", paste0(object$user.params$weights, collapse = ","),") \n\n",
      " === likelihood === \n",
      " ensemble counts: (", paste0(object$ensemble.params$output$counts, collapse = ","),") \n\n",
      " === feature selection results === \n",
      ifelse(is.null(object$output$feature_set), "no output produced yet",
             paste0(
               apply(object$output$feature_set, 1, function(x){
                 paste0(" ( ", paste0(which(x == 1), collapse = ",")," )")
               }),
               collapse = "\n")
      ),
      " \n"
  )


}





#' Plot a UBayFS model
#' @describeIn print.UBaymodel A barplot of an UBayFS model containing prior weights, ensemble counts and the selected features.
#' @import ggplot2
#' @import gridExtra
#' @importFrom methods is
#' @export

plot.UBaymodel <- function(x,...){

  if(!is(x, "UBaymodel")){
    stop("Wrong class of x")
  }

  # prepare data structure
  names_feats <- colnames(x$data)

  df <- data.frame(
    feature = factor(rep(names_feats,2), levels = names_feats),
    counts = c(x$ensemble.params$output$counts,
               x$user.params$weights),
    weights = factor(rep(c("ensemble", "prior"), each = length(names_feats)), levels = c("ensemble", "prior"))
  )

  # red borders of selected features
  if(!is.null(x$output)){
    if(nrow(x$output$feature_set) > 1){
      print("Warning: multiple optimal feature sets, plotting first feature set.")
    }
    feature_set <- x$output$feature_set[1,]
    df$selected <- factor(rep(feature_set, 2) == 1, levels = c(TRUE, FALSE))
    p <- ggplot2::ggplot(data = df,
                         aes(x = .data$feature,
                             y = .data$counts)) +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = NA))+
      geom_bar(aes(color = .data$selected), stat = "identity", size = 1.5, width = 0.8)
  }else{
    p <- ggplot2::ggplot(data = df,
                         aes(x = .data$feature,
                             y = .data$counts))
  }

  # barplot
  p <- p +
    geom_bar(aes(fill = .data$weights), stat = "identity", size = 1.5, width = 0.8) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(3,"Blues")[c(2,3)])+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "bottom")

  # constraint plot
  if(!is.null(x$constraint.params$constraints$A)){
    A_c = x$constraint.params$constraints$A
    A = A_c[which(!duplicated(as.data.frame(abs(A_c)))),,drop=FALSE]
    rho = x$constraint.params$constraints$rho[which(!duplicated(as.data.frame(abs(A_c))))]
    num_feat_const = ifelse(is.matrix(A), nrow(A), 1)

    if(!is.null(x$constraint.params$block_constraints)){
      A_b = x$constraint.params$block_constraints$A %*% x$constraint.params$block_constraints$block_matrix
      A = rbind(A, A_b[which(!duplicated(as.data.frame(abs(A_b)))),, drop=FALSE])
      rho = c(rho, x$constraint.params$block_constraints$rho[which(!duplicated(as.data.frame(abs(A_b))))])
    }
    df1 <- data.frame(feature = c(), constraint = c(), type = c(), level = c())

    for(i in 1:nrow(A)){
      df1 <- rbind(df1, data.frame(feature = factor(names_feats[which(A[i,] != 0)], levels = names_feats),
                                   constraint = i,
                                   type = ifelse(all(A[i,] == 1), "max-size",
                                                 ifelse(any(A[i,] < 0), "ML",
                                                        ifelse(all(A[i,] %in% c(0,1)), "CL", "other"))),
                                   rho = rho[i],
                                   level = ifelse(i <= num_feat_const, "feature", "block")
      ))
    }
    df1$rho = factor(df1$rho, levels = sort(unique(rho)))

    q <- ggplot(data = df1, aes(x = .data$feature, y = .data$constraint, group = .data$constraint, color = .data$rho))+
      geom_line(aes(linetype = .data$level), size = 1)+
      geom_point(aes(shape = .data$type), size = 4)+
      theme_classic()+
      scale_y_discrete()+
      scale_color_manual(values = RColorBrewer::brewer.pal(max(length(levels(df1$rho))+1,3), "Greens")[-1])+
      scale_shape_manual(values = c("CL" = 15, "ML" = 16, "max-size" = 18, "other" = 17))+
      scale_linetype_manual(values = c("feature" = 1, "block" = 2))+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.line.x = element_blank(),
            legend.position = "top")

    grid.arrange(q, p, nrow = 2,  heights = c(1,2))
  }
  else{
    p
  }
}
