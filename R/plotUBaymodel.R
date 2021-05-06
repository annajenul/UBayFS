#' plot a UBayFS model
#' @description a barplot of an UBayFS model containing prior weights, ensemble counts and the selected features and map estimate
#' @param x a UBaymodel object created using build.UBaymodel
#' @param ... unused
#' @import ggplot2
#' @import ggpubr
#' @export

plot.UBaymodel <- function(x,...){
  plot_UBaymodel(x,...)
}

plot_UBaymodel <- function(x,...){

  if(class(x) != "UBaymodel"){
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
    if(nrow(x$output$map) > 1){
      print("Warning: multiple optimal feature sets, plotting first feature set.")
    }
    map <- x$output$map[1,]
    df$selected <- factor(rep(map, 2) == 1, levels = c(TRUE, FALSE))
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
    theme(axis.text.x = element_text(angle = 90))

  # constraint plot
  if(!is.null(x$user.params$constraints$A)){
    A = x$user.params$constraints$A
    rho = x$user.params$constraints$rho
    num_feat_const = nrow(A)

    if(!is.null(x$user.params$block_constraints)){
      A = rbind(A, x$user.params$block_constraints$A %*% x$user.params$block_constraints$block_matrix)
      rho = c(rho, x$user.params$block_constraints$rho)
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
            axis.line.x = element_blank())

    ggarrange(q, p, nrow = 2, align = "v", heights = c(1,2))
  }
  else{
    p
  }

}
