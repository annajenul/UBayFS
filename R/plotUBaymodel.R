#' plot a UBayFS model
#' @description a barplot of an UBayFS model containing prior weights, ensemble counts and the selected features and map estimate
#' @param x a UBaymodel object created using build.UBaymodel
#' @param ... unused
#' @import ggplot2
#' @export

plot.UBaymodel <- function(x,...){

  if(class(x) != "UBaymodel"){
    stop("Wrong class of x")
  }

  names_feats <- colnames(x$data)

  df <- data.frame(
    feature = factor(rep(names_feats,2), levels = names_feats),
    counts = c(x$ensemble.params$output$counts,
              x$user.params$weights),
    weights = factor(rep(c("ensemble", "prior"), each = length(names_feats)), levels = c("ensemble", "prior"))
  )
  if(!is.null(x$output)){
    df$selected <- factor(rep(x$output$map, 2) == 1, levels = c(TRUE, FALSE))
    p <- ggplot2::ggplot(data = df,
                         aes(x = .data$feature,
                             y = .data$counts,
                             fill = .data$weights,
                             color = .data$selected)) +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = NA))
  }
  else{
  p <- ggplot2::ggplot(data = df,
                    aes(x = .data$feature,
                        y = .data$counts,
                        fill = .data$weights))
  }

  p + geom_bar(stat = "identity", size = 1.5, width = 0.8)+
    scale_fill_manual(values = RColorBrewer::brewer.pal(3,"Blues")[c(2,3)])+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90))
}
