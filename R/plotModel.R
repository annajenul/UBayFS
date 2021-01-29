#' plot a UBayFS model
#' @description a barplot of an UBayFS model containing prior weights, ensemble counts and the selected features and map estimate
#' @param model a UBaymodel object created using build.model
#' @import ggplot2
#' @export

plot.UBaymodel <- function(model){

  if(class(model) != "UBaymodel"){
    stop("Wrong class of model")
  }

  names_feats <- colnames(model$data)

  df <- data.frame(
    feature = factor(rep(names_feats,2), levels = names_feats),
    value = c(model$ensemble.params$output$counts,
              model$user.params$weights),
    weights = factor(rep(c("ensemble", "prior"), each = length(names_feats)), levels = c("ensemble", "prior"))
  )
  if(!is.null(model$output)){
    df$selected <- factor(rep(model$output$map, 2) == 1, level = c(TRUE, FALSE))
    p <- ggplot2::ggplot(data = df,
                         aes(x = feature, y = value, fill = weights, color = selected)) +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = NA))
  }
  else{
    p <- ggplot2::ggplot(data = df,
                    aes(x = feature, y = value, fill = weights))
  }

  p + geom_bar(stat = "identity", size = 1.5, width = 0.8)+
    scale_fill_brewer(palette = "Blues")+
    theme_dark()+
    theme(axis.text.x = element_text(angle = 90))
}
