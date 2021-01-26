#' @export
#'
load_wisconsin <- function(){
  # load Wisconsin breast cancer dataset directly from UCI
  dat <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data",
                  header = FALSE,
                  row.names = 1)

  labels <- as.factor(dat[,1])
  data <- apply(dat[,-1], 2, as.numeric)

  return(list(
    data = data,
    labels = labels
  ))
}
