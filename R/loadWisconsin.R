#' loads the Wisconsin breast cancer dataset remotely
#' @description loads the Wisconsin breast cancer dataset as demonstration dataset from UCI database
#' @return a list with elements data and labels
#' @import utils
#' @export

loadWisconsin <- function(){
  # load Wisconsin breast cancer dataset directly from UCI
  tryCatch({dat <- utils::read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data",
                  header = FALSE,
                  row.names = 1)},
           error = function(e){stop("Could not load demo file")}
  )

  labels <- as.factor(dat[,1])
  data <- apply(dat[,-1], 2, as.numeric)

  return(list(
    data = data,
    labels = labels
  ))
}
