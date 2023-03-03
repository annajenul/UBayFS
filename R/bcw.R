#' Breast Cancer Wisconsin dataset
#'
#' A dataset containing features computed from digitized images of a fine needle aspirate (FNA) of a breast mass.
#' The target function contains two classes representing patient diagnoses (M...malignant and B...benign).
#' The dataset has been taken from the UCI Repository of Machine Learning Databases and was created by W. H. Wolberg, W. N. Street and O. L. Mangasarian in 1995.
#' For details, see UCI documentation or literature:
#' \itemize{
#'  \item{\doi{10.1117/12.148698}}
#'  \item{\url{https://www.jstor.org/stable/171686}}
#' }
#' Feature blocks were added to the original dataset according to the dataset description (10 blocks corresponding to different image characteristics).
#'
#' @format A list containing:
#'        \itemize{
#'         \item{a matrix `data` with 569 rows and 30 columns representing features,}
#'         \item{a vector `labels` of factor type with 569 entries representing the binary target variable, and}
#'         \item{a list of feature indices representing feature blocks.}
#'        }
#' @source \url{https://archive.ics.uci.edu/ml/datasets/breast+cancer+wisconsin+(diagnostic)}
'bcw'
