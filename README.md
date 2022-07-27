UBayFS
======

The UBayFS package implements the framework proposed in the article [A User-Guided Bayesian Framework for Ensemble Feature Selection in Life Science Applications (UBayFS)](https://arxiv.org/abs/2104.14787), together with an interactive Shiny dashbord, which makes UBayFS applicable to R-users with different levels of expertise. UBayFS is an ensemble feature selection technique, embedded in a Bayesian statistical framework. The method combines data and user knowledge, where the first is extracted via data-driven ensemble feature selection. The user can control the feature selection by assigning prior weights to features and penalizing specific feature combinations. In particular, the user can define a maximal number of selected features and must-link constraints (features must be selected together) or cannot-link constraints (features must not be selected together). Using relaxed constraints, a parameter $\rho$ regulates the penalty shape. Hence, violation of constraints can be valid but leads to a lower target value of the feature set that is derived from the violated constraints. UBayFS can be used for common feature selection and also for block feature selection.

Example
-------

Two vignettes in markdown format demonstrate the use of UBayFS:

* [feature selection](https://github.com/annajenul/UBayFS/tree/master/vignettes/UBayFS.Rmd) 
* [block feature selection](https://github.com/annajenul/UBayFS/tree/master/vignettes/BFS_UBayFS.Rmd)

UBayFS is implemented via a core S3-class 'UBaymodel', along with help functions. An overview of the 'UBaymodel' class and its main generic functions, is shown in the following diagram:

<img src="vignettes/UBay_UML.jpg" width="500"/>

Requirements and dependencies
-----------------------------

- R (>= 3.5.0)
- GA
- matrixStats
- shiny
- mRMRe
- Rdimtools
- caret
- DirichletReg
- glmnet
- ggplot2
- ggpubr
- utils
- hyper2
- rpart
- GSelection
- knitr


In addition, some functionality of the package (in particular, the interactive Shiny interface) requires the following depedencies:

- shinyWidgets
- shinyalert
- DT
- RColorBrewer
- tcltk
- shinyjs
- shinythemes
- shinyBS
- testthat (>= 3.0.0)
- rmarkdown
- prettydoc
- plyr

Installation
------------
The development version of the package can be installed with \
`remotes::install_github("annajenu/UBayFS", ref = "main")`
