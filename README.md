
<!-- badges: start -->
  [![R-CMD-check](https://github.com/annajenul/UBayFS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/annajenul/UBayFS/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


# UBayFS <img src="vignettes/logo.png" align="right" width="200"/>



The UBayFS package implements the framework proposed in the article [Jenul et al. (2022)](https://link.springer.com/article/10.1007/s10994-022-06221-9), together with an interactive Shiny dashboard, which makes UBayFS applicable to R-users with different levels of expertise. UBayFS is an ensemble feature selection technique embedded in a Bayesian statistical framework. The method combines data and user knowledge, where the first is extracted via data-driven ensemble feature selection. The user can control the feature selection by assigning prior weights to features and penalizing specific feature combinations. In particular, the user can define a maximal number of selected features and must-link constraints (features must be selected together) or cannot-link constraints (features must not be selected together). Using relaxed constraints, a parameter $\rho$ regulates the penalty shape. Hence, violation of constraints can be valid but leads to a lower target value of the feature set that is derived from the violated constraints. UBayFS can be used for common feature selection and also for block feature selection.

If you prefer Python, please check out our corresponding [Python implementation](https://www.github.com/annajenul/UBayFSpy).

Documentation and Structure
---------------------------

A [documentation](https://annajenul.github.io/UBayFS/) illustrates how UBayFS can be used for standard feature selection 

UBayFS is implemented via a core S3-class 'UBaymodel', along with help functions. An overview of the 'UBaymodel' class and its main generic functions, is shown in the following diagram:

<p align="center">
	<img src="vignettes/UBay_UML.png" width="500"/>
</p>

Requirements and Dependencies
-----------------------------

- R (>= 3.5.0)
- GA
- matrixStats
- shiny
- mRMRe
- Rdimtools
- DirichletReg
- ggplot2
- gridExtra
- utils
- hyper2
- methods
- prettydoc


In addition, some functionality of the package (in particular, the interactive Shiny interface) requires the following depedencies:

- shinyWidgets
- shinyalert
- DT
- RColorBrewer
- shinyjs
- shinyBS
- testthat (>= 3.0.0)
- rmarkdown
- dplyr
- plyr
- knitr
- rpart
- GSelection
- caret
- glmnet

Implementation Details
----------------------
The original paper defines the following utility function $U(\boldsymbol{\delta},\boldsymbol{\theta})$ for optimization with respect to $\boldsymbol{\delta}\in \lbrace 0,1\rbrace ^N$:
$$U(\boldsymbol{\delta},\boldsymbol{\theta}) = \boldsymbol{\delta}^T \boldsymbol{\theta}-\lambda \kappa(\boldsymbol{\delta}), $$
for fixed $\lambda>0$.


For practical reasons, the implementation in the UBayFS package uses a modified utility function $\tilde{U}(\boldsymbol{\delta},\boldsymbol{\theta})$ which adds an admissibility term $1-\kappa(\boldsymbol{\delta})$ rather than subtracting an inadmissibility term $\kappa(\boldsymbol{\delta})$
$$\tilde{U}(\boldsymbol{\delta},\boldsymbol{\theta}) = \boldsymbol{\delta}^T \boldsymbol{\theta}+\lambda (1-\kappa(\boldsymbol{\delta})) = \boldsymbol{\delta}^T \boldsymbol{\theta}-\lambda \kappa(\boldsymbol{\delta}) +\lambda.$$

Thus, the function values of $U(\boldsymbol{\delta},\boldsymbol{\theta})$ and $\tilde{U}(\boldsymbol{\delta},\boldsymbol{\theta})$ deviate by a constant $\lambda$; however, the optimal feature set $$\boldsymbol{\delta}^{\star} = \underset{\boldsymbol{\delta}\in\lbrace 0,1\rbrace ^N}{\text{arg max}}~ U(\boldsymbol{\delta},\boldsymbol{\theta}) = \underset{\boldsymbol{\delta}\in\lbrace 0,1\rbrace ^N}{\text{arg max}}~ \tilde{U}(\boldsymbol{\delta},\boldsymbol{\theta})$$ remains unaffected.


Installation
------------
The development version of the package can be installed with:

    remotes::install_github("annajenul/UBayFS", build_manual = TRUE, build_vignettes = TRUE)

If you use a macOS operator system, make sure you have [XQuartz](https://www.xquartz.org/) installed.

To build the vignettes, Pandoc is required. It may happen that Pandoc is missing on your computer, or that the version is too old. Then the installation will return the error
    
    
    Pandoc is required to build R Markdown vignettes but not available. Please make sure it is installed.
    
An installation guide for Pandoc on different operation systems is provided [here](https://pandoc.org/installing.html).

Contributing
------------
Your contribution to UBayFS is very welcome! 

Contribution to the package requires the agreement of the [Contributor Code of Conduct](https://github.com/annajenul/UBayFS/blob/master/CODE_OF_CONDUCT.md) terms.

For the implementation of a new feature or bug-fixing, we encourage you to send a Pull Request to [the repository](https://github.com/annajenul/UBayFS). Please add a detailed and concise description of the invented feature or the bug. In case of fixing a bug, include comments about your solution. To improve UBayFS even more, feel free to send us issues with bugs, you are not sure about. We are thankful for any kind of constructive criticism and suggestions.

Citation
------------
If you use UBayFS in a report or scientific publication, we would appreciate citations to the following papers:

[![DOI](https://joss.theoj.org/papers/10.21105/joss.04848/status.svg)](https://doi.org/10.21105/joss.04848)


Jenul, A. and Schrunner, S., (2023). UBayFS: An R Package for User Guided Feature Selection. Journal of Open Source Software, 8(81), 4848, https://doi.org/10.21105/joss.04848

Bibtex entry:

	@article{Jenul2023,
	  doi = {10.21105/joss.04848},
	  url = {https://doi.org/10.21105/joss.04848},
	  year = {2023},
	  month = jan,
	  publisher = {The Open Journal},
	  volume = {8},
	  number = {81},
	  pages = {4848},
	  author = {Anna Jenul and Stefan Schrunner},
	  title = {{UBayFS}: An R Package for User Guided Feature
			Selection},
	  journal = {Journal of Open Source Software}
	}

Jenul, A., Schrunner, S. et al. A user-guided Bayesian framework for ensemble feature selection in life science applications (UBayFS). Mach Learn (2022). https://doi.org/10.1007/s10994-022-06221-9

Bibtex entry:

	@article{Jenul2022,
	  doi = {10.1007/s10994-022-06221-9},
	  url = {https://doi.org/10.1007/s10994-022-06221-9},
	  year = {2022},
	  month = aug,
	  publisher = {Springer Science and Business Media {LLC}},
	  volume = {111},
	  number = {10},
	  pages = {3897--3923},
	  author = {Anna Jenul and Stefan Schrunner and J\"{u}rgen Pilz and Oliver Tomic},
	  title = {A user-guided Bayesian framework for ensemble feature selection in life science applications ({UBayFS})},
	  journal = {Machine Learning}
}
