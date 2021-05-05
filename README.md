A quick tour through UBayFS
===========================

The UBayFS package implements the framework proposed in [@jenul:UBayFS], together with an interactive Shiny dashbord, which makes UBayFS applicable to R-users as well as non R-users. UBayFS is an ensemble feature selection technique, embedded in a Bayesian statistical framework. The method combines data and user knowledge, where the first is extracted from data driven ensemble feature selection techniques. The user can control the feature selection by assigning prior weights to features and penalizing specific feature combinations. Further, the user can define a maximal number of selected features and must-link constraints (features must be selected together) or cannot-link constraints (features must not be selected together). Using relaxed constraints, a parameter $\rho$ regulates the penalty shape. Hence, violation of constraints can be valid but leads to a lower probability of the feature set that is derived from the violated constraints.

In this vignette we use the Wisconsin breast cancer dataset [@wolberg:wisconsin] for demonstration. Specifically, the dataset consists of 569 samples and 30 features, and can be downloaded as demo function using the function ``loadWisconsin()``. The dataset describes a classification problem, where the aim is to distinguish between malignant and benign cancer based on image data. Features are derived from 10 image characteristics, where each characteristic is represented by 3 features (summary statistics) in the dataset. For instance, the characteristic *radius* is represented by features *radius mean*, *radius standard deviation* and *radius worst*.

### Requirements and dependencies
- R (>= 3.5.0)
- GA
- matrixStats
- shiny
- mRMRe
- Rdimtools
- caret
- DirichletReg
- dplyr
- glmnet
- utils
- ggplot2
- ggpubr

In addition, some functionality of the package (in particular, the interactive Shiny interface) requires the following depedencies:

- shinyWidgets
- DT
- RColorBrewer
- tcltk
- shinyjs
- shinythemes
- shinyBS
- testthat (>= 3.0.0)
- knitr
- rmarkdown

UBayFS is implemented via a core S3-class 'UBaymodel', along with help functions. An overview of the 'UBaymodel' class and its main generic functions, is shown in the following UML diagram:

```{r, out.width="45%", out.height="45%", fig.align="center", echo = FALSE}
include_graphics("UBay_UML.png")
``` 

The help function ``buildConstraints(...)`` provides an easy way to define side constraints for the model. Further, ``runInteractive()`` enters the Shiny dashboard, given that the required depedencies are available (see above).

Like other R packages, UBayFS is loaded using the ``library(UBayFS)`` command. Via ``loadWisconsin()`` the user can download the Wisconsin breast cancer dataset directly from the UCI web server, given that an internet connection is available.

```{r, include = TRUE, cache = TRUE}
library(UBayFS)
data(wbc)
```

## Ensemble Training
The function ``build.UBaymodel()`` initializes the UBayFS and performs ensemble feature selection. The training dataset and target are initialized with the arguments ``data`` and ``target``. Although the UBayFS concept permits unsupervised, multiclass or regression setups, the current implementation supports binary target variables only. The input variable ``M`` defines the number ensembles of each feature selector in ``method`` shall be performed. Each ensemble model is computed on ``tt_split`` percent randomly selected samples of the whole sample space. Currently, the ``method`` parameter can be set to ``mRMR`` (minimum redundancy maximum relevance), see [@ding:mrmr], and ``lscore`` (Laplacian score), see [@he:laplacian]. Furthermore, the number of features selected in each ensemble can be controled with the parameter ``nr_features``.

For the standard UBayFS initialization, all prior feature weights are set to 1 and no prior feature constraints are included, yet. The ``summary()`` function provides an overview  of the dataset, the prior weights and the likelihood --- ensemble counts indicate how often a feature was selected over the ensemble feature selections. 
```{r, include = TRUE}
model = build.UBaymodel(data = wbc$data,
                        target = wbc$labels,
                        M = 100, 
                        tt_split = 0.75,
                        nr_features = 10,
                        method = "mRMR")
summary(model)
```
## User knowledge
With the function ``setWeights()`` the user can change the feature weights from the standard initialization to desired values. In our example dataset we assign features representing common image characteristics the same weight.  
```{r, include=TRUE}
weights = c(10,15,20,16,15,10,12,17,21,14)
print(weights)
model = setWeights(model = model, weights = weights, block_list = wbc$blocks)
```

Prior feature constraints can defined with the function ``buildConstraints()``. The input ``constraint_types`` consists of a vector, where all onstraint types are defined. Then, with ``constraint_vars``, the user specifies details about the constraint: for max-size, the number of features to select is provided, while for must-link and cannot-link, the set of feature indices to be linked must be provided. Each list entry corresponds to one constraint in ``constraint_types``. In addition, ``num_features`` denotes the total number of features in the dataset and ``rho`` corresponds to the relaxation parameter of the admissibility function.  

As ``print(constraints)`` shows, the matrix ``A`` has 10 rows for 4 constraints. While *max-size* and *cannot-link* can be expressed in one equation each, *must-link* is a pairwise constraint. In specific, the *must-link* constraint between $n$ features is split into $\frac{n!}{(n-2)!}$ elementary constraints. Hence, 6 equations represent the *must-link* constraint. The function ``setConstraints()`` integrates the constraints into the UBay model. 
```{r, include=TRUE}
constraints = buildConstraints(constraint_types = c("max_size", "must_link", rep("cannot_link", 2)),
                               constraint_vars = list(10, # max_size
                                                      c(1,11,21), # must_link
                                                      c(1,10), # cannot_link
                                                      c(20,23,24) #cannot_link
                                                      ),
                               num_elements = ncol(model$data),
                                rho = c(Inf, # max_size
                                        0.1, # must_link
                                        1, # cannot_link
                                        1) # cannot_link
)
# )
print(constraints)
model = setConstraints(model = model, constraints = constraints)
```

## Optimization and evaluation
A genetic algorithm, described by [@givens:compstat] and implemented in [@R:GA], searches for the maximum a posteriori estimate, delivering an estimation of the optimal feature set from the UBayFS framework. With ``setOptim()`` we initialize the genetic algorithm. ``popsize`` inicates the number of candidate feature set created in each iteration and ``maxiter`` is the iteration size. 
  - train function with GA
  - print/summary/plot

```{r, include=TRUE}
model = setOptim(model = model, popGreedy = 80, popsize = 100, maxiter = 200, constraint_dropout_rate = 0.05)
```

At this point, we are have initialized prior weights, prior constraints and the optimization procedure --- we can now train the UBayFS model with the generic function ``train``. The summary function provides an overview on all components, UBay exists of. The ``plot()`` function shows the prior feature information as bar charts, with the selected features marked with red borders. In addition, the constraints and the regularization parameter $\rho$ are presented. 
```{r, include=TRUE, fig.width=7, fig.height=6}
model = train(x = model)
summary(model)
plot(model)
```


## Shiny dashboard
``UBayFS`` provides an interactive ``R Shiny`` dashboard, making UBayFS applicable to non-R users, as well. With its intuitive user interface, the user can load data, set likelihood parameters and even control the admissibility regularization strength of each constraint. Histograms and further plots help to get an overview on the users settings. The interactive dashboard offers ``save`` and ``load`` buttons, in order to save or load UBayFS models as RData files.
```{r,eval=FALSE}
runInteractive()
```

```{r, out.width="100%", echo = FALSE}
include_graphics("UBay_Shiny_Screenshot.png")
``` 

## Conclusion
Although the current version of UBayFS is limited to ``mRMR`` and ``Laplacian score``, it will be extended with additional feature selectors that provide a scoring of the features. In addition, an expansion of the package with UBay Regression and UBay for unsupervised problems is planned. The exactness of the likelihood can be considered as a trade off between the number of models and the runtime, which linearly increases with the number of models. Especially the Shiny dashboard delivers insight into the single UBayFS steps. Nevertheless, the dashboard is slow for high dimensional datasets, what makes it necessary to use the console version in such settings. 

## References
Anna Jenul, Stefan Schrunner, Jürgen Pilz and Oliver Tomic. "A User-Guided Bayesian Framework for Ensemble Feature Selection in Life Science Applications (UBayFS)." <em>arXiv preprint arXiv:2104.14787</em> (2021).
