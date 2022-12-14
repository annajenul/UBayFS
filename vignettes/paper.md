---
title: "UBayFS: An R Package for User Guided Feature Selection"
tags:
- R
- feature selection
date: "01 October 2022"
output: pdf_document
authors:
- name: Anna Jenul
  orcid: 0000-0002-6919-3483
  equal-contrib: yes
  affiliation: 1
- name: Stefan Schrunner
  orcid: 0000-0003-1327-4855
  equal-contrib: yes
  affiliation: 1
bibliography: paper.bib
affiliations:
- name: Norwegian University of Life Sciences, Ås, Norway
  index: 1
---

# Statement of Need

Feature selection, also known as variable selection in statistics, is the process of selecting important variables (features) from a list of variables in a dataset. When training predictive models, the intention behind removing the least informative features from a dataset beforehand is (a) to reduce the computational burden and mathematical limitations associated with the curse of dimensionality, and (b) to increase interpretability of the model by allowing the user to obtain insights into the relevant input variables. In particular, feature selection speeds up the training process of machine learning models, especially when the dataset is high-dimensional.

The $\mathtt{R}$ package UBayFS implements the user-guided framework for feature selection proposed in @jenul2022ubayfs, which incorporates information from the data and prior knowledge from domain experts. \autoref{fig:UBayFS} demonstrates the framework. Different approaches for integrating prior knowledge in feature selection exist, though there is a lack of general and sophisticated frameworks that deliver stable and reproducible feature selection along with implementations. With its generic setup and the possibilities to specify prior weights as well as side constraints, UBayFS shows the flexibility to be applied in a broad range of application scenarios, which exceed the capabilities of conventional feature selectors while preserving large model generality. Besides side constraints, such as the option to specify a maximum number of features, the user can add must-link constraints (features must be selected together) or cannot-link constraints (features must not be selected together). In addition, constraints can be defined on feature-block level, as well. Thus, UBayFS is also capable of solving more general problems such as block feature selection. A parameter $\rho$ regulates the shape of a penalty term accounting for side constraints, where feature sets that violate constraints lead to a lower target value. State-of-the-art methods do not cover such scenarios. 

The presented $\mathtt{R}$ package UBayFS provides an implementation along with an interactive Shiny dashboard, which makes feature selection available to $\mathtt{R}$-users with different levels of expertise. The implementation allows the user to define their own feature selectors via a function interface or to use one out of three state-of-the-art feature selectors for building the generic ensemble of feature selectors covering the data-driven component of UBayFS. State-of-the-art choices include:

- Laplacian score
- Fisher score
- mRMR

$\mathtt{R}$ offers multiple packages implementing feature selection methodology. To name a few, $\mathtt{caret}$ [@caret] is an essential machine learning repository, containing models with built-in feature selection such as tree based methods (for instance $\mathtt{rpart2}$), regularized approaches like $\mathtt{lasso}$, and non-integrated feature selectors such as recursive feature elimination $\mathtt{rfe}$. Other examples are the $\mathtt{Boruta}$ [@boruta] package implementing the $\mathtt{Boruta}$ feature selector or the $\mathtt{GSelection}$ [@gselection] package containing $\mathtt{hsic\;lasso}$ feature selection. All feature selectors available in $\mathtt{R}$ can be used as underlying ensemble feature selectors in UBayFS. 
Prior weights can be specified for single features or whole blocks as weight vectors. Linear side constraints are implemented via a matrix $A$ and a right side $b$ or with a customized function for specific constraint types. Hence, the sophisticated statistical model is summarized in a user-friendly and easy-to-use package.


![At first, UBayFS elaborates information directly from data via ensemble feature selection. This information is merged with prior expert knowledge (a-priori feature weights) in a Bayesian model framework. Additionally, the user can include further side constraints such as a maximum number of features or cannot-link constraints between features. The final step comprises the optimization with respect to the model's utility function, including the side constraints.\label{fig:UBayFS}](UBayFS_concept.png)

# Concept of UBayFS

As described in @jenul2022ubayfs, UBayFS is a Bayesian ensemble feature selection framework. The methodology is based on quantifying a random variable  $\boldsymbol{\theta}$, representing feature importances, given evidence collected from the data, denoted as $\boldsymbol{y}$. In particular, $\boldsymbol{y}$ counts the number of elementary models in the generic ensemble of feature selectors, which select a particular feature. Statistically, we interpret the result from each elementary feature selector as a realization from a multinomial distribution with parameters $\boldsymbol{\theta}$, where $\boldsymbol{\theta}\in[0,1]^N$ defines the success probabilities of sampling each feature in an individual feature selection and thus the success probability in the ensemble. Both sources of information are combined using Bayes' Theorem:

$$p(\boldsymbol{\theta}|\boldsymbol{y})\propto p(\boldsymbol{y}|\boldsymbol{\theta})\cdot p(\boldsymbol{\theta}).$$

In the framework of UBayFS,  $p(\boldsymbol{y}|\boldsymbol{\theta})$ represents the data-driven component (implemented via a multinomial likelihood), while $p(\boldsymbol{\theta})$ describes the user knowledge part modeled with a Dirichlet distribution. Due to the conjugate prior property of the Dirichlet distribution, the posterior parameter update has a tractable form and can be computed analytically.  Side constraints are represented by a system of linear inequalities $\boldsymbol{A}\cdot \boldsymbol{\delta}-\boldsymbol{b}\leq \boldsymbol{0}$, where $\boldsymbol{A}\in\mathbb{R}^{K\times N}$ and $\boldsymbol{b}\in\mathbb{R}^K$. $K$ is defined as the total number of constraints. 

In UBayFS, a relaxed inadmissibility function $\kappa_{k,\rho}(\boldsymbol{\delta})$ is used as a penalization for the violation of a given side constraint $k=1,...,K$. The joint inadmissibility function $\kappa$ pursues the idea that $\kappa = 1$ (maximum penalization) if at least one $\kappa_{k,\rho}=1$, while $\kappa=0$ (no penalization) if all $\kappa_{k,\rho}=0$. A more detailed description is provided in the original paper [@jenul2022ubayfs].

To obtain an optimal feature set $\boldsymbol{\delta}^\star$, we use a target function $U(\boldsymbol{\delta}, \boldsymbol{\theta})$ which represents a posterior expected utility of feature sets $\boldsymbol{\delta}$ given the posterior feature importance parameter $\boldsymbol{\theta}$, regularized by the inadmissibility function $\kappa(.)$.

$$\mathbb{E}_{\boldsymbol{\theta}|\boldsymbol{y}}[U(\boldsymbol{\delta}, \boldsymbol{\theta}(\boldsymbol{y}))] = \boldsymbol{\delta}^T \mathbb{E}_{\boldsymbol{\theta}|\boldsymbol{y}}[\boldsymbol{\theta}(\boldsymbol{y} )]-\lambda\kappa(\boldsymbol{\delta})\longrightarrow \underset{\boldsymbol{\delta}\in\{0,1\}^N}{\text{max}}
$$

The optimization is implemented via a genetic algorithm along with a greedy algorithm for initialization, suggested by @jenul2022ubayfs to find a proper start vector for the optimization.

# Package Summary
The function `build.UBaymodel()` initializes an S3 class object UBaymodel and computes the ensemble of elementary feature selectors. In the current version, among others, linear feature selectors such as Fisher score and Laplacian score [@you2022rdimtools], mRMR [@de2013mrmre], or the non-linear HSIC Lasso are supported as options [@gselection]. In addition, the number of elementary models $M$ is specified. The user can directly set prior weights inside the `build` function. Constraints are either provided as a matrix $A$ and a right side $b$, or built using the `buildConstraints()` function, which supports max-size, must-link, and cannot-link constraints on both feature and block level. UBayFS requires at least one constraint limiting the total number of features to be selected ("max-size"). The level of constraint-relaxation is steered with an input parameter $\rho$. In addition, the weights for single features or feature blocks are set with `setWeights()`.

The function `admissibility()` allows the user to evaluate the penalty term for a given feature set under a set of constraints. After initializing the model and computing the ensembles, the `train.UBaymodel()` function optimizes the feature set via a genetic algorithm [@scrucca2013ga] with greedy initialization. According to empirical evaluations, the greedy initialization decreases the runtime and leads to faster convergence towards an optimal feature set. Finally, the package implements the generic functions `print.UBaymodel()`, `plot.UBaymodel()`, and `summary.UBaymodel()` as well as an evaluation function `evaluteFS()` to report and visualize results. Two vignettes guide the user through the package and demonstrate how the method can be deployed in common application scenarios, including how user knowledge is specified and how feature- and block-wise constraints are set.

# Interactive Shiny Dashboard
 The function `runInteractive()` opens an interactive Shiny dashboard allowing the user to load and analyze data interactively. However, due to computational limitations, it is not recommended to use the HTML interface for larger datasets ($> 100$ features or $>1000$ samples). Instead, functions should be called from the $\mathtt{R}$ console in such cases. \autoref{fig:Shiny} shows the dashboard with the different tabs:

 - **data**: Load the dataset and specify whether row names, column names, or a block structure is present. A demo dataset is ready to be loaded and used for a first touch on the package.
 - **likelihood**: Select elementary feature selectors for ensemble feature selection, the number of models $M$, the number of features in each model, and the ratio of the train-test split. Further, the dashboard allows the user to mix different elementary feature selectors, although this option is not recommended due to limited stability [@seijo:ensembleSurvey].
 - **weights**: The prior feature weights are set by the user. For block feature selection, it is possible to set weights for blocks; otherwise, for a single feature.
 - **constraints**: In this task, the user sets different constraints (at least a max-size constraint). The penalty $\rho$ can be varied here as well.
 - **feature selection**: In the dashboard's last step, an optimization procedure determines the final feature set. A plot of the final result is produced - also, the model can be saved as an Rdata file and loaded to the dashboard again.

 ![Illustration of the Shiny HTML dashboard.\label{fig:Shiny}](UBay_Shiny_Screenshot.png)

# Ongoing research
Based on the present UBayFS package, ongoing work focuses on the implementation of even more types of expert constraints and elementary feature selection models. Moreover, a Python package with similar functionality is planned for the future.

# Acknowledgements
We would like to thank Prof. Jürgen Pilz (University of Klagenfurt) and Prof. Oliver Tomic (Norwegian University of Life Sciences), who contributed to the development of the methodology and supported us with ideas and fruitful discussions, as well as Kristian Hovde Liland (Norwegian University of Life Sciences) for testing the $\mathtt{R}$ package.

# References
