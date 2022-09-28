---
title: "UBayFS: An R Package for User Guided Feature Selection in a Bayesian framework"
tags:
- R
- feature selection
date: "13 August 2022"
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

# Summary and Statement of Need

The $\mathtt{R}$ package UBayFS implements the user-guided framework for feature selection, proposed in @jenul2022ubayfs, which incorporates information from the data and prior knowledge from domain experts.  Prior feature weights are incorporated as weight vectors in the model. With its generic setup, UBayFS shows large flexibility to specify side constraints, which exceeds the capabilities of conventional feature selectors.Beyond the option to specify a maximum number of features the user can specify must-link constraints (features must be selected together) or cannot-link constraints (features must not be selected together). Must-link constraint can also be used for block feature selection (a problem conventionally tackled by group lasso, see @sgl). UBayFS provides more flexibility to specify user input, and no latent parameter must be estimated. The cannot-link constraint is of special interest when multiple redundant features exist - putting them under one cannot-link constraint, only one feature can be selected. Using relaxed constraints, a parameter $\rho$ regulates the penalty shape. Hence, the violation of constraints can be valid but leads to penalizing a feature set. Such scenarios are not covered by state-of-the-art methods.
UBayFS allows to specify a full sysem of inequalities as side constraints --- thus, a user might be interested in selecting features from a low number of feature groups or avoid to select redundant features. UBayFS has the potential to

- cover both expert knowledge and information from an ensemble of elementary feature selectors jointy in a Bayesian framework, as well as 
- consider constraints to the feature set as side conditions during optimization. The framework is demonstrated in \autoref{fig:UBayFS}.

The presented $\mathtt{R}$ package UBayFS provides an implementation along with an interactive Shiny dashboard, which makes feature selection applicable for $\mathtt{R}$-users with different levels of expertise. The implementation allows the user to choose from multiple state-of-the-art feature selectors as elementary model types for building the generic ensemble of UBayFS. The current choice includes but are not limited to: 
- Laplacian score
- Fisher score
- mRMR
- Lasso
- decision tree
- recursive feature elimination
- HSIC Lasso

Prior weights can be specified for single features of for whole blocks and linear side constraints can be implemented in a handy way as well. Hence, the rather complex model is summarized in a user-friendly way and is ready to use. The runtime depends linearly on the number of ensemble models in the data-driven part. Further the optimization is speeded up with a greedy heuristic to find adjusting start values.


![At first, UbayFS elaborates information directly from data via ensemble feature selection. This information is merged with prior expert knowledge (a-priori feature weights) in a Bayesian model framework. Additionally, the user can include further side constraints such as a maximum number of features or cannot-link constraints between features. The final step is the optimization of the model's utility function including the side constraints.\label{fig:UBayFS}](UBayFS_concept.png)

# Concept and structure of UBayFS

As described in @jenul2022ubayfs, UBayFS is a Bayesian ensemble feature selector. The methodology is based on quantifying two random variables: $\boldsymbol{\theta}$, representing feature importances, and $\boldsymbol{y}$, representing evidence collected from the data, which is collected from a generic ensemble of elementary feature selection models. Both sources of information are combined using the Bayes' Theorem:

$$p(\boldsymbol{\theta}|\boldsymbol{y})\propto p(\boldsymbol{y}|\boldsymbol{\theta})\cdot p(\boldsymbol{\theta}).$$

In the framework of UBayFS,  $p(\boldsymbol{y}|\boldsymbol{\theta})$ represents the data-driven component (implemented via a multinomial likelihood) while $p(\boldsymbol{\theta})$ describes the user knowledge part modelled with a Dirichlet distribution. Due to the conjugate prior property of the Dirichlet distribution, the posterior parameter update has a tractable form and can be computed analytically. Statistically, we interpret the result from each elementary feature selector as realization from a multinomial distribution with parameters $\boldsymbol{\theta}$, where $\boldsymbol{\theta}\in[0,1]^N$ defines the success probabilities of sampling each feature in an individual feature selection and thus the success probability in the ensemble. Side constraints are represented by a system of linear inequalities $\boldsymbol{A}\cdot \boldsymbol{\delta}-\boldsymbol{b}\leq 0$, where $\boldsymbol{A}\in\mathbb{R}^{K\times N}$ and $\boldsymbol{b}\in\mathbb{R}^K$. $K$ is defined as the total number of constraints. In general, a feature set $\boldsymbol{\delta}$ ($\boldsymbol{\delta}$ is represented b a binary membership vector) is admissible only if $\left(\boldsymbol{a}^{(k)}\right)^T\boldsymbol{\delta} - b^{(k)} \leq 0$, according to the inadmissibility function

$$ \kappa_k(\boldsymbol{\delta}) = \left\{\begin{array}{l l}
    0 & \text{if}~ \left(\boldsymbol{a}^{(k)}\right)^T\boldsymbol{\delta} - b^{(k)} \leq 0 \\
    1 & \text{otherwise}.\end{array}\right.$$

In UBayFS, $\kappa_k(\boldsymbol{\delta})$ is substituted by a relaxed inadmissibility function $\kappa_{k,\rho}(\boldsymbol{\delta})$, given as

$$
\kappa_{k,\rho}(\boldsymbol{\delta}) = \left\{
    \begin{array}{l l}
    0 & \text{if}~\left(\boldsymbol{a}^{(k)}\right)^T\boldsymbol{\delta}\leq b^{(k)}\\
    1 & \text{if}~ \left(\boldsymbol{a}^{(k)}\right)^T\boldsymbol{\delta}> b^{(k)} \land \rho =\infty\\
    \frac{1-\xi_{k,\rho}}{1 + \xi_{k,\rho}} & \text{otherwise},
    \end{array}
    \right.
$$
    
where $\rho\in\mathbb{R}^+ \cup {\infty}$ denotes a relaxation parameter and
$\xi_{k,\rho} = \exp\left(-\rho \left(\left( \boldsymbol{a}^{(k)}\right)^T\boldsymbol{\delta} - b^{(k)}\right)\right)$ defines the exponential term of a logistic function. To handle $K$ different constraints for one feature selection problem, the joint admissibility function is given as

$$ \kappa(\boldsymbol{\delta})
    = 1 - \prod\limits_{k=1}^{K} \left(1 -\kappa_{k,\rho}(\boldsymbol{\delta})\right)$$

which originates from the idea that $\kappa = 1$ (maximum penalization) if at least one $\kappa_{k,\rho}=1$, while $\kappa=0$ (no penalization) if all $\kappa_{k,\rho}=0$. 

The target function to select an optimal feature set $\boldsymbol{\delta}$ is given by an expected utility given the posterior parameter $\boldsymbol{\delta} | \boldsymbol{y}$, as well as the inadmissibility function $\kappa(.)$.

$$\mathbb{E}_{\boldsymbol{\theta}|\boldsymbol{y}}[U(\boldsymbol{\delta}, \boldsymbol{\theta}(\boldsymbol{y}))] = \boldsymbol{\delta}^T \mathbb{E}_{\boldsymbol{\theta}|\boldsymbol{y}}[\boldsymbol{\theta}(\boldsymbol{y} )]-\lambda\kappa(\boldsymbol{\delta})\longrightarrow \underset{\boldsymbol{\delta}\in\{0,1\}^N}{\text{max}}
$$

The optimization happens with a genetic algorithm with a greedy search algorithm to find a proper start vector for the optimizaton.

# Package structure
The function `build.UBaymodel()` initializes the UBayFS model and computes the ensemble of elementary feature selectors. In the current version, among others, linear feature selectors such as Fisher and Laplacian scores from `Rdimtools` [@you2022rdimtools] or mrmr `mRMRe` [@de2013mrmre] or the non-linear HSIC Lasso are supported as options [@gselection]. In addition, the number of elementary models $M$ and the number of features selected in the elementary feature selectors are set. Inside the `build` function, the user can directly specify prior weights and constraints need to be specified with `buildConstraints()` where single-feature and block side constraints can be added to the model. UBayFS requires that at least one constraint, limiting the total number of features to be selected ("max-size"). Since constraints may be defined as soft constraints, the level of relaxation is steered with a parameter $\rho$. The weights for single features or feature blocks are set with `setWeights()`.

The functions `admissibility()` and `block_admissibility()` allow the user to evaluate the penalty term for a given feature set under a given set of constraints. After initializing the model and computing the ensembles, the `train.Ubaymodel()` function optimizes the feature set via a genetic algorithm [@scrucca2013ga] together with a greedy feature-set initialization, such that an optimal feature set can be returned. The greedy initialization decreases the runtime and leads to faster convergence of the feature set. Finally, the package features methods `print.UBaymodel()`, `plot.UBaymodel()`, `evaluateFS()` and `summary.UBaymodel()` to report and visualize the results. Two vignettes guide the user through the package and demonstrate how to method can be deployed in common application scenarios, including how user knowledge is specified, as well as how feature- and block-wise constraints are set.

# Interactive Shiny dashboard
 The function `runInteractive()` opens an interactive Shiny dashboard where the user can load and analyze data in an interactive way - however, the dashboard which offers the user an HTML interface is not recommended for larger datasets ($> 50$ features or $>1000$ samples) due to computational limitations - instead, functions should be called from the $\mathtt{R}$ console in such cases. \autoref{fig:Shiny} shows the dashboard with the different tabs:
 - **data**: Load the dataset and specify whether rownames, colnames or a block structure is given. A demo dataset is ready to be loaded and used for a firs touch on the package.
 - **likelihood**: Select elementary feature selectors for ensemble feature selection, the number of models $M$, the number of feature in each model and the ratio of the train-test split. Further, the dashboard allows the user to mix different elementary feature selector, salthough this option is not recommended due to limited stability [@seijo:ensembleSurvey].
 - **weights**: The prior feature weights set by the user. For block feature selection, it is possible to set weights for blocks, otherwise for single feature.
 - **constraints**: In this task, the user sets different constraints (at least a max-size constraint). The penalty $\rho$ can be varied here as well.
 - **feature selection**: In the last step of the dashboard, the optimization of the feature set takes place. A plot of the final result is produced - also, the model can be saved as an Rdata file and loaded to the dashboard, again.

 ![Illustration of the Shiny HTML dashboard.\label{fig:Shiny}](UBay_Shiny_Screenshot.png)

# Ongoing research
Based on the present UBayFS package, ongoing work focuses on the implementation of even more types of expert constraints and elementary feature selection models. Moreover, a Python package with similar functionality is planned in the future.

# Acknowledgements
We would like to thank Prof. Jürgen Pilz (University of Klagenfurt) and Prof. Oliver Tomic (Norwegian University of Life Sciences), who contributed to the development of the methodology and supported us with ideas and fruitful discussions, as well as Kristian Hovde Liland (Norwegian University of Life Sciences) for testing the $\mathtt{R}$ package.

# References
