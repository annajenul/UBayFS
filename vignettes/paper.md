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

The UBayFS package implements the framework with the same name, proposed in @jenul2022ubayfs, incorporating information from the data and prior knowledge from domain experts. With its generic setup, the method is extremely flexible in specifying side constraints, which is usually impossible with classical feature selectors. While, in most cases, a maximum number of features may be provided to a feature selection algorithm, further constraints between features are not covered by state-of-the-art methods. For instance, a user might be interested in selecting features from a low number of feature groups or might want to avoid selecting redundant features jointly. UBayFS has the potential to 
- cover both expert knowledge and information from an ensemble of elementary feature selectors, as well as 
- consider constraints on the feature set as side conditions during optimization. The framework is demonstrated in Figure \autoref{fig:UBayFS}.

The UBayFS package provides an R implementation together with an interactive Shiny dashboard, which makes feature selection applicable for R-users with different levels of expertise. UBayFS is an ensemble feature selection technique embedded in a Bayesian statistical framework. The method combines data and user knowledge, where the first is extracted via data-driven ensemble feature selection. Due to the generic structure of the method, any available feature selection method, such as Fisher score, Laplacian score, mRMR can be used for building the ensemble. The user can control the feature selection by assigning prior weights to features and penalizing specific feature combinations. In particular, the user can define a maximal number of selected features and must-link constraints (features must be selected together) or cannot-link constraints (features must not be selected together). Must-link constraint can also be used for block feature selection(a problem conventionally tackled by group lasso @sgl). UBayFS provides more flexibility to specify user input, and no latent parameter must be estimated. The cannot-link constraint is of special interest when multiple redundant features exist - putting them under one cannot-link constraint, only one feature can be selected. Using relaxed constraints, a parameter \rhoρ regulates the penalty shape. Hence, the violation of constraints can be valid but leads to penalizing a feature set.


![UBayFS concept.\label{fig:UBayFS}](UBayFS_concept.png)

# Concept and structure of UBayFS

As described in @jenul2022ubayfs, UBayFS is a Bayesian ensemble feature selector. The methodology is based on quantifying two random variables: $\boldsymbol{\theta}$, representing feature importances, and $\boldsymbol{y}$, representing evidence collected from the data, which are connected via Bayes' Theorem:

$$p(\boldsymbol{\theta}|\boldsymbol{y})\propto p(\boldsymbol{y}|\boldsymbol{\theta})\cdot p(\boldsymbol{\theta}).$$

Thus, UBayFS consists of a data-driven part $p(\boldsymbol{y}|\boldsymbol{\theta})$ (implemented via a multinomial likelihood) and a user knowledge part $p(\boldsymbol{\theta})$ (Dirichlet distributed prior distribution). As the Dirichlet distribution is the conjugated prior with respect to the multinomal likelihood, the posterior distribution follows a Dirichlet distributed as well. To collect evidence from the data, a similar approach to the ensemble feature selector RENT [@jenul2021rent] is pursued: UBayFS trains an ensemble of $M$ elementary feature selectors, where each elementary feature selector $1,…,M$ suggests a feature set, represented by a binary membership vector $\boldsymbol{\delta}^{(m)}$. In $\boldsymbol{\delta}^{(m)}$, a component $\delta_i^{(m)}=1$ indicates that feature $i\in\{1,\dots,N\}$ is selected by model $m$, and $\delta_i^{(m)}=0$ otherwise. Statistically, we interpret the result from each elementary feature selector as realization from a multinomial distribution with parameters $\boldsymbol{\theta}$ and $l$, where $\boldsymbol{\theta}\in[0,1]^N$ defines the success probabilities of sampling each feature in an individual feature selection and $l$ corresponds to the number of feature selected in $\boldsymbol{\delta}^{(m)}$. Constraints are represented as a linear optimization problem $\boldsymbol{A}\cdot \boldsymbol{\delta}-\boldsymbol{b}\leq 0$, where $\boldsymbol{A}\in\mathbb{R}^{K\times N}$ and $\boldsymbol{b}\in\mathbb{R}^K$. $K$ is defined as the total number of constraints. In general, a feature set $\boldsymbol{\delta}$ is admissible only if $\left(\boldsymbol{a}^{(k)}\right)^T\boldsymbol{\delta} - b^{(k)} \leq 0$, according to the inadmissibility function

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


# Package structure
The function `build.UBaymodel()` initializes the UBayFS model and computes the ensemble of elementary feature selectors. In the current version, among others, linear feature selectors such as Fisher and Laplacian scores from `Rdimtools`[@you2022rdimtools] or mrmr `mRMRe`[@de2013mrmre] or the non-linear HSIC Lasso are supported as options [@gselection]. In addition, the number of elementary models $M$ and the number of features selected in the elementary feature selectors are set. Inside the build function, the user can directly specify prior weights and constraints with `buildConstraints()` where single-feature and block side constraints can be added to the model. At least a max-size constraint must be set to ensure that the model selects a reasonable number of features (better explanation??). Since constraints may be defined as soft constraints, the level of relaxation is steered with a parameter $\rho$. The weights for single features or feature blocks are set with `setWeights()`.Further, parameters for the optimization step are initialized. 

The functions `admissibility()` and `block_admissibility()` allow the user to evaluate the penalty term for a given feature set for a given set of constraints. After initializing the model and computing the ensembles, the `trainUbaymodel()` function optimizes the feature set via a genetic algorithm [@scrucca2013ga], such that an optimal feature set can be returned. Finally, the package features methods `print.UBaymodel()`, `plot.UBaymodel()`, `evaluateFS()` and `summary.UBaymodel()` to visualize the results. Two vignettes guide the user through the package and illustrate how to method can be deployed in different scenarios, including how to define user knowledge, how to set feature-wise constraints, and how to apply constraints on feature blocks.

# Interactive Shiny dashboard
 The function `runInteractive()` opens the Shiny dashboard where the user can load and analyze data in an interactive way - however, the dashboard is not recommended for larger datasets ($> 50$ features or $>1000$ samples) due to computational limitations - instead, functions should be called from the R console in such cases. \autoref{fig:Shiny} shows the dashboard with the different tasks:
 - **data**: Load the dataset and specify whether rownames, colnames or a block structure is given.
 - **likelihood**: Select elementary feature selectors for ensemble feature selection, the number of models $M$, the number of feature in each model $1,...,M$ and the size of the train-test split. It is possible to mix different elementary feature selectors.
 - **weights**: The prior feature weights set by the user. For block feature selection, it is possible to set weights for blocks, otherwise for single feature.
 - **constraints**: In this task, the user sets different constraints (at least a max-size constraint). The penalty $\rho$ can be varied here as well.
 - **feature selection**: In the last step of the dashboard, the optimization of the feature set takes place. The final result is plotted and can be downloaded.

 ![Shiny dashboard.\label{fig:Shiny}](UBay_Shiny_Screenshot.png)

# Ongoing research
Based on the present UBayFS package, ongoing work focuses on the implementation of even more types of expert constraints and elementary feature selection models. Moreover, a Python package with similar functionality is planned in the future.

# Acknowledgements
We would like to thank Prof. Jürgen Pilz (University of Klagenfurt) and Prof. Oliver Tomic (Norwegian University of Life Sciences), who contributed to the development of the methodology and supported us with ideas and fruitful discussions, as well as Kristian Hovde Liland (Norwegian University of Life Sciences) for testing the R package.

# References
