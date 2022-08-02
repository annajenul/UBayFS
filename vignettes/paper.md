---
title: "UBayFS"
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

# Summary

Moder data acquisition techniques in different domains like healthcare or economy allow the generation of an enormous number of features daily. The high amount of tracked measurements, i.e., features, impedes the analysis with modern machine learning techniques due to the higher model complexity. In many cases the number of features exceeds the number of samples which leads to underdetermined systems from a mathematical perspective., making reliable data analysis even more difficult. Hence, lowering the number of features can reduce (a) the complexity of a machine learning algorithm and (b) noise within the data. Moreover, feature selection helps to interpret the dataset by restricting it to the most important and influencing features. In the longer run, costs can be reduced by not collecting features from expensive machines when the features are not necessary to be collected.   

In different research fields, commonly two sources of information are available: information from the data and profound domain knowledge from experts. Combining the two sources to select relevant features can improve machine learning model performances and interpretation, especially in high-dimensional datasets, where the selected features are often very unstable, i.e., when the feature selection algorithm is applied multiple times on the same data, in each run different features are selected. Preliminary information from experts stabilizes the feature selection process and supports interpretation, such as in cases where features must or must not be selected together. Stability is essential in feature selection to provide reliable and trustworthy analyses.  

# Statement of need

The UBayFS package implements the framework proposed in @jenul2021user, together with an interactive Shiny dashbord, which makes UBayFS applicable to R-users with different levels of expertise. UBayFS is an ensemble feature selection technique, embedded in a Bayesian statistical framework. The method combines data and user knowledge, where the first is extracted via data-driven ensemble feature selection. Due to the generic structure of the method, any available feature selection method, such as Fisher score, Laplacian score, mRMR can be used for building the ensemble. The user can control the feature selection by assigning prior weights to features and penalizing specific feature combinations. In particular, the user can define a maximal number of selected features and must-link constraints (features must be selected together) or cannot-link constraints (features must not be selected together). Using relaxed constraints, a parameter $\rho$ regulates the penalty shape. Hence, violation of constraints can be valid but leads to a lower target value of the feature set that is derived from the violated constraints.  

The must-link constraints make the final feature selection set more stable, meaning that when two features are equally important, it is random which one is selected, while with a must-link constraint, both are selected. On the other hand, the cannot-link constraint selects only one feature from a set of highly correlated ones. Furthermore, must-link constraint can be used for block feature selection, what makes UBayFS a competitor to the groupLasso which is implemented in R [..]. UBayFS provides more flexibility regarding user-input than groupLasso, and no latent parameter must be estimated.  

# Concept and structure of UBayFS

Based on the Bayes theorem two random variables $\theta$ representing the unknown feature importances and $y$ representing the distribution of the data over ensembles

$$p(\theta|y)\propto p(y|\theta)\cdot p(\theta),$$

UBayFS consists of a data-driven part $p(y|\theta)$ (multinomial likelihood) and the user knowledge as prior $p(\theta)$(Dirichlet distributed prior). As the Dirichlet distribution is the conjugated prior for a multinomal likelihood , the posterior distribution is Dirichlet distributed as well. Similar to the ensemble feature selector RENT [@jenul2021rent] UBayFS builds $M$ ensembles of elementary feature selectors, where each elementary feature selector $1,…,M$ selects features, meaning that the count of each feature is between $0$ (never selected) and $M$ (always selected). In the binary membership vector $\delta^{m}$, a component $\delta_i^m=1$ indicates that feature $i\in\{1,\dots,N\}$ is selected in selection $m$, and $\delta_i^m=0$ otherwise. Statistically, we interpret the result from each elementary feature selector as realization from a multinomial distribution with parameters $\theta$ and $l$, where $\theta\in[0,1]^N$ defines the success probabilities of sampling each feature in an individual feature selection and $l$ corresponds to the number of feature selected in $\delta^{m}$.Constraints are represented as a linear optimization problem $A\cdot \delta-b\leq 0$, where $A\in\mathbb{R}^{K\times N}$ and $b\in\mathbb{R}^K$. $K$ is defined as the total number of constraints. In general, a feature set $\delta$ is admissible only if $\left(a^{(k)}\right)^T\delta - b^{(k)} \leq 0$, according to the inadmissibility function

$$ \kappa_k(\delta) = \left\{\begin{array}{l l}
    0 & \text{if}~ \left(a^{(k)}\right)^T\delta - b^{(k)} \leq 0 \\
    1 & \text{otherwise},\end{array}\right.$$

In UBayFS, $\kappa_k(\delta)$ is substituted by a relaxed inadmissibility function $\kappa_{k,\rho}(\delta)$, given as

$$
\kappa_{k,\rho}(\delta) = \left\{
    \begin{array}{l l}
    0 & \text{if}~\left(a^{(k)}\right)^T\delta\leq b^{(k)}\\
    1 & \text{if}~ \left(a^{(k)}\right)^T\delta> b^{(k)} \land \rho =\infty\\
    \frac{1-\xi_{k,\rho}}{1 + \xi_{k,\rho}} & \text{otherwise},
    \end{array}
    \right.
$$
    
where $\rho\in\mathbb{R}^+ \cup {\infty}$ denotes a relaxation parameter and
$\xi_{k,\rho} = \exp\left(-\rho \left(\left( a^{(k)}\right)^T\delta - b^{(k)}\right)\right)$ defines the exponential term of a logistic function. To handle $K$ different constraints for one feature selection problem, the joint admissibility function is given as

$$ \kappa(\delta)
    = 1 - \prod\limits_{k=1}^{K} \left(1 -\kappa_{k,\rho}(\delta)\right)$$

which originates from the idea that $\kappa = 1$ (maximum penalization) if at least one $\kappa_{k,\rho}=1$, while $\kappa=0$ (no penalization) if all $\kappa_{k,\rho}=0$. 

The target function to select an optimal feature set $\delta$ is given by an expected utility given the posterior parameter $\delta | y$, as well as the inadmissibility function $\kappa(.)$.

$$\mathbb{E}_{\theta|y}[U(\delta, \theta(y))] = \delta^T \mathbb{E}_{\theta|y}[\theta(y)]-\lambda\kappa(\delta)\longrightarrow \underset{\delta\in\{0,1\}^N}{\text{max}}
$$

The function `build.UBaymodel()` initializes the UBayFS model and performs ensemble feature selection In the current version, linear feature selectors such as Fisher and Laplacian scores from `Rdimtools`[you2020rdimtools] or mrmr `mRMRe`[@de2013mrmre] but also the non-linear hsic lasso can be used [@gselection] . With help of `buildConstraints()`, single-feature and block side constraints can be added to the model. The strength of the constraints is defined by the $\rho$ parameter and with the `admissibility()` and `block_admissibility()` functions, the user can check the admissibility of the defined constraints. After initializing the model and computing the ensembles, the `trainUbaymodel()` function combines likelihood and prior to the final model and optimized the feature set with a genetic algorithm [@scrucca2013ga]. In addition, it is possible to change constraints, block constraints, prior weights and optimization parameters and retrain the model. The methods `printUbaymodel()`, `plotUbaymodel()`, `evaluateFS()` and `summary()` help to visualize the method and to give some output on the properties and settings. The function `runInteractive()` opens the Shiny dashboard, where the user can load and analyze data. Furthermore, two vignettes guide the user through the package and illustrate how to use it in different scenarios.

# Ongoing research
- in ongoing project?
- students ? 
- published ? 

# Acknowledgements

# References
