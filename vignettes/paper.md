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

# Summary

Across domains, modern data acquisition techniques allow the generation of an enormous volume of data. The high number of tracked measurements, i.e., features, confronts statistical and machine learning models with challenges due to the curse of dimensionality. In many cases, the number of features exceeds the number of samples, leading to underdetermined systems from a mathematical perspective, resulting in unreliable models that are prone to overfit. Hence, restricting the input to the most informative features can help to reduce (a) the complexity of a machine learning algorithm and (b) noise within the data. Moreover, feature selection paves the way for interpretations within the dataset since factors with influence on the target variable may be identified. In the longer run, costs can be reduced by avoiding collecting features from expensive sources if their individual contribution to the model is low.   

Two sources of information are commonly available when training machine learning models: information from the data and domain knowledge from experts. Combining these two sources when selecting relevant features can improve machine learning model performances and interpretation, especially in high-dimensional datasets. High dimensionality in particular causes unstable feature selection results, i.e., minor changes in the training data result in different feature sets being selected in each run. Prior information from domain experts, along with taking multiple model runs into account (ensemble feature selection) stabilizes the feature selection process and provides reliable and trustworthy results.

Furthermore, feature selection is often inflexible since side conditions cannot be properly specified. While, in most cases, a maximum number of features may be provided to a feature selection algorithm, further constraints between features are not covered by state-of-the-art methods. For instance, a user might be interested in selecting features from a low number of feature blocks (representing distinct data sources), or might want to avoid selecting highly correlated features in common. UBayFS is an ensemble feature selector with the potential to (a) cover both expert knowledge and information from an ensemble of elementary feature selectors, as well as (b) consider constraints on the feature set as side conditions during optimization.

# Statement of need

The UBayFS package implements the framework with the same name, proposed in @jenul2021user, together with an interactive Shiny dashboard, which makes UBayFS applicable for R-users with different levels of expertise. UBayFS is an ensemble feature selection technique embedded in a Bayesian statistical framework. The method combines data and user knowledge, where the first is extracted via data-driven ensemble feature selection. Due to the generic structure of the method, any available feature selection method, such as Fisher score, Laplacian score, mRMR can be used for building the ensemble. The user can control the feature selection by assigning prior weights to features and penalizing specific feature combinations. In particular, the user can define a maximal number of selected features and must-link constraints (features must be selected together) or cannot-link constraints (features must not be selected together). Must-link constraint can also be used for block feature selection, which makes UBayFS a competitor to the group lasso, where in their paper @jenul2021user, use a sparse version of group lasso implemented in the `SGL R` package [@sgl]. UBayFS provides more flexibility regarding user input than group lasso, and no latent parameter must be estimated. The cannot-link constraint is of special interest when multiple high correlated features exist - putting them under one cannot-link constraint, only one feature can be selected. Using relaxed constraints, a parameter $\rho$ regulates the penalty shape. Hence, violation of constraints can be valid but leads to a lower target value of the feature set that is derived from the violated constraints.  


# Concept and structure of UBayFS

As described in @jenul2021user, UBayFS is a Bayesian ensemble feature selector. The methodology is based on quantifying two random variables: $\theta$, representing feature importances, and $y$, representing evidence collected from the data, which are connected via Bayes' Theorem:

$$p(\theta|y)\propto p(y|\theta)\cdot p(\theta).$$

Thus, UBayFS consists of a data-driven part $p(y|\theta)$ (implemented via a multinomial likelihood) and a user knowledge part $p(\theta)$ (Dirichlet distributed prior distribution). As the Dirichlet distribution is the conjugated prior with respect to the multinomal likelihood, the posterior distribution is Dirichlet distributed as well. To collect evidence from the data, a similar approach to the ensemble feature selector RENT [@jenul2021rent] is pursued: UBayFS builds $M$ ensembles of elementary feature selectors, where each elementary feature selector $1,…,M$ returns a feature set, represented by a binary membership vector $\delta^{m}$. In $\delta^{(m)}$, a component $\delta_i^{(m)}=1$ indicates that feature $i\in\{1,\dots,N\}$ is selected in selection $m$, and $\delta_i^{(m)}=0$ otherwise. Statistically, we interpret the result from each elementary feature selector as realization from a multinomial distribution with parameters $\theta$ and $l$, where $\theta\in[0,1]^N$ defines the success probabilities of sampling each feature in an individual feature selection and $l$ corresponds to the number of feature selected in $\delta^{(m)}$. Constraints are represented as a linear optimization problem $A\cdot \delta-b\leq 0$, where $A\in\mathbb{R}^{K\times N}$ and $b\in\mathbb{R}^K$. $K$ is defined as the total number of constraints. In general, a feature set $\delta$ is admissible only if $\left(a^{(k)}\right)^T\delta - b^{(k)} \leq 0$, according to the inadmissibility function

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

The function `build.UBaymodel()` initializes the UBayFS model and computes the ensemble of elementary feature selector runs. In the current version, linear feature selectors such as Fisher and Laplacian scores from `Rdimtools`[you2020rdimtools] or mrmr `mRMRe`[@de2013mrmre] and the non-linear HSIC Lasso can be used [@gselection]. Using `buildConstraints()`, single-feature and block side constraints can be added to the model. Since constraints are, by default, defined as soft constraints, the level of relaxation is steered with a parameter $\rho$. The functions `admissibility()` and `block_admissibility()` allow the user to evaluate the penalty term for a given feature set with respect to a set of constraints. After initializing the model and computing the ensembles, the `trainUbaymodel()` function optimizes the feature set via a genetic algorithm [@scrucca2013ga], such that an optimal feature set can be returned. Finally, the package features methods `print.UBaymodel()`, `plot.UBaymodel()`, `evaluateFS()` and `summary.UBaymodel()` to visualize the results. The function `runInteractive()` opens the Shiny dashboard, where the user can load and analyze data in an interactive way - however, the dashboard is for demonstration only, and is not recommended for larger datasets. Two vignettes guide the user through the package and illustrate how to method can be deployed in different scenarios.

# Ongoing research
Based on the present UBayFS package, ongoing work focuses on the implementation of even more types of expert constraints and elementary feature selection models. Moreover, a Python package with similar functionality is planned in the future.

# Acknowledgements
We would like to thank the contributors to the methodology of UBayFS, Prof. Jürgen Pilz (University of Klagenfurt) and Prof. Oliver Tomic (Norwegian University of Life Sciences) for their ideas and fruitful discussions, as well as Kristian Hovde Liland (Norwegian University of Life Sciences) for testing the R package.

# References
