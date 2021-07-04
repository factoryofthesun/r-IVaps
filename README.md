# r-IVaps
**[Overview](#overview)** | **[Installation](#installation)** | **[Usage](#usage)** | **[Acknowledgements](#acknowledgements)**
<details>
<summary><strong>Table of Contents</strong></summary>

- [Overview](#overview)
  - [ML as a Data Production Service](#ml-as-a-data-production)
  - [Framework](#framework)
  - [r-IVaps](#r-IVaps)
    - [Supported ML Frameworks](#supported-ml-frameworks)
- [Installation](#installation)
  - [Dependencies](#dependencies)
- [Usage](#usage)
  - [Vignettes](#vignettes)
- [Versioning](#versioning)
- [Contributing](#contributing)
- [Citation](#citation)
- [License](#license)
- [Authors](#authors)
- [Acknowledgements](#acknowledgements)
  - [References](#references)
  - [Related Projects](#related-projects)
</details>

# Overview

## ML as a Data Production Service
Today’s society increasingly resorts to machine learning (“AI”) and other algorithms for decision-making and resource allocation. For example, judges make legal judgements using predictions from supervised machine learning (descriptive regression). Supervised learning is also used by governments to detect potential criminals and terrorists, and financial companies (such as banks and insurance companies) to screen potential customers. Tech companies like Facebook, Microsoft, and Netflix allocate digital content by reinforcement learning and bandit algorithms. Uber and other ride sharing services adjust prices using their surge pricing algorithms to take into account local demand and supply information. Retailers and e-commerce platforms like Amazon engage in algorithmic pricing. Similar algorithms are invading into more and more high-stakes treatment assignment, such as education, health, and military.

All of the above, seemingly diverse examples share a common trait: An algorithm makes decisions based only on observable input variables the data-generating algorithm uses. Conditional on the observable variables, therefore, algorithmic treatment decisions are (quasi-)randomly assigned. This property makes algorithm-based treatment decisions **an instrumental variable we can use for measuring the causal effect of the final treatment assignment**. The algorithm-based instrument may produce regression-discontinuity-style local variation (e.g. machine judges), stratified randomization (e.g. several bandit and reinforcement leaning algorithms), or mixes of the two. Narita & Yata 2021 introduces the formal framework and characterizes the sources of causal effect identification.[[1]](#1)

## Framework
On a high level, the AlgoIsExp method works by exploiting the fact that all inputs into a machine learning model are observable to the researcher, and thus controlling for these inputs allows one to make use of the feature space in which machine learning algorithms are effectively stochastic -- there is random assignment between two (or more) possible discrete outputs. For algorithm-based treatment decisions, we know that the algorithm output strongly affects the ultimate treatment decision (either because the algorithm *is* the treatment decision, or a final arbiter relies on the algorithm output to make the final assignment). Thus in theory, we should be able to use the output predictions of algorithms which mediate any treatment decision regime to estimate the causal effects of the treatments.
<p align="center" width = "100%">
  <img src="/images/ml_natural_experiment_diagram.PNG" width="50%" height="50%"/>
</p>

Below is the basic setup to the framework.
<p align="center" width = "100%">
  <img src="/images/framework_1.PNG" width="50%" height="50%"/>
</p>

Below are the assumptions on the characteristics of the algorithm for this method to apply.
<p align="center" width = "100%">
  <img src="/images/framework_2.PNG" width="50%" height="50%"/>
</p>

The Approximate Propensity Score (APS) is an object that captures all the experimental information for an algorithm. The 2nd image below illustrates an example of this for an algorithm that takes 2-D inputs and produces output predictions between 0 and 1. As highlighted, APS helps to identify the sources of causal effects generated by the algorithm -- the subsets of the support space for which the algorithm's recommendation is non-deterministic.
<p align="center" width = "100%">
  <img src="/images/framework_3.PNG" width="50%" height="50%"/>
  <img src="/images/aps_chart.PNG" width="50%" height="50%"/>
</p>

Narita & Yata 2021 proposes an instrumental variables approach to causal effect estimation. In particular, a 2SLS regression framework instrumenting treatment assignment with the algorithmic recommendation, controlling for estimated APS.
<p align="center" width = "100%">
  <img src="/images/framework_2sls.png" width="40%" height="40%"/>
</p>

For more detail on the theoretical framework, please refer to the paper "Machine Learning is Natural Experiment" (Narita & Yata, forthcoming).

## r-IVaps
The IVaps package is an implementation of the treatment effect estimation method and paper described above. This package provides functions for the two primary estimation steps -- APS estimation and treatment effect estimation -- and is compatible with all major machine-learning libraries in R.

### Supported ML Frameworks
Below are the supported ML libraries in R. Please contact the package author if there is missing support for a library you are using.
- mlr3
- caret
- randomForest
- e1071
- bestridge
- rpart
- tree
- custom functions

# Installation
This package is still in its development phase, but you can compile the package from source
```bash
git clone https://github.com/factoryofthesun/r-IVaps
R CMD build IVaps
R CMD INSTALL IVaps_0.0.0.9000.tar.gz
```

## Dependencies
Below are the required imports. Additional packages are needed to use the parallelized and diagnostic functions.
- data.table (>= 1.13.6)
- ivreg (>= 0.5)

# Usage
Below is an abstracted example of how to use the functions in this package to estimate LATE, given a trained model and historical treatment data.
```r
aps <- estimate_aps(data, algorithm_object, ml_type="library of algorithm object", Xc=c("vector", "of", "continuous", "variable", "names"), Xd=c("vector", "of", "discrete", "variable", "names"))
ivreg_model <- estimate_treatment_effect(aps = aps, Y = outcome_vector, Z=treatment_reccomendation, D = treatment_assignment)
summary(ivreg_model)
```

## Vignettes
- [Introduction and Quickstart](https://github.com/factoryofthesun/r-IVaps/vignettes/IVaps-intro.Rmd)
- [Iris Simulation Example](https://github.com/factoryofthesun/r-IVaps/vignettes/IVaps-iris_sim.Rmd)

# Versioning

# Contributing
Please contact Richard Liu at <richard.liu@yale.edu> for details on how to contribute to this project.

# Citation

# License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

# Authors

# Acknowledgements

## References
<a id="1">[1]</a>
1. Narita, Yusuke and Yata, Kohei. Algorithm is Experiment (forthcoming). 2021.

## Related Projects
