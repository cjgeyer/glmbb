This is an [R](https://www.r-project.org/) package, which is on
[CRAN](https://cran.r-project.org/), that searches for all hierarchical
submodels of a specified
[GLM](https://en.wikipedia.org/wiki/Generalized_linear_model)
with criterion
([AIC](https://en.wikipedia.org/wiki/Akaike_information_criterion),
[BIC](https://en.wikipedia.org/wiki/Bayesian_information_criterion), or
[AICc](https://en.wikipedia.org/wiki/Akaike_information_criterion#AICc))
within a specified cutoff of the minimum value.  Alternatively, all the
same thing except with just graphical models rather than hierarchical models.
It uses the branch and bound algorithm so it does not have to fit all models.

There are also functions `isHierarchical` and `isGraphical` that say whether
a formula corresponds to a hierarchical or graphical model.

