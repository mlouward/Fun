# NoML learning track
##### [source site](https://weifoo.gitbooks.io/noml/content/)

## Introduction

* Bias / Variance

Bias error comes from wrong assumptions in the algorithm/implementation. High bias models oversimplify the data and has high error on the training data (underfitting).  
Variance error comes from the variability of the data. High variance leads to fitting the training set too well (overfitting).

&#xfeff;          |     Low Bias    |    High Bias
------------------|-----------------|----------------
**Low Variance**  |  *Ideal model*  |**Underfitting**
**High Variance** | **Overfitting** |   *Very bad*

* L1 and L2 regularization

Regularization is used in machine learning to prevent overfitting in regression problems. It does so by adding a penalty to the model as the complexity increases. This way, higher terms of the equation become negligible and the model is less prone to overfitting.  

1. L1 regularization

Also called *Lasso Regression*, it adds the absolute value of the coefficient to the cost function. This technique will shrink the less important features' coefficients to zero, and give sparse estimates. Therefore, it is more effective with a large number of features.  
(In high dimensional spaces, you get a lot of 0s and a few non-zero coefficients)

2. L2 regularization

Also caller *Ridge Regression*, it adds the square of the coefficient as a penalty. With a large coefficient, l2 reduces the magnitude of the parameters (it shrinks the parameter towards zero). Increases bias, reduces variance.  
  
(cf. isosurfaces for L1 and L2: L1 will more often set features to zero because of the corners)

&rarr; L2 is more stable