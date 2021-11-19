# NoML learning track notes

##### [source site](https://weifoo.gitbooks.io/noml/content/)

## Introduction

1. #### Bias / Variance

Bias error comes from **wrong assumptions** in the algorithm/implementation. High bias models oversimplify the data and have high error on the training data (underfitting).

Variance error comes from the **variability of the data**. High variance leads to fitting the training set too well (overfitting).

| &#xfeff;          | Low Bias        | High Bias        |
| ----------------- | --------------- | ---------------- |
| **Low Variance**  | _Ideal model_   | **Underfitting** |
| **High Variance** | **Overfitting** | Very bad         |

2. #### L1 and L2 regularization

Regularization is used in machine learning to prevent overfitting in regression problems. It does so **by adding a penalty** to the model as the complexity increases. This way, higher terms of the equation become negligible and the model is less prone to overfitting.

- <ins>L1 regularization

Also called _Lasso Regression_, it adds the absolute value of the coefficient to the cost function. This technique will shrink the less important features' coefficients to zero, and give **sparse estimates**. Therefore, it is more effective with a large number of features.
(In high dimensional spaces, you get a lot of 0s and a few non-zero coefficients)

- <ins>L2 regularization

Also caller _Ridge Regression_, it adds the square of the coefficient as a penalty. With a large coefficient, l2 **reduces the magnitude of the parameters** (it shrinks the parameter towards zero). Increases bias, reduces variance.

(cf. isosurfaces for L1 and L2: L1 will more often set features to zero because of the corners)

&rarr; **L2 is more stable**

3. #### SVM and Logistic Regression

Logistic regression focuses on **maximizing the likelihood**. The farther the data lies from the separating hyperplane (on the correct side), the happier LR is.

An SVM tries to find the separating hyperplane that maximizes the distance of the **closest points** to the margin (the support vectors). If a point is not a support vector, it doesn’t really matter.
SVM can use nonlinear kernels if data can't be split linearly

We can express [SVM as a derivation of LR](http://www.cs.toronto.edu/~kswersky/wp-content/uploads/svm_vs_lr.pdf#page=7).

4. #### Basic fully-connected Neural network using NumPy

See [`neural_net.py`](./neural_net.py)
