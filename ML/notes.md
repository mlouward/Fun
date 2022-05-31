# NoML learning track notes

[source site](https://weifoo.gitbooks.io/noml/content/)

<!-- @import "[TOC]" {cmd="toc" depthFrom=2 depthTo=6 orderedList=false} -->

<!-- code_chunk_output -->

- [Bias / Variance](#bias-variance)
- [L1 and L2 regularization](#l1-and-l2-regularization)
- [Fighting over/underfitting](#fighting-overunderfitting)
  - [Feature selection](#feature-selection)
  - [Regularization](#regularization)
  - [Dimension Reduction](#dimension-reduction)
  - [Model ensemble](#model-ensemble)
    - [Bagging](#bagging)
    - [Boosting](#boosting)
      - [Bagging vs Boosting](#bagging-vs-boosting)
- [Generative vs Discriminative](#generative-vs-discriminative)
- [SVM and Logistic Regression](#svm-and-logistic-regression)
- [Basic fully-connected Neural network using NumPy](#basic-fully-connected-neural-network-using-numpy)

<!-- /code_chunk_output -->

## Bias / Variance

Bias error comes from **wrong assumptions** in the algorithm/implementation. High bias models oversimplify the data and have high error on the training data (underfitting).

Variance error comes from the **variability of the data**. High variance leads to fitting the training set too well (overfitting).

| &#xfeff;          | Low Bias        | High Bias        |
| ----------------- | --------------- | ---------------- |
| **Low Variance**  | _Ideal model_   | **Underfitting** |
| **High Variance** | **Overfitting** | Very bad         |

## L1 and L2 regularization

Regularization is used in machine learning to prevent overfitting in regression problems. It does so **by adding a penalty** to the model as the complexity increases. This way, higher terms of the equation become negligible and the model is less prone to overfitting.

- L1 regularization

Also called _Lasso Regression_, it adds the absolute value of the coefficient to the cost function. This technique will shrink the less important features' coefficients to zero, and give **sparse estimates** (feature selection effect + more storage efficient). Therefore, it is more effective with a large number of features. It is nonlinear, there is no closed formsolution.
(In high dimensional spaces, you get a lot of 0s and a few non-zero coefficients).

- L2 regularization

Also caller _Ridge Regression_, it adds the square of the coefficient as a penalty. With a large coefficient, l2 **reduces the magnitude of the parameters** (it shrinks the parameter towards zero). Increases bias, reduces variance. It is also quadratic in the weights vector $w$ (less computational complexity than L1).

(cf. isosurfaces for L1 and L2: L1 will more often set features to zero because of the corners)

$\Rightarrow$ **L2 is more stable because of the square**
$\Rightarrow$ ElasticNet is a combination of L1 and L2 regularization, "should" be used in practice.

## Fighting over/underfitting

2 main ways to manage the bias/variance tradeoff:

1. Model selection

    - Feature selection
    - Regularization
    - Dimension Reduction

2. Model ensemble
    - Bagging (reduces variance)
    - Boosting (reduces bias)

### Feature selection

Best subset selection = Start from a model $\mathcal{M_0}$ with no features, and fit k models with 1 feature. Choose the best resulting model (smallest RSS $\Leftrightarrow$ largest R²) and repeat up to $M$ features
Select a **single** best model among $\mathcal{M_0}, \dots ,\mathcal{M}_M$ models (using CV/AIC/BIC/...)
Problem : **Too computationally expensive + overfitting**

In practice, use of meta-heuristics:

- Filter (rank features, select the best ones)
- Embedded (built-in, e.g. lasso/decision tree)
- Wrapper (forward selection or backward selection)

To choose the optimal model, RSS and R² are nott suitable because we need to evaluate the test errror.

- Direct estimation with validation set (LOO cv, in practice k-fold)
- Adjustment to training error to account for model complexity ($AIC$/$BIC$/Adjusted $R^2$)

### Regularization

- Shrinkage methods ($L1$, $L2$, $ElasticNet$). Use CV to compute the best $\lambda$ parameter for regularization.

### Dimension Reduction

Transforms the original features and learn a model on the transformed features, whereas other methods worked on the original features.
Examples: PCA, ICA, LDA, Self-organizing maps, autoencoders,...

- PCA

Keep the most information as possible, using _variance_ as a measure of it
Gives a new orthogonal basis whose axes align with the max variance of the original data. Example: eigenfaces, PCA on images of faces gives us a small list of features to look for to recognize a human face. Each obtained eigenvector can be represented as an image of a 'face feature'

Caveats:
Fails when data has multiple clusters.
Greatest variance might not mean most information.
PCA is linear, but data often lies on nonlinear spaces.

### Model ensemble

Meta-algorithms. Instead of learning one model, learn a set of models and combine them intelligently. Typically improves accuracy by a lot.

#### Bagging

Reduces variance without increasing bias.

1. Generate $\mathcal{B}$ boostrap samples of the training data (random sampling w/ replacement).
1. Train a model on each sample
1. Prediction = Majoriti vote for classification, average of prediction for reg.

$\Rightarrow$ Decreases variance due to averaging ($Var(\bar X) = \frac{Var(X)}{N}$)
$\Rightarrow$ Reduces variation (i.e. overfitting) especially for unstable learners, works particularly well with decision trees (random forest).
Does not help when high bias.

#### Boosting

Reduces bias without increasing variance.

Sequentially train weak learners:

1. Train model on train set
1. compute training error
1. Increase weights on train cases where model is wrong
1. Repeat training and weights update

Final model = weighted prediciton of each model

Example models: AdaBoost for classification

##### Bagging vs Boosting

|                | Bagging        | Boosting               |
| -------------- | -------------- | ---------------------- |
| reduces        | variance       | bias                   |
| stable models  | **not better** | might help             |
| noisy datasets | no problem     | might hurt performance |

In practice, bagging almost always good. Boosting helps more than bagging but can be detrimental as well. The weights grow exponentially.
Bagging = easy to parallelize.

## Generative vs Discriminative

**Generative** models learn a **joint distribution** over the data $p(x, y)$ and can be used to generate samples. Bayes' rule allow to get the conditional distribution $p(y|x)$ from the joint distribution.
**Discriminative** models learn a **conditional distribution** over the data $p(y|x)$ and can be used to predict the class of a sample.

**Discriminative** models **generally outperform generative** ones in classification tasks.

## SVM and Logistic Regression

Logistic regression focuses on **maximizing the likelihood**. The further the data lies from the separating hyperplane (on the correct side), the happier LR is.

An SVM tries to find the separating hyperplane that maximizes the distance of the **closest points** to the margin (the support vectors). If a point is not a support vector, it doesn’t really matter.
SVM can use nonlinear kernels if data can't be split linearly. Gaussian RBF kernel is a good choice in practice.

We can express [SVM as a derivation of LR](http://www.cs.toronto.edu/~kswersky/wp-content/uploads/svm_vs_lr.pdf#page=7).

## Basic fully-connected Neural network using NumPy

See [`neural_net.py`](./neural_net.py)
