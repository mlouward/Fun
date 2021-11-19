from sklearn.datasets import make_regression
import matplotlib.pyplot as plt
import numpy as np


def load_data(path):
    with open(path, "r") as file:
        a = list(map(str.strip, file.readlines()))
    X, y = list(), list()
    for line in a:
        tmp = list(map(float, line.split(",")))
        X.append(tmp[0])
        y.append(tmp[1])
    return np.array(X), np.array(y)


def mse(predicted: np.ndarray, observed: np.ndarray) -> float:
    return np.mean(np.square(predicted - observed))


def dw(w, b, X, y):
    return -2 * np.sum(np.dot(X, (y - np.dot(X.T, w) - b))) / len(X)


def db(w, b, X, y):
    return -2 * np.sum(y - np.dot(X.T, w) - b) / len(X)


def get_better_parameters(w, b, X, y, step):
    grad_w = dw(w, b, X, y)
    grad_b = db(w, b, X, y)

    return w - step * grad_w, b - step * grad_b, grad_b, grad_w


def gradient_descent(w, b, X, y, step=0.001, iterations=1000):
    i = 0
    grad_b, grad_w = 1, 1
    loss_plot = []
    while i < iterations and (abs(grad_b) > 1e-2 and abs(grad_w) > 1e-2):
        w, b, grad_b, grad_w = get_better_parameters(w, b, X, y, step)
        i += 1
        loss_plot.append(mse(np.dot(w, X), y))
    print(i)
    return w, b, loss_plot


if __name__ == "__main__":
    # Initialization + get data
    # np.random.seed(1)
    # X, y = make_regression(n_samples=500, n_features=1, n_informative=1)
    X, y = load_data("data.csv")
    weight = 0
    bias = 0
    result = np.dot(weight, X) + bias
    print("Initial loss:", mse(result, y))
    # Gradient descent algorithm
    w, b, loss_plot = gradient_descent(weight, bias, X, y, 0.0001, 1000)
    result = np.dot(X.T, w) + b

    print(w, b, "\nLoss:", mse(result, y))

    f1 = plt.figure(1)
    plt.scatter(X, y, color="red")
    line_x = np.linspace(min(X), max(X), 100)
    line_y = w * line_x + b
    plt.plot(line_x, line_y)

    f2 = plt.figure(2)
    plt.plot(loss_plot)

    a = np.polyfit(X, y, 1)
    print(a)

    plt.show()
