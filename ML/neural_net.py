import sys
from types import new_class
from typing import NewType
import numpy as np
import matplotlib.pyplot as plt


def sigmoid(x):
    return 1 / (1 + np.exp(-x))


def sigmoid_derivative(x):
    return x * (1 - x)


# Neurons
train_inputs = np.array([[0, 0, 1], [1, 1, 1], [1, 0, 1], [0, 1, 1]])
train_outputs = np.array([[0, 1, 1, 0]]).T

# np.random.seed(1)
synaptic_weights = 2 * np.random.random((3, 1)) - 1

error = np.ones((3, 1))
i = 0

while np.any(np.abs(error) > 0.01):
    i += 1
    # Forward propagation
    input_layer = train_inputs
    outputs = sigmoid(np.dot(input_layer, synaptic_weights))

    # Back propagation
    error = train_outputs - outputs
    adjustment = error * sigmoid_derivative(outputs)
    synaptic_weights += np.dot(input_layer.T, adjustment)

print(i, "iterations, results:")
print("Errors:\n", error)
print("Output:\n", outputs)
print("Weights:\n", synaptic_weights)