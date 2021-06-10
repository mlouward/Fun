from PIL import Image

import numpy as np
import matplotlib.pyplot as plt


def create_image(path: str):
    """
    Convert an input image to a squared, 2D grayscale matrix
    """
    image = Image.open(path)
    image_bw = image.convert("L")
    matrix = np.asarray(image_bw)
    return image_bw, matrix, image


def gaussian_blur(kernel_size, verbose=False):
    """
    Returns the gaussian kernel matrix of given size with appropriate sigma
    """
    return _gaussian_kernel(kernel_size, sigma=(kernel_size-1)/18, verbose=verbose)


def _gaussian_kernel(size, sigma, verbose=False):
    kernel_1D = np.linspace(-(size // 2), size // 2, size)
    for i in range(size):
        kernel_1D[i] = _dnorm(kernel_1D[i], 0, sigma)

    # from 1D to 2D
    kernel_2D = np.outer(kernel_1D.T, kernel_1D.T)

    # Normalize the values
    # kernel_2D *= 1.0 / kernel_2D.max()

    if verbose:
        plt.imshow(kernel_2D, interpolation='none', cmap='gray')
        plt.title("Kernel")
        plt.show()

    return kernel_2D


def _dnorm(x: float, mu: float, sig: float) -> float:
    return np.exp(-np.power((x - mu) / sig, 2) / 2) / (sig * np.sqrt(2 * np.pi))


if __name__ == "__main__":
    gaussian_blur(41, verbose=True)
