import sys
import time

from PIL import Image
import numpy as np
import matplotlib.pyplot as plt
from numba import jit

from image_process import create_image, gaussian_blur


@jit(nopython=True)
def gaussian_filter(matrix, kernel):
    """
    Input matrix is a 2D array
    """
    s = kernel.shape[0]
    h = matrix.shape[0]
    w = matrix.shape[1]
    res = np.zeros((h - s + 1, w - s + 1))

    for i in range(h - s + 1):
        for j in range(w - s + 1):
            res[i, j] = np.sum(np.multiply(kernel, matrix[i:i + s,j:j + s]))

    return res

@jit(nopython=True)
def sobel_filter(matrix):
    """
    Input matrix is a 2D array
    """
    h = matrix.shape[0]
    w = matrix.shape[1]

    gx = np.array([[1, 0, -1],
                  [2, 0, -2],
                  [1, 0, -1]])
    gy = np.array([[1, 2, 1],
                  [0, 0, 0],
                  [-1, -2, -1]])
    res = np.zeros((h - 2, w - 2))

    for i in range(h - 2):
        for j in range(w - 2):
            S1 = np.sum(np.multiply(gx, matrix[i:i+3, j:j+3]))
            S2 = np.sum(np.multiply(gy, matrix[i:i+3, j:j+3]))

            res[i, j] = np.sqrt(S1**2 + S2**2)

    return res

@jit(nopython=True)
def prewitt_filter(matrix):
    """
    Input matrix is a 2D array
    """
    h = matrix.shape[0]
    w = matrix.shape[1]

    gx = np.array([[1, 0, -1],
                  [1, 0, -1],
                  [1, 0, -1]])
    gy = np.array([[1, 1, 1],
                  [0, 0, 0],
                  [-1, -1, -1]])
    res = np.zeros((h - 2, w - 2))

    for i in range(h - 2):
        for j in range(w - 2):
            S1 = np.sum(np.multiply(gx, matrix[i:i+3, j:j+3]))
            S2 = np.sum(np.multiply(gy, matrix[i:i+3, j:j+3]))

            res[i, j] = np.sqrt(S1**2 + S2**2)
    return res

def show_images(images, cols = 2, titles = None):
    """Display a list of images in a single figure with matplotlib.
    @Params:
    images: List of np.arrays compatible with plt.imshow.
    """
    assert((titles is None)or (len(images) == len(titles)))
    n_images = len(images)
    if titles is None: titles = ['Image (%d)' % i for i in range(1,n_images + 1)]
    fig = plt.figure()
    for n, (image, title) in enumerate(zip(images, titles)):
        a = fig.add_subplot(cols, np.ceil(n_images/float(cols)), n + 1)
        plt.imshow(image, cmap="gray")
        a.set_title(title)
    fig.set_size_inches(np.array(fig.get_size_inches()) * n_images)
    plt.show()


if __name__ == "__main__":
    THRESHOLD = int(sys.argv[2])
    size = 11

    s = time.time()
    base_image_bw, matrix, image = create_image(sys.argv[1])
    print("Conversion time:", time.time() - s)
    # base_image_bw.show()

    kernel = gaussian_blur(size)
    s = time.time()
    filtered_mat = gaussian_filter(matrix, kernel)
    print("Gaussian time:", time.time() - s)
    gauss = Image.fromarray(filtered_mat)
    # gauss.show()

    s = time.time()
    new_mat = sobel_filter(matrix)
    new_mat = np.maximum(new_mat, THRESHOLD)
    new_mat[new_mat <= THRESHOLD] = 0
    print("Sobel time:", time.time() - s)
    sobel = Image.fromarray(new_mat)
    # sobel.show(title="Before blur")

    s = time.time()
    new_mat = sobel_filter(filtered_mat)
    new_mat = np.maximum(new_mat, THRESHOLD)
    new_mat[new_mat <= THRESHOLD] = 0
    print("Sobel2 time:", time.time() - s)
    gauss_sobel = Image.fromarray(new_mat)
    # gauss_sobel.show(title=f"After blur ({size})")

    to_show = [image, gauss, sobel, gauss_sobel]
    show_images(to_show, 2,
                ["Base", f"Gaussian blur ({size})",
                 "Sobel on base", f"Sobel on {size}-Gaussian"])

# python .\sharpen.py .\Saturn.jpg 0