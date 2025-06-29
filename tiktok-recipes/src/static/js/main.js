document.addEventListener("DOMContentLoaded", () => {
    const form = document.getElementById("recipe-form");
    const urlInput = document.getElementById("tiktok-url");
    const loadingSpinner = document.getElementById("loading-spinner");
    const errorMessage = document.getElementById("error-message");
    const resultsContainer = document.getElementById("results-container");
    const recipeOutput = document.getElementById("recipe-output");
    const copyButton = document.getElementById("copy-button");

    form.addEventListener("submit", async (e) => {
        e.preventDefault();

        // Reset UI
        loadingSpinner.classList.remove("hidden");
        errorMessage.classList.add("hidden");
        resultsContainer.classList.add("hidden");

        const url = urlInput.value;

        try {
            const response = await fetch("/process-url/", {
                method: "POST",
                headers: {
                    "Content-Type": "application/json",
                    Accept: "application/json",
                },
                body: JSON.stringify({ url }),
            });

            if (!response.ok) {
                const errorData = await response.json();
                throw new Error(
                    errorData.detail || "An unknown error occurred."
                );
            }

            const data = await response.json();

            // The 'recipe' key now contains a JSON object directly.
            // We just need to pretty-print it for display.
            recipeOutput.textContent = JSON.stringify(data.recipe, null, 2);

            resultsContainer.classList.remove("hidden");
        } catch (error) {
            errorMessage.textContent = `Error: ${error.message}`;
            errorMessage.classList.remove("hidden");
        } finally {
            loadingSpinner.classList.add("hidden");
        }
    });

    copyButton.addEventListener("click", () => {
        navigator.clipboard
            .writeText(recipeOutput.textContent)
            .then(() => {
                copyButton.textContent = "Copied!";
                setTimeout(() => {
                    copyButton.textContent = "Copy to Clipboard";
                }, 2000);
            })
            .catch((err) => {
                console.error("Failed to copy text: ", err);
            });
    });
});
