document.addEventListener("DOMContentLoaded", () => {
    const form = document.getElementById("recipe-form");
    const urlInput = document.getElementById("tiktok-url");
    const loadingSpinner = document.getElementById("loading-spinner");
    const errorMessage = document.getElementById("error-message");
    const resultsContainer = document.getElementById("results-container");
    const recipeOutput = document.getElementById("recipe-output");
    const copyButton = document.getElementById("copy-button");
    const exportPaprikaFileButton = document.getElementById(
        "export-paprika-file-button"
    );

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

    exportPaprikaFileButton.addEventListener("click", async () => {
        try {
            const recipeJson = recipeOutput.textContent;
            if (!recipeJson) {
                alert("No recipe to export!");
                return;
            }
            const response = await fetch("/export-paprika-file/", {
                method: "POST",
                headers: {
                    "Content-Type": "application/json",
                    Accept: "application/octet-stream",
                },
                body: recipeJson,
            });
            if (!response.ok) {
                throw new Error("Failed to export Paprika file.");
            }
            // Try to extract filename from Content-Disposition header
            let filename = "recipe";
            const disposition = response.headers.get("Content-Disposition");
            if (disposition) {
                const match = disposition.match(/filename=([^;]+)/);
                if (match && match[1]) {
                    filename = match[1].replace(/\"/g, "").trim();
                }
            }
            const blob = await response.blob();
            const url = window.URL.createObjectURL(blob);
            const a = document.createElement("a");
            console.log("Exporting to Paprika file:", filename);
            a.href = url;
            a.download = filename;
            document.body.appendChild(a);
            a.click();
            a.remove();
            window.URL.revokeObjectURL(url);
        } catch (err) {
            alert("Export failed: " + err.message);
        }
    });
});
