import React, { useState, useEffect } from "react";
import "./App.css"; // Import global styles
import { marked } from "marked";

interface RecipeFormProps {
    recipe: RecipeData;
    photoSuggestions: string[]; // base64 image strings
    onSave: (updated: RecipeData) => void;
    loading?: boolean;
    fetchWithAuth: (url: string, options?: any) => Promise<Response>;
}

export interface RecipeData {
    title: string;
    servings: number;
    prep_time: number;
    cook_time: number;
    ingredients: string;
    instructions: string;
    selectedPhotoIdx: number;
}

const RecipeForm: React.FC<RecipeFormProps> = ({
    recipe,
    photoSuggestions,
    onSave,
    loading,
    fetchWithAuth,
}) => {
    const [form, setForm] = useState<RecipeData>(recipe);
    const [showPreview, setShowPreview] = useState(false);

    // Ensure selectedPhotoIdx is always valid
    useEffect(() => {
        if (
            photoSuggestions.length > 0 &&
            (form.selectedPhotoIdx === undefined ||
                form.selectedPhotoIdx < 0 ||
                form.selectedPhotoIdx >= photoSuggestions.length)
        ) {
            setForm((f) => ({ ...f, selectedPhotoIdx: 0 }));
        }
    }, [photoSuggestions]);

    const handleChange = (
        e: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement>
    ) => {
        const { name, value, type } = e.target;
        setForm((f) => ({
            ...f,
            [name]: type === "number" ? parseInt(value) || 0 : value,
        }));
    };

    const handlePhotoSelect = (idx: number) => {
        setForm((f) => ({ ...f, selectedPhotoIdx: idx }));
    };

    const handleSubmit = (e: React.FormEvent) => {
        e.preventDefault();
        onSave(form);
    };

    // Helper to get the selected cover image as a data URL
    const getSelectedCoverImage = () => {
        if (
            photoSuggestions.length > 0 &&
            form.selectedPhotoIdx >= 0 &&
            form.selectedPhotoIdx < photoSuggestions.length
        ) {
            const img = photoSuggestions[form.selectedPhotoIdx];
            // If not already a data URL, add prefix
            if (img && !img.startsWith("data:image")) {
                return `data:image/jpeg;base64,${img}`;
            }
            return img;
        }
        return null;
    };

    // Export to Paprika handler
    const handleExportPaprika = async () => {
        try {
            // Call backend endpoint to get gzipped .paprikarecipe file
            const resp = await fetchWithAuth(`/api/recipes/export_paprika`, {
                method: "POST",
                headers: { "Content-Type": "application/json" },
                body: JSON.stringify(recipe),
            });
            if (!resp.ok) throw new Error("Failed to export Paprika file");
            const blob = await resp.blob();
            // Download as .paprikarecipe file
            const url = window.URL.createObjectURL(blob);
            const a = document.createElement("a");
            a.href = url;
            a.download = `${recipe.title || "recipe"}.paprikarecipe`;
            document.body.appendChild(a);
            a.click();
            a.remove();
            window.URL.revokeObjectURL(url);
        } catch (e) {
            alert("Failed to export Paprika file");
        }
    };

    return (
        <form id="edit-recipe-form" onSubmit={handleSubmit}>
            {/* Show TikTok source URL if available */}
            {recipe && (recipe as any).source_url && (
                <div style={{ marginBottom: "1em" }}>
                    <span
                        style={{
                            color: "var(--light-text-color)",
                            fontSize: "0.95em",
                        }}
                    >
                        TikTok Link:{" "}
                        <a
                            href={(recipe as any).source_url}
                            target="_blank"
                            rel="noopener noreferrer"
                        >
                            {(recipe as any).source_url}
                        </a>
                    </span>
                </div>
            )}
            {/* Show recipe title at the top */}
            <h2
                style={{
                    marginBottom: "1em",
                    color: "var(--primary-color)",
                }}
            >
                {form.title || "Untitled Recipe"}
            </h2>
            {/* Show selected cover image preview */}
            {photoSuggestions.length > 0 && getSelectedCoverImage() && (
                <div style={{ marginBottom: "1em" }}>
                    <img
                        src={getSelectedCoverImage()!}
                        alt="Selected cover"
                        style={{
                            width: 180,
                            height: 180,
                            objectFit: "cover",
                            borderRadius: 12,
                            border: "2px solid var(--primary-color)",
                            boxShadow: "0 2px 8px rgba(0,0,0,0.08)",
                        }}
                    />
                </div>
            )}
            <label htmlFor="title">Title</label>
            <input
                type="text"
                id="title"
                name="title"
                value={form.title}
                onChange={handleChange}
                required
            />
            <label htmlFor="servings">Servings</label>
            <input
                type="number"
                id="servings"
                name="servings"
                value={form.servings}
                min={1}
                step={1}
                onChange={handleChange}
                required
            />
            <label htmlFor="prep_time">Prep Time (min)</label>
            <input
                type="number"
                id="prep_time"
                name="prep_time"
                value={form.prep_time}
                min={0}
                step={1}
                onChange={handleChange}
                required
            />
            <label htmlFor="cook_time">Cook Time (min)</label>
            <input
                type="number"
                id="cook_time"
                name="cook_time"
                value={form.cook_time}
                min={0}
                step={1}
                onChange={handleChange}
                required
            />
            <label htmlFor="ingredients">Ingredients</label>
            <textarea
                id="ingredients"
                name="ingredients"
                value={form.ingredients}
                onChange={handleChange}
                rows={4}
                required
            />
            <label htmlFor="instructions">Instructions</label>
            <textarea
                id="instructions"
                name="instructions"
                value={form.instructions}
                onChange={handleChange}
                rows={6}
                required
            />
            <div className="recipe-actions">
                <button type="button" onClick={() => setShowPreview((p) => !p)}>
                    {showPreview ? "Hide" : "Show"} Markdown Preview
                </button>
                <button type="button" onClick={handleExportPaprika}>
                    Export to Paprika
                </button>
                <button
                    id="save-recipe-button"
                    type="submit"
                    disabled={loading}
                >
                    {loading ? "Saving..." : "Save Recipe"}
                </button>
            </div>
            {showPreview && (
                <div className="markdown-preview">
                    <div className="markdown-preview-title">Preview</div>
                    <div
                        dangerouslySetInnerHTML={{
                            __html: marked.parse(form.instructions || ""),
                        }}
                    />
                </div>
            )}
            {photoSuggestions.length > 0 && (
                <div>
                    <span className="photo-suggestions-label">
                        Select a cover photo:
                    </span>
                    <div className="photo-suggestions">
                        {photoSuggestions.map((img, idx) => (
                            <img
                                key={idx}
                                src={
                                    img.startsWith("data:image")
                                        ? img
                                        : `data:image/jpeg;base64,${img}`
                                }
                                alt={`Cover suggestion ${idx + 1}`}
                                className={
                                    form.selectedPhotoIdx === idx
                                        ? "selected"
                                        : ""
                                }
                                onClick={() => handlePhotoSelect(idx)}
                            />
                        ))}
                    </div>
                </div>
            )}
        </form>
    );
};

export default RecipeForm;
