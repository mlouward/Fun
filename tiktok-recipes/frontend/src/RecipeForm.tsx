import React, { useState, useEffect } from "react";
import {
    Box,
    Button,
    TextField,
    Typography,
    Paper,
    Grid,
    Divider,
} from "@mui/material";
import DeleteIcon from "@mui/icons-material/Delete";
import PreviewIcon from "@mui/icons-material/Visibility";
import SaveIcon from "@mui/icons-material/Save";
import KeyboardBackspaceIcon from "@mui/icons-material/KeyboardBackspace";
import { marked } from "marked";
import { confirmAlert } from "react-confirm-alert";
import "react-confirm-alert/src/react-confirm-alert.css";
import { getCoverImageBase64 } from "./App";

interface RecipeFormProps {
    recipe: RecipeData;
    onSave: (updated: RecipeData) => void;
    loading?: boolean;
    fetchWithAuth: (url: string, options?: RequestInit) => Promise<Response>;
    onDelete?: () => void;
    onBack: () => void;
}

export interface RecipeData {
    title: string;
    servings: number;
    prep_time: number;
    cook_time: number;
    ingredients: string;
    instructions: string;
    cover_image_idx: number;
    cover_image_paths?: string[]; // Paths to images (e.g., URLs)
    tiktok_username?: string;
    tiktok_video_id?: string;
    source_url?: string;
    photo_data?: string; // base64 image data for export
}

const RecipeForm: React.FC<RecipeFormProps> = ({
    recipe,
    onSave,
    loading,
    fetchWithAuth,
    onDelete,
    onBack,
}) => {
    const images = recipe.cover_image_paths || [];
    const [form, setForm] = useState<RecipeData>(recipe);
    const [showPreview, setShowPreview] = useState(false);

    useEffect(() => {
        if (
            images.length > 0 &&
            (form.cover_image_idx === undefined ||
                form.cover_image_idx < 0 ||
                form.cover_image_idx >= images.length)
        ) {
            setForm((f) => ({ ...f, cover_image_idx: 0 }));
        }
    }, [images]);

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
        setForm((f) => ({ ...f, cover_image_idx: idx }));
    };

    const handleSubmit = (e: React.FormEvent) => {
        e.preventDefault();
        onSave(form);
    };

    const getSelectedCoverImage = () => {
        if (
            images.length > 0 &&
            form.cover_image_idx >= 0 &&
            form.cover_image_idx < images.length
        ) {
            return `/images/${images[form.cover_image_idx]}`;
        }
        return null;
    };

    const handleExportPaprika = async () => {
        try {
            const photo_data = await getCoverImageBase64(
                images,
                form.cover_image_idx
            );
            const exportData = { ...recipe };
            if (photo_data) exportData.photo_data = photo_data;
            else {
                console.warn("No cover image data available for export");
            }
            const resp = await fetchWithAuth(`/api/recipes/export_paprika`, {
                method: "POST",
                headers: { "Content-Type": "application/json" },
                body: JSON.stringify(exportData),
            });
            if (!resp.ok) throw new Error("Failed to export Paprika file");
            const blob = await resp.blob();
            const url = window.URL.createObjectURL(blob);
            const a = document.createElement("a");
            a.href = url;
            a.download = `${recipe.title || "recipe"}.paprikarecipe`;
            document.body.appendChild(a);
            a.click();
            a.remove();
            window.URL.revokeObjectURL(url);
        } catch {
            alert("Failed to export Paprika file");
        }
    };

    const handleDelete = async () => {
        confirmAlert({
            title: "Delete Recipe",
            message:
                "Are you sure you want to delete this recipe? This cannot be undone.",
            buttons: [
                {
                    label: "Yes, Delete",
                    onClick: async () => {
                        try {
                            const resp = await fetchWithAuth(
                                "/api/recipes/delete_recipe",
                                {
                                    method: "DELETE",
                                    headers: {
                                        "Content-Type": "application/json",
                                    },
                                    body: JSON.stringify({
                                        tiktok_username: recipe.tiktok_username,
                                        tiktok_video_id: recipe.tiktok_video_id,
                                        title: recipe.title,
                                    }),
                                }
                            );
                            if (!resp.ok)
                                throw new Error("Failed to delete recipe");
                            if (onDelete) onDelete();
                        } catch (e) {
                            if (e instanceof Error) {
                                alert(e.message || "Failed to delete recipe");
                            } else {
                                alert("Unknown error");
                            }
                        }
                    },
                },
                {
                    label: "Cancel",
                    onClick: () => {},
                },
            ],
        });
    };

    return (
        <Paper
            elevation={3}
            sx={{
                p: { xs: 2, md: 4 },
                width: "90%", // Take 90% width on all screens by default
                maxWidth: 600, // Max width for larger screens
                mx: "auto",
                mt: { xs: 2, md: 2 },
            }}
        >
            <form id="edit-recipe-form" onSubmit={handleSubmit}>
                <Box
                    sx={{
                        display: "flex",
                        justifyContent: "flex-start",
                        mb: 2,
                    }}
                >
                    <Button
                        variant="outlined"
                        startIcon={<KeyboardBackspaceIcon />}
                        onClick={onBack}
                    >
                        Back to My Recipes
                    </Button>
                </Box>
                {recipe && recipe.source_url && (
                    <Typography
                        variant="body2"
                        sx={{
                            color: "var(--light-text-color)",
                            mb: 2,
                        }}
                    >
                        TikTok Link:{" "}
                        <a
                            href={recipe.source_url}
                            target="_blank"
                            rel="noopener noreferrer"
                            style={{ color: "var(--primary-color)" }}
                        >
                            {recipe.source_url}
                        </a>
                    </Typography>
                )}
                <Typography
                    variant="h5"
                    sx={{
                        color: "var(--primary-color)",
                        mb: 2,
                    }}
                >
                    {form.title || "Untitled Recipe"}
                </Typography>
                {images.length > 0 && getSelectedCoverImage() && (
                    <Box sx={{ mb: 2, textAlign: "center" }}>
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
                    </Box>
                )}
                <Grid container spacing={3} alignItems="stretch">
                    <Grid size={{ xs: 12, sm: 6 }}>
                        <TextField
                            label="Title"
                            name="title"
                            value={form.title}
                            onChange={handleChange}
                            fullWidth
                            required
                        />
                    </Grid>
                    <Grid size={{ xs: 12, sm: 6 }}>
                        <TextField
                            label="Servings"
                            name="servings"
                            type="number"
                            value={form.servings}
                            onChange={handleChange}
                            fullWidth
                            required
                            slotProps={{ htmlInput: { min: 1, step: 1 } }}
                        />
                    </Grid>
                    <Grid size={{ xs: 12, sm: 6 }}>
                        <TextField
                            label="Prep Time (min)"
                            name="prep_time"
                            type="number"
                            value={form.prep_time}
                            onChange={handleChange}
                            fullWidth
                            required
                            slotProps={{ htmlInput: { min: 0, step: 1 } }}
                        />
                    </Grid>
                    <Grid size={{ xs: 12, sm: 6 }}>
                        <TextField
                            label="Cook Time (min)"
                            name="cook_time"
                            type="number"
                            value={form.cook_time}
                            onChange={handleChange}
                            fullWidth
                            required
                            slotProps={{ htmlInput: { min: 0, step: 1 } }}
                        />
                    </Grid>
                    <Grid size={{ xs: 12, sm: 12 }}>
                        <TextField
                            label="Ingredients"
                            name="ingredients"
                            value={form.ingredients}
                            onChange={handleChange}
                            fullWidth
                            required
                            multiline
                            rows={6}
                        />
                    </Grid>
                    <Grid size={{ xs: 12, sm: 12 }}>
                        <TextField
                            label="Instructions"
                            name="instructions"
                            value={form.instructions}
                            onChange={handleChange}
                            fullWidth
                            required
                            multiline
                            rows={12}
                        />
                    </Grid>
                </Grid>
                <Box
                    sx={{
                        display: "flex",
                        flexDirection: { xs: "column", sm: "row" },
                        gap: 2,
                        mt: 3,
                        width: "100%",
                    }}
                >
                    <Button
                        variant="outlined"
                        startIcon={<PreviewIcon />}
                        onClick={() => setShowPreview((p) => !p)}
                        fullWidth
                        sx={{ flex: 1 }}
                    >
                        {showPreview ? "Hide" : "Show"} Preview
                    </Button>
                    <Button
                        variant="outlined"
                        onClick={handleExportPaprika}
                        fullWidth
                        sx={{ flex: 1, gap: 1 }}
                    >
                        <span className="pepper-icon">🌶️</span>
                        Export to Paprika
                    </Button>
                    <Button
                        id="save-recipe-button"
                        type="submit"
                        variant="contained"
                        color="primary"
                        startIcon={<SaveIcon />}
                        disabled={loading}
                        fullWidth
                        sx={{ flex: 1 }}
                    >
                        {loading ? "Saving..." : "Save Recipe"}
                    </Button>
                </Box>
                {showPreview && (
                    <Paper
                        elevation={1}
                        sx={{
                            mt: 3,
                            p: 2,
                            bgcolor: "#f6f8fa",
                        }}
                    >
                        <Typography
                            variant="subtitle1"
                            sx={{
                                color: "var(--primary-color)",
                                mb: 1,
                            }}
                        >
                            Preview
                        </Typography>
                        <Divider sx={{ mb: 1 }} />
                        <div
                            dangerouslySetInnerHTML={{
                                __html: marked.parse(form.instructions || ""),
                            }}
                        />
                    </Paper>
                )}
                {images.length > 0 && (
                    <Box sx={{ mt: 3 }}>
                        <Typography
                            className="photo-suggestions-label"
                            sx={{ mb: 1 }}
                        >
                            Select a cover photo:
                        </Typography>
                        <Box
                            sx={{
                                display: "flex",
                                gap: 2,
                                flexWrap: "wrap",
                                justifyContent: "center",
                            }}
                        >
                            {images.map((img, idx) => (
                                <img
                                    key={idx}
                                    src={`/images/${img}`}
                                    alt={`Cover suggestion ${idx + 1}`}
                                    style={{
                                        width: 90,
                                        height: 90,
                                        objectFit: "cover",
                                        borderRadius: 8,
                                        border:
                                            form.cover_image_idx === idx
                                                ? "2.5px solid var(--primary-color)"
                                                : "2px solid var(--border-color)",
                                        cursor: "pointer",
                                        boxShadow:
                                            form.cover_image_idx === idx
                                                ? "0 0 0 2px var(--primary-color)"
                                                : undefined,
                                    }}
                                    onClick={() => handlePhotoSelect(idx)}
                                />
                            ))}
                        </Box>
                    </Box>
                )}
                <Button
                    variant="contained"
                    color="error"
                    startIcon={<DeleteIcon />}
                    onClick={handleDelete}
                    fullWidth
                    sx={{ mt: 3 }}
                >
                    Delete Recipe
                </Button>
            </form>
        </Paper>
    );
};

export default RecipeForm;
