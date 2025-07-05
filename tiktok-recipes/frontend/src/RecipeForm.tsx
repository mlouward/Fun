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
import { marked } from "marked";
import { confirmAlert } from "react-confirm-alert";
import "react-confirm-alert/src/react-confirm-alert.css";

interface RecipeFormProps {
    recipe: RecipeData;
    onSave: (updated: RecipeData) => void;
    loading?: boolean;
    fetchWithAuth: (url: string, options?: RequestInit) => Promise<Response>;
    onDelete?: () => void;
}

export interface RecipeData {
    title: string;
    servings: number;
    prep_time: number;
    cook_time: number;
    ingredients: string;
    instructions: string;
    cover_image_idx: number;
    cover_images?: string[];
    tiktok_username?: string;
    tiktok_video_id?: string;
    source_url?: string;
}

const RecipeForm: React.FC<RecipeFormProps> = ({
    recipe,
    onSave,
    loading,
    fetchWithAuth,
    onDelete,
}) => {
    const images = recipe.cover_images || [];
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
            const img = images[form.cover_image_idx];
            if (img && !img.startsWith("data:image")) {
                return `data:image/jpeg;base64,${img}`;
            }
            return img;
        }
        return null;
    };

    const handleExportPaprika = async () => {
        try {
            const resp = await fetchWithAuth(`/api/recipes/export_paprika`, {
                method: "POST",
                headers: { "Content-Type": "application/json" },
                body: JSON.stringify(recipe),
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
                maxWidth: 600,
                mx: "auto",
                mt: 2,
            }}
        >
            <form id="edit-recipe-form" onSubmit={handleSubmit}>
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
                            inputProps={{ min: 1, step: 1 }}
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
                            inputProps={{ min: 0, step: 1 }}
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
                            inputProps={{ min: 0, step: 1 }}
                        />
                    </Grid>
                    <Grid size={{ xs: 24, sm: 12 }}>
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
                    <Grid size={{ xs: 24, sm: 12 }}>
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
                                    src={
                                        img.startsWith("data:image")
                                            ? img
                                            : `data:image/jpeg;base64,${img}`
                                    }
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
