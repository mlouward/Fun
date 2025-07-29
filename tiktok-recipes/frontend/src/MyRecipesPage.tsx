import React from "react";
import { Box, Select, MenuItem, Pagination, Typography } from "@mui/material";
import type { RecipeData } from "./RecipeForm";
import "./App.css";

interface MyRecipesPageProps {
    recipes: RecipeData[];
    total: number;
    page: number;
    pageSize: number;
    onPageChange: (page: number) => void;
    onPageSizeChange: (size: number) => void;
    onViewRecipe: (idx: number) => void;
}

const MyRecipesPage: React.FC<MyRecipesPageProps> = ({
    recipes,
    total,
    page,
    pageSize,
    onPageChange,
    onPageSizeChange,
    onViewRecipe,
}) => {
    const pageCount = Math.ceil(total / pageSize);
    return (
        <Box sx={{
            width: '90%',
            maxWidth: { xs: '90vw', sm: 600 }, // Use 90vw for xs, 600px for sm and up
            mx: 'auto',
            p: { xs: 0, md: 2 } // Add some padding for desktop if needed
        }}>
            <Typography
                variant="h5"
                sx={{ color: "var(--primary-color)", mb: 3 }}
            >
                My Recipes
            </Typography>
            {recipes.length === 0 ? (
                <Typography sx={{ color: "var(--light-text-color)" }}>
                    No recipes found.
                </Typography>
            ) : (
                <ul style={{ listStyle: "none", padding: 0, margin: 0 }}>
                    {recipes.map((r, idx) => (
                        <li
                            key={idx}
                            style={{
                                background: "#fafbfc",
                                borderRadius: 8,
                                marginBottom: 12,
                                padding: "0.8em 0.8em",
                                boxShadow: "0 1px 4px rgba(0,0,0,0.04)",
                                display: "flex",
                                alignItems: "center",
                                justifyContent: "flex-start",
                                cursor: "pointer",
                                gap: 12,
                                minHeight: 64,
                            }}
                            onClick={() => onViewRecipe(idx)}
                        >
                            {/* Thumbnail image */}
                            {r.cover_images && r.cover_images.length > 0 ? (
                                (() => {
                                    const selectedIdx =
                                        typeof r.cover_image_idx === "number" &&
                                        r.cover_image_idx >= 0 &&
                                        r.cover_image_idx <
                                            r.cover_images.length
                                            ? r.cover_image_idx
                                            : 0;
                                    const imgSrc = r.cover_images[selectedIdx];
                                    return (
                                        <img
                                            src={
                                                imgSrc.startsWith("data:image")
                                                    ? imgSrc
                                                    : `data:image/jpeg;base64,${imgSrc}`
                                            }
                                            alt="Recipe thumbnail"
                                            style={{
                                                width: 48,
                                                height: 48,
                                                objectFit: "cover",
                                                borderRadius: 8,
                                                marginRight: 12,
                                                border: "1.5px solid var(--border-color)",
                                                background: "#eee",
                                                flexShrink: 0,
                                            }}
                                        />
                                    );
                                })()
                            ) : (
                                <div
                                    style={{
                                        width: 48,
                                        height: 48,
                                        borderRadius: 8,
                                        marginRight: 12,
                                        background: "#f0f0f0",
                                        display: "flex",
                                        alignItems: "center",
                                        justifyContent: "center",
                                        color: "#bbb",
                                        fontSize: 22,
                                        border: "1.5px solid var(--border-color)",
                                        flexShrink: 0,
                                    }}
                                >
                                    🥣
                                </div>
                            )}
                            <div style={{ flex: 1, minWidth: 0 }}>
                                <span
                                    className="recipe-card-title"
                                    style={{
                                        fontWeight: 600,
                                        color: "var(--primary-color)",
                                        fontSize: "1.1rem",
                                    }}
                                >
                                    {r.title || "Untitled Recipe"}
                                </span>
                                <span
                                    style={{
                                        fontSize: 13,
                                        color: "var(--light-text-color)",
                                        display: "block",
                                    }}
                                >
                                    {r.servings ? `${r.servings} servings` : ""}
                                </span>
                            </div>
                        </li>
                    ))}
                </ul>
            )}
            <Box
                sx={{
                    display: "flex",
                    flexDirection: { xs: "column", sm: "row" },
                    alignItems: "center",
                    justifyContent: "space-between",
                    mt: 3,
                    gap: { xs: 2, sm: 0 },
                }}
            >
                <Pagination
                    count={pageCount}
                    page={page}
                    onChange={(_, value) => onPageChange(value)}
                    color="primary"
                />
                <Box
                    sx={{
                        display: "flex",
                        alignItems: "center",
                        gap: 1,
                    }}
                >
                    <Typography variant="body2">Recipes per page:</Typography>
                    <Select
                        value={pageSize}
                        onChange={(e) =>
                            onPageSizeChange(Number(e.target.value))
                        }
                        size="small"
                    >
                        <MenuItem value={10}>10</MenuItem>
                        <MenuItem value={20}>20</MenuItem>
                        <MenuItem value={50}>50</MenuItem>
                    </Select>
                </Box>
            </Box>
        </Box>
    );
};

export default MyRecipesPage;
