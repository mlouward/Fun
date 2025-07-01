import React from "react";
import type { RecipeData } from "./RecipeForm";
import "./App.css";

interface MyRecipesPageProps {
    recipes: RecipeData[];
    onViewRecipe: (idx: number) => void;
}

const MyRecipesPage: React.FC<MyRecipesPageProps> = ({
    recipes,
    onViewRecipe,
}) => (
    <div>
        <h2
            style={{
                color: "var(--primary-color)",
                marginBottom: 24,
            }}
        >
            My Recipes
        </h2>
        {recipes.length === 0 ? (
            <div style={{ color: "var(--light-text-color)" }}>
                No recipes found.
            </div>
        ) : (
            <ul style={{ listStyle: "none", padding: 0 }}>
                {recipes.map((r, idx) => (
                    <li
                        key={idx}
                        style={{
                            background: "#fafbfc",
                            borderRadius: 8,
                            marginBottom: 12,
                            padding: "1em 1.2em",
                            boxShadow: "0 1px 4px rgba(0,0,0,0.04)",
                            display: "flex",
                            alignItems: "center",
                            justifyContent: "space-between",
                            cursor: "pointer",
                        }}
                        onClick={() => onViewRecipe(idx)}
                    >
                        <span
                            style={{
                                fontWeight: 600,
                                color: "var(--primary-color)",
                            }}
                        >
                            {r.name || r.title || "Untitled Recipe"}
                        </span>
                        <span
                            style={{
                                fontSize: 13,
                                color: "var(--light-text-color)",
                            }}
                        >
                            {r.servings ? `${r.servings} servings` : ""}
                        </span>
                    </li>
                ))}
            </ul>
        )}
    </div>
);

export default MyRecipesPage;
