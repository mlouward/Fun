import { useState, useCallback } from "react";
import type { RecipeData } from "./RecipeForm";

export function useRecipes(fetchWithAuth: any) {
    const [recipes, setRecipes] = useState<RecipeData[]>([]);
    const [total, setTotal] = useState(0);
    const [loading, setLoading] = useState(false);

    const fetchRecipesPaginated = useCallback(
        async (page: number, pageSize: number) => {
            setLoading(true);
            try {
                const offset = (page - 1) * pageSize;
                const resp = await fetchWithAuth(
                    `/api/recipes/?limit=${pageSize}&offset=${offset}`
                );
                const data = await resp.json();
                setRecipes(Array.isArray(data.recipes) ? data.recipes : []);
                setTotal(typeof data.total === "number" ? data.total : 0);
            } catch {
                setRecipes([]);
                setTotal(0);
            } finally {
                setLoading(false);
            }
        },
        [fetchWithAuth]
    );

    return {
        recipes,
        total,
        loading,
        fetchRecipesPaginated,
        setRecipes,
        setTotal,
        setLoading,
    };
}
