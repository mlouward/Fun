import { useState, useEffect } from "react";
import TiktokUrlForm from "./TiktokUrlForm";
import RecipeForm from "./RecipeForm";
import AuthForm from "./AuthForm";
import MyRecipesPage from "./MyRecipesPage";
import type { RecipeData } from "./RecipeForm";
import "./App.css";

interface ApiRecipeResponse {
    recipe: RecipeData;
    photoSuggestions?: string[];
    cover_images?: string[];
}

function App() {
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState<string | null>(null);
    const [recipe, setRecipe] = useState<RecipeData | null>(null);
    const [photoSuggestions, setPhotoSuggestions] = useState<string[]>([]);
    const [step, setStep] = useState<"url" | "edit" | "done">("url");
    const [token, setToken] = useState<string | null>(null);
    const [authMode, setAuthMode] = useState<"login" | "register">("login");
    const [showAccount, setShowAccount] = useState(false);
    const [recipes, setRecipes] = useState<RecipeData[]>([]);

    const fetchWithAuth = async (url: string, options: any = {}) => {
        options.headers = options.headers || {};
        if (token) options.headers["Authorization"] = `Bearer ${token}`;
        return fetch(url, options);
    };

    const handleProcessUrl = async (url: string) => {
        setLoading(true);
        setError(null);
        setRecipe(null);
        setPhotoSuggestions([]);
        try {
            const resp = await fetchWithAuth("/api/recipes/process_url", {
                method: "POST",
                headers: { "Content-Type": "application/json" },
                body: JSON.stringify({ url }),
            });
            if (!resp.ok)
                throw new Error(
                    (await resp.json()).detail || "Failed to process TikTok URL"
                );
            const data: ApiRecipeResponse = await resp.json();
            setRecipe(data.recipe);
            setPhotoSuggestions(
                data.photoSuggestions || data.cover_images || []
            );
            setStep("edit");
        } catch (e: any) {
            setError(e.message || "Unknown error");
        } finally {
            setLoading(false);
        }
    };

    const handleSaveRecipe = async (updated: RecipeData) => {
        setLoading(true);
        setError(null);
        try {
            const resp = await fetchWithAuth("/api/recipes/update_recipe", {
                method: "POST",
                headers: { "Content-Type": "application/json" },
                body: JSON.stringify(updated),
            });
            if (!resp.ok)
                throw new Error(
                    (await resp.json()).detail || "Failed to save recipe"
                );
            setStep("done");
        } catch (e: any) {
            setError(e.message || "Unknown error");
        } finally {
            setLoading(false);
        }
    };

    // Fetch all recipes for account page
    useEffect(() => {
        if (token && showAccount) {
            fetchWithAuth("/api/recipes/", {})
                .then(async (resp) => {
                    if (!resp.ok) throw new Error("Failed to fetch recipes");
                    const data = await resp.json();
                    setRecipes(data);
                })
                .catch(() => setRecipes([]));
        }
    }, [token, showAccount]);

    // Handler to view a recipe from account sidebar
    const handleViewRecipe = (idx: number) => {
        setRecipe(recipes[idx]);
        setStep("edit");
        setShowAccount(false);
    };

    return (
        <div style={{ display: "flex", minHeight: "100vh" }}>
            {/* Sidebar */}
            {token && (
                <aside
                    style={{
                        width: 220,
                        background: "#fff",
                        borderRight: "1.5px solid var(--border-color)",
                        boxShadow: "2px 0 8px rgba(0,0,0,0.03)",
                        padding: "2rem 1rem 1rem 1rem",
                        display: "flex",
                        flexDirection: "column",
                        alignItems: "flex-start",
                        gap: 24,
                    }}
                >
                    <div
                        style={{
                            fontWeight: 700,
                            color: "var(--primary-color)",
                            fontSize: 22,
                            marginBottom: 32,
                        }}
                    >
                        Account
                    </div>
                    <button
                        style={{ width: "100%", marginBottom: 12 }}
                        onClick={() => {
                            setShowAccount(true);
                            setStep("url");
                        }}
                    >
                        My Recipes
                    </button>
                    <button
                        style={{ width: "100%", marginBottom: 12 }}
                        onClick={() => {
                            setShowAccount(false);
                            setStep("url");
                        }}
                    >
                        New Recipe
                    </button>
                    <button
                        style={{ width: "100%", marginTop: "auto" }}
                        onClick={() => setToken(null)}
                    >
                        Log out
                    </button>
                </aside>
            )}
            {/* Main content */}
            <div className="container" style={{ flex: 1 }}>
                <header>
                    <h1>TikTok Recipe Transcriber</h1>
                    <p>
                        {showAccount
                            ? "Your saved TikTok recipes"
                            : "Paste a TikTok recipe URL to transcribe and edit the recipe!"}
                    </p>
                </header>
                {!token ? (
                    <AuthForm
                        onAuth={setToken}
                        mode={authMode}
                        setMode={setAuthMode}
                        loading={loading}
                    />
                ) : showAccount ? (
                    <MyRecipesPage
                        recipes={recipes}
                        onViewRecipe={handleViewRecipe}
                    />
                ) : (
                    <>
                        {step === "url" && (
                            <TiktokUrlForm
                                onProcess={handleProcessUrl}
                                loading={loading}
                                error={error}
                            />
                        )}
                        {step === "edit" && recipe && (
                            <RecipeForm
                                recipe={recipe}
                                photoSuggestions={photoSuggestions}
                                onSave={handleSaveRecipe}
                                loading={loading}
                                fetchWithAuth={fetchWithAuth}
                            />
                        )}
                        {step === "done" && (
                            <div id="results-container">
                                <h2>Recipe Saved!</h2>
                                <button onClick={() => setStep("url")}>
                                    Transcribe Another
                                </button>
                            </div>
                        )}
                    </>
                )}
                {error && <div id="error-message">{error}</div>}
            </div>
        </div>
    );
}

export default App;
