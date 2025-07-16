import { useState, useEffect } from "react";
import {
    Box,
    Button,
    Container,
    Typography,
    useMediaQuery,
    useTheme,
} from "@mui/material";
import AuthForm from "./AuthForm";
import MyRecipesPage from "./MyRecipesPage";
import RecipeForm from "./RecipeForm";
import TiktokUrlForm from "./TiktokUrlForm";
import Sidebar from "./Sidebar";
import { useRecipes } from "./useRecipes";
import type { RecipeData } from "./RecipeForm";
import "./App.css";
import Snackbar from "@mui/material/Snackbar";
import MuiAlert, { type AlertColor } from "@mui/material/Alert";

function App() {
    const [error, setError] = useState<string | null>(null);
    const [recipe, setRecipe] = useState<RecipeData | null>(null);
    const [step, setStep] = useState<"url" | "edit" | "done">("url");
    const decodeJwt = (token: string) => {
        try {
            const base64Url = token.split(".")[1];
            const base64 = base64Url.replace(/-/g, "+").replace(/_/g, "/");
            const jsonPayload = decodeURIComponent(
                atob(base64)
                    .split("")
                    .map(function (c) {
                        return (
                            "%" +
                            ("00" + c.charCodeAt(0).toString(16)).slice(-2)
                        );
                    })
                    .join("")
            );
            return JSON.parse(jsonPayload);
        } catch (e) {
            console.error("Failed to decode JWT:", e);
            return null;
        }
    };

    const [token, setToken] = useState<string | null>(() => {
        const storedToken = localStorage.getItem("authToken");
        if (storedToken) {
            const decoded = decodeJwt(storedToken);
            if (decoded && decoded.exp * 1000 > Date.now()) {
                return storedToken;
            } else {
                localStorage.removeItem("authToken");
                return null;
            }
        }
        return null;
    });

    useEffect(() => {
        let logoutTimer: NodeJS.Timeout;

        if (token) {
            localStorage.setItem("authToken", token);
            const decoded = decodeJwt(token);
            if (decoded && decoded.exp) {
                const expirationTime = decoded.exp * 1000; // Convert to milliseconds
                const timeUntilExpiration = expirationTime - Date.now();

                // Log out 5 seconds before actual expiration
                if (timeUntilExpiration > 5000) {
                    logoutTimer = setTimeout(() => {
                        setToken(null);
                        setToastMsg(
                            "Your session has expired. Please log in again."
                        );
                        setToastSeverity("info");
                        setToastOpen(true);
                    }, timeUntilExpiration - 5000);
                } else {
                    // If already expired or very close to expiration, log out immediately
                    setToken(null);
                    setToastMsg(
                        "Your session has expired. Please log in again."
                    );
                    setToastSeverity("info");
                    setToastOpen(true);
                }
            }
        } else {
            localStorage.removeItem("authToken");
        }

        return () => {
            if (logoutTimer) {
                clearTimeout(logoutTimer);
            }
        };
    }, [token]);
    const [authMode, setAuthMode] = useState<"login" | "register">("login");
    const [showAccount, setShowAccount] = useState(false);
    const [page, setPage] = useState(1);
    const [pageSize, setPageSize] = useState(10);
    const [sidebarOpen, setSidebarOpen] = useState(false);
    const [toastOpen, setToastOpen] = useState(false);
    const [toastMsg, setToastMsg] = useState("");
    const [toastSeverity, setToastSeverity] = useState<AlertColor>("info");
    const theme = useTheme();
    const isDesktop = useMediaQuery(theme.breakpoints.up("md"));

    const fetchWithAuth = async (url: string, options: RequestInit = {}) => {
        // Always use VITE_API_URL if it's set, otherwise use a default
        const baseUrl = import.meta.env.VITE_API_URL || "";
        // Remove any trailing slashes from the base URL and leading slashes from the path
        const cleanBase = baseUrl.replace(/\/+$/, "");
        const cleanPath = url.replace(/^\/+/, "");
        const fullUrl = cleanBase
            ? `${cleanBase}/${cleanPath}`
            : `/${cleanPath}`;

        console.log("API URL:", import.meta.env.VITE_API_URL);
        console.log("Full URL:", fullUrl);

        options.headers = {
            "Content-Type": "application/json",
            Accept: "application/json",
            ...(options.headers || {}),
            ...(token ? { Authorization: `Bearer ${token}` } : {}),
        };

        return fetch(fullUrl, options);
    };
    const { recipes, total, loading, fetchRecipesPaginated, setLoading } =
        useRecipes(fetchWithAuth);

    const handleProcessUrl = async (url: string) => {
        setLoading(true);
        setError(null);
        setRecipe(null);
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
            const data = await resp.json();
            // Instead of showing the recipe, show a toast and go to My Recipes
            setToastMsg(
                data.message ||
                    "Recipe is being processed. You will be notified when it is ready."
            );
            setToastSeverity("info");
            setToastOpen(true);
            setShowAccount(true);
            setStep("url");
        } catch (e) {
            if (e instanceof Error) {
                setError(e.message || "Unknown error");
                setToastMsg(e.message || "Unknown error");
            } else {
                setError("Unknown error");
                setToastMsg("Unknown error");
            }
            setToastSeverity("error");
            setToastOpen(true);
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
            await fetchRecipesPaginated(page, pageSize);
            setStep("done");
        } catch (e) {
            if (e instanceof Error) {
                setError(e.message || "Unknown error");
            } else {
                setError("Unknown error");
            }
        } finally {
            setLoading(false);
        }
    };

    const handleDeleteRecipe = () => {
        setStep("url");
        setShowAccount(true);
        fetchRecipesPaginated(page, pageSize);
    };

    useEffect(() => {
        if (token && showAccount) {
            fetchRecipesPaginated(page, pageSize);
        }
    }, [token, showAccount, page, pageSize]);

    // Handler to view a recipe from account sidebar
    const handleViewRecipe = (idx: number) => {
        setRecipe(recipes[idx]);
        setStep("edit");
        setShowAccount(false);
        setSidebarOpen(false);
    };

    return (
        <Box
            sx={{
                display: "flex",
                minHeight: "100vh",
                bgcolor: "var(--background-color)",
            }}
        >
            {token && (
                <Sidebar
                    open={sidebarOpen}
                    isDesktop={isDesktop}
                    showAccount={showAccount}
                    onHome={() => {
                        setShowAccount(false);
                        setStep("url");
                        setSidebarOpen(false);
                    }}
                    onMyRecipes={() => {
                        setShowAccount(true);
                        setStep("url");
                        setSidebarOpen(false);
                    }}
                    onClose={() => setSidebarOpen(false)}
                />
            )}
            <Snackbar
                open={toastOpen}
                autoHideDuration={5000}
                onClose={() => setToastOpen(false)}
                anchorOrigin={{ vertical: "top", horizontal: "center" }}
            >
                <MuiAlert
                    onClose={() => setToastOpen(false)}
                    severity={toastSeverity}
                    elevation={6}
                    variant="filled"
                    sx={{ width: "100%" }}
                >
                    {toastMsg}
                </MuiAlert>
            </Snackbar>
            {/* Main content */}
            <Box
                component="main"
                sx={{ flex: 1, minWidth: 0, p: { xs: 2, md: 5 }, mt: 8 }}
            >
                <Container maxWidth="md">
                    <Box sx={{ mb: 4 }}>
                        <Typography
                            variant="h4"
                            sx={{ color: "var(--primary-color)", mb: 1 }}
                        >
                            TikTok Recipe Transcriber
                        </Typography>
                        <Typography
                            variant="subtitle1"
                            sx={{ color: "var(--light-text-color)" }}
                        >
                            {token && showAccount
                                ? "Your saved TikTok recipes"
                                : "Paste a TikTok recipe URL to transcribe and edit the recipe!"}
                        </Typography>
                    </Box>
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
                            total={total}
                            page={page}
                            pageSize={pageSize}
                            onPageChange={setPage}
                            onPageSizeChange={(size) => {
                                setPageSize(size);
                                setPage(1);
                            }}
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
                                    onSave={handleSaveRecipe}
                                    loading={loading}
                                    fetchWithAuth={fetchWithAuth}
                                    onDelete={handleDeleteRecipe}
                                />
                            )}
                            {step === "done" && (
                                <Box sx={{ textAlign: "center", mt: 6 }}>
                                    <Typography
                                        variant="h5"
                                        sx={{ color: "var(--primary-color)" }}
                                    >
                                        Recipe saved!
                                    </Typography>
                                    <Button
                                        sx={{ mt: 3 }}
                                        variant="contained"
                                        color="primary"
                                        onClick={() => {
                                            setShowAccount(true);
                                            setStep("url");
                                        }}
                                    >
                                        Back to My Recipes
                                    </Button>
                                </Box>
                            )}
                        </>
                    )}
                </Container>
            </Box>
        </Box>
    );
}

export default App;
