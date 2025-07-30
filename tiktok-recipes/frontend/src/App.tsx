import { useState, useEffect } from "react";
import {
    Box,
    Button,
    Container,
    Typography,
    useMediaQuery,
    useTheme,
    IconButton,
    Toolbar,
    CircularProgress,
} from "@mui/material";
import { createTheme, ThemeProvider } from "@mui/material/styles";
import MenuIcon from "@mui/icons-material/Menu";
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
        setLoadingApp(false); // Set loading to false after auth check

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
    const [loadingApp, setLoadingApp] = useState(true); // New state for app loading
    const theme = useTheme();
    const isDesktop = useMediaQuery(theme.breakpoints.up("md"));

    const tiktokTheme = createTheme({
        palette: {
            primary: {
                main: "#ff3b5c", // Your TikTok primary color
            },
        },
        components: {
            MuiButton: {
                styleOverrides: {
                    root: {
                        fontWeight: 700,
                        textTransform: "none",
                        borderRadius: "8px",
                        border: "2px solid #ff3b5c",
                        "&:hover": {
                            transform: "translateY(-2px)",
                            boxShadow: "0 4px 8px rgba(0, 0, 0, 0.1)",
                        },
                    },
                    containedPrimary: {
                        color: "#fff",
                        backgroundColor: "#ff3b5c",
                        "&:hover": {
                            backgroundColor: "#e03050",
                        },
                    },
                    outlinedPrimary: {
                        color: "#ff3b5c",
                        borderColor: "#ff3b5c",
                        "&:hover": {
                            backgroundColor: "rgba(255, 59, 92, 0.04)",
                        },
                    },
                    textPrimary: {
                        color: "#ff3b5c",
                        "&:hover": {
                            backgroundColor: "rgba(255, 59, 92, 0.04)",
                        },
                    },
                },
            },
        },
    });

    const handleDrawerToggle = () => {
        setSidebarOpen(!sidebarOpen);
    };

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
        <ThemeProvider theme={tiktokTheme}>
            <Box
                sx={{
                    display: "flex",
                    minHeight: "100vh",
                    bgcolor: "var(--background-color)",
                }}
            >
                {loadingApp ? (
                    <Box
                        sx={{
                            display: "flex",
                            justifyContent: "center",
                            alignItems: "center",
                            height: "100vh",
                            width: "100vw",
                            position: "fixed",
                            top: 0,
                            left: 0,
                            bgcolor: "rgba(255, 255, 255, 0.8)",
                            zIndex: 9999,
                        }}
                    >
                        <CircularProgress color="primary" />
                    </Box>
                ) : (
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
                    sx={{
                        flexGrow: 1,
                        p: { xs: 1, sm: 3 }, // Reduced padding for extra-small screens
                        width: { sm: `calc(100% - ${isDesktop ? 260 : 0}px)` },
                        mt: { xs: "64px", md: 0 },
                    }}
                >
                    <Toolbar
                        sx={{
                            display: { xs: "flex", md: "none" },
                            justifyContent: "space-between",
                            alignItems: "center",
                            position: "fixed",
                            top: 0,
                            left: 0,
                            right: 0,
                            height: 64,
                            bgcolor: "var(--background-color)",
                            zIndex: 1200,
                            boxShadow: "0 2px 4px rgba(0,0,0,0.1)",
                            p: 2,
                        }}
                    >
                        {!isDesktop && token && (
                            <IconButton
                                color="inherit"
                                aria-label="open drawer"
                                edge="start"
                                onClick={handleDrawerToggle}
                                sx={{ color: "var(--primary-color)" }}
                            >
                                <MenuIcon />
                            </IconButton>
                        )}
                        <Typography
                            variant="h6"
                            noWrap
                            sx={{ color: "var(--primary-color)" }}
                        >
                            TikTok Recipes
                        </Typography>
                    </Toolbar>
                    <Container
                        maxWidth={isDesktop ? "md" : false}
                        sx={{ pt: { xs: "64px", md: 0 } }}
                    >
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
                                        onBack={() => {
                                            setShowAccount(true);
                                            setStep("url");
                                        }}
                                    />
                                )}
                                {step === "done" && (
                                    <Box sx={{ textAlign: "center", mt: 6 }}>
                                        <Typography
                                            variant="h5"
                                            sx={{
                                                color: "var(--primary-color)",
                                            }}
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
        </ThemeProvider>
    );
}

export default App;
