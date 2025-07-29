import React, { useState } from "react";
import { Button, Typography, Box } from "@mui/material";

interface AuthFormProps {
    onAuth: (token: string) => void;
    mode: "login" | "register";
    setMode: (mode: "login" | "register") => void;
    loading?: boolean;
}

const AuthForm: React.FC<AuthFormProps> = ({
    onAuth,
    mode,
    setMode,
    loading,
}) => {
    const [username, setUsername] = useState("");
    const [password, setPassword] = useState("");
    const [error, setError] = useState<string | null>(null);

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        setError(null);
        try {
            const endpoint =
                mode === "login" ? "api/auth/login" : "api/auth/register";
            // Get the base URL from environment variables
            const baseUrl = import.meta.env.VITE_API_URL || "";
            const cleanBase = baseUrl.replace(/\/+$/, "");
            const cleanPath = endpoint.replace(/^\/+/, "");
            const fullUrl = cleanBase
                ? `${cleanBase}/${cleanPath}`
                : `/${cleanPath}`;

            console.log("API URL:", import.meta.env.VITE_API_URL);
            console.log("Full URL:", fullUrl);

            const resp = await fetch(fullUrl, {
                method: "POST",
                headers: { "Content-Type": "application/json" },
                body: JSON.stringify({ username, password }),
            });
            const data = await resp.json();
            if (!resp.ok)
                throw new Error(data.detail || data.message || "Auth failed");
            if (mode === "register") {
                setMode("login");
                setError("Registration successful! Please log in.");
                return;
            }
            onAuth(data.access_token);
        } catch (e) {
            console.error("Fetch error:", e); // Log the full error object
            if (e instanceof Error) {
                setError(e.message || "Auth failed");
            } else {
                setError("Unknown error");
            }
        }
    };

    return (
        <form className="auth-form" onSubmit={handleSubmit}>
            <h2>{mode === "login" ? "Login" : "Register"}</h2>
            <input
                type="text"
                placeholder="Username"
                value={username}
                onChange={(e) => setUsername(e.target.value)}
                required
                autoComplete="username"
            />
            <input
                type="password"
                placeholder="Password"
                value={password}
                onChange={(e) => setPassword(e.target.value)}
                required
                autoComplete={
                    mode === "login" ? "current-password" : "new-password"
                }
            />
            <Button
                type="submit"
                variant="contained"
                color="primary"
                disabled={loading}
                fullWidth
                sx={{ mt: 2, mb: 1 }}
            >
                {loading
                    ? mode === "login"
                        ? "Logging in..."
                        : "Registering..."
                    : mode === "login"
                    ? "Login"
                    : "Register"}
            </Button>
            <Box sx={{ mt: 1 }}>
                {mode === "login" ? (
                    <Typography variant="body2">
                        No account?{" "}
                        <Button
                            variant="text"
                            onClick={() => setMode("register")}
                            sx={{
                                p: 0,
                                minWidth: 0,
                                verticalAlign: "baseline",
                            }}
                        >
                            Register
                        </Button>
                    </Typography>
                ) : (
                    <Typography variant="body2">
                        Already have an account?{" "}
                        <Button
                            variant="text"
                            onClick={() => setMode("login")}
                            sx={{
                                p: 0,
                                minWidth: 0,
                                verticalAlign: "baseline",
                            }}
                        >
                            Login
                        </Button>
                    </Typography>
                )}
            </Box>
            {error && <div className="error-message">{error}</div>}
        </form>
    );
};

export default AuthForm;
