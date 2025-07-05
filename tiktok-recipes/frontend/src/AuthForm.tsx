import React, { useState } from "react";

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
            <button type="submit" disabled={loading}>
                {loading
                    ? mode === "login"
                        ? "Logging in..."
                        : "Registering..."
                    : mode === "login"
                    ? "Login"
                    : "Register"}
            </button>
            <div style={{ marginTop: 8 }}>
                {mode === "login" ? (
                    <span>
                        No account?{" "}
                        <button
                            type="button"
                            onClick={() => setMode("register")}
                        >
                            Register
                        </button>
                    </span>
                ) : (
                    <span>
                        Already have an account?{" "}
                        <button type="button" onClick={() => setMode("login")}>
                            Login
                        </button>
                    </span>
                )}
            </div>
            {error && <div className="error-message">{error}</div>}
        </form>
    );
};

export default AuthForm;
