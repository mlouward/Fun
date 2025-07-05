import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

export default defineConfig({
    plugins: [react()],
    build: {
        outDir: "dist",
        sourcemap: true,
    },
    // This ensures environment variables are loaded from .env files
    envPrefix: 'VITE_',
});
