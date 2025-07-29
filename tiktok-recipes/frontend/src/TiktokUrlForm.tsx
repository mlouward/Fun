import React, { useState } from "react";
import { Box, Typography, TextField, Button } from "@mui/material";

interface TiktokUrlFormProps {
    onProcess: (url: string) => void;
    loading?: boolean;
    error?: string | null;
}

const TiktokUrlForm: React.FC<TiktokUrlFormProps> = ({
    onProcess,
    loading,
    error,
}) => {
    const [url, setUrl] = useState("");

    const handleSubmit = (e: React.FormEvent) => {
        e.preventDefault();
        if (url.trim()) {
            onProcess(url.trim());
        }
    };

    return (
        <Box sx={{
            textAlign: "center",
            mb: 4,
            width: '90%',
            maxWidth: { xs: '90vw', sm: 600 }, // Use 90vw for xs, 600px for sm and up
            mx: 'auto',
        }}>
            <Typography
                variant="h6"
                sx={{ mb: 2, color: "var(--primary-color)" }}
            >
                Get started by transcribing a TikTok recipe!
            </Typography>

            <form
                id="tiktok-url-form"
                onSubmit={handleSubmit}
                style={{ marginBottom: "2em" }}
            >
                <TextField
                type="url"
                id="tiktok-url"
                name="tiktok-url"
                label="Paste a TikTok recipe URL"
                value={url}
                onChange={(e) => setUrl(e.target.value)}
                placeholder="https://www.tiktok.com/@user/video/1234567890"
                fullWidth
                required
                sx={{ mb: 2 }}
            />
            <Button type="submit" variant="contained" color="primary" disabled={loading || !url.trim()} fullWidth>
                {loading ? "Processing..." : "Transcribe Recipe"}
            </Button>
                {error && <div className="error-message">{error}</div>}
            </form>
        </Box>
    );
};

export default TiktokUrlForm;
