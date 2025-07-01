import React, { useState } from "react";

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
        <form
            id="tiktok-url-form"
            onSubmit={handleSubmit}
            style={{ marginBottom: "2em" }}
        >
            <label htmlFor="tiktok-url">Paste a TikTok recipe URL:</label>
            <input
                type="url"
                id="tiktok-url"
                name="tiktok-url"
                value={url}
                onChange={(e) => setUrl(e.target.value)}
                placeholder="https://www.tiktok.com/@user/video/1234567890"
                required
                style={{ width: "100%", marginBottom: "0.5em" }}
            />
            <button type="submit" disabled={loading || !url.trim()}>
                {loading ? "Processing..." : "Transcribe Recipe"}
            </button>
            {error && <div className="error-message">{error}</div>}
        </form>
    );
};

export default TiktokUrlForm;
