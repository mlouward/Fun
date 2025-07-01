# TikTok Recipe Transcriber - Project Plan

## Suggested Project Structure

```text
tiktok-recipes/
├── audio_files/               # To store downloaded audio
├── data/                      # To store transcribed recipes (JSON)
├── src/
│   ├── __init__.py
│   ├── audio_extractor.py     # Module for downloading audio
│   ├── transcriber.py         # Module for transcribing audio to text
│   ├── nlp_processor.py       # Module for extracting recipe info from text
│   ├── paprika_exporter.py    # Module for exporting to Paprika format
│   └── main.py                # Main application logic (FastAPI)
├── templates/                 # HTML templates for the web UI
│   └── index.html
├── static/                    # CSS and JS files
│   └── styles.css
├── tests/                     # Unit and integration tests
│   ├── __init__.py
│   ├── test_audio_extractor.py
│   ├── test_transcriber.py
│   └── test_nlp_processor.py
├── .gitignore
├── pyproject.toml
├── README.md                  # Updated with project details
└── PLAN.md                    # This plan
```

## Long-Term Plan

### Phase 1: Core Functionality (Backend)

1. **Restructure Project (Completed)**:
    * Directories created and `audio_extractor.py` moved to `src/`.

2. **Transcription Module (Completed)**:
    * `src/transcriber.py` created with a function to transcribe audio using `openai-whisper`.

3. **Create Paprika JSON Schema**:
    * Create a new file: `src/paprika_schema.json`.
    * Define the full JSON structure for a Paprika recipe in this file. This will serve as the template and guide for the NLP model.

4. **NLP Processing Module (Revision)**:
    * Update `src/nlp_processor.py`.
    * Modify the module to use the `transformers` library with the `numind/NuExtract-2.0-2B` model for structured data extraction.
    * The `extract_recipe_info(transcript)` function will:
        * Load the `paprika_schema.json`.
        * Construct a detailed prompt that instructs the model to populate the JSON schema with information from the transcript.
        * Process the prompt and transcript to generate a JSON-formatted string.
        * Parse and return the resulting dictionary.

5. **Paprika Export Module (Revision)**:
    * Update `src/paprika_exporter.py`.
    * The `format_for_paprika(recipe_dict)` function will now take the dictionary produced by the NLP module.
    * It will validate this dictionary, add server-generated values (`uid`, `hash`, `created`), and save the final, complete recipe to a file in the `data/` directory.

### Phase 2: Web Interface (Frontend & API)

1. **FastAPI Application**:
    * Create `src/main.py`.
    * Set up a basic FastAPI app.

2. **API Endpoint**:
    * Create a `/transcribe` endpoint that accepts a TikTok URL.
    * This endpoint will call the functions from the backend modules in sequence.
    * It will return the final recipe in JSON format.

3. **Basic Frontend**:
    * Create `templates/index.html` with a form for the TikTok URL.
    * Add JavaScript to call the `/transcribe` endpoint and display the results.

### Phase 3: Paprika Integration

1. **Paprika API Client**:
    * In `src/paprika_exporter.py`, add functionality to export a recipe to Paprika.

2. **Download Functionality**:
    * Users can download the recipe in Paprika format from the web interface.

### Phase 4: Refinement and Testing

1. **Unit Tests**:
    * Write tests for each module in the `tests/` directory to ensure reliability.

2. **Error Handling & Configuration**:
    * Improve error handling across the application.
    * Use a configuration file for managing settings like API keys.

3. **Documentation**:
    * Update the `README.md` with detailed setup and usage instructions.

## Future Improvements

### 1. User Experience & Frontend

* **Recipe Gallery:** Add a page to browse all processed recipes with thumbnails, search, and filter by ingredient or difficulty.
* **Recipe Editing:** Allow users to edit the extracted recipe before exporting or uploading to Paprika.
* **Progress Feedback:** Show step-by-step progress (downloading, transcribing, extracting, etc.) in the UI.

### 2. Backend & API

* **Async Processing:** Offload long-running tasks (transcription, NLP) to a background worker (e.g., Celery, FastAPI background tasks, or RQ) and provide job status endpoints.
* **WebSocket Notifications:** Use WebSockets to push progress updates to the frontend.

### 3. Recipe Quality & NLP

* **Ingredient Normalization:** Use an ingredient parser to standardize ingredient names and units for better search and export.

### 4. Paprika & Export

* **Bulk Export:** Allow users to select and export multiple recipes at once.
