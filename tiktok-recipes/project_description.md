# TikTok Recipe Transcriber

## Concept

Develop an application where you paste a TikTok video URL, and it attempts to transcribe the recipe steps, ingredients, and quantities into a structured text format.

## Key Challenges & Learning

### Download audio

Using yt-dlp, download the audio from the TikTok video.

### Video Processing/Transcription

This would be the core challenge. Setup and use OpenAI's Whisper to transcribe the audio from the TikTok video.

### Natural Language Processing (NLP)

Once you have the raw text, you'd need to use NLP techniques to extract meaningful information: identifying ingredients, quantities, cooking instructions, and timings. We will use spacy models & potentially regexes.

### Data Structuring

Use Paprika App's JSON schema to store the extracted recipe information.

Example:

```json
{
  "uid": "ccb42915-5fe9-425d-98da-c1ffbe420159",
  "name": "Test",
  "directions": "Prepare the chicken...",
  "servings": "",
  "rating": 0,
  "difficulty": "Easy",
  "ingredients": "1 chicken\n1 onion\n1 garlic\n1 tomato\n1 cup of water",
  "notes": "",
  "created": "2018-03-26 09:00:02",
  "image_url": null,
  "on_favorites": 0,
  "cook_time": "",
  "prep_time": "",
  "source": "",
  "source_url": "",
  "photo_hash": null,
  "photo": null,
  "nutritional_info": "",
  "scale": null,
  "deleted": false,
  "categories": [
    "cbaca738-cdfb-4150-960d-e1b1ac4cdcc3"
  ],
  "hash": "162e5ad0134e9398b98057aea951304780d0396582238320c28b34a7c35f841e"
}
```

### Export Functionality

Use this [user's gist](https://gist.github.com/mattdsteele/7386ec363badfdeaad05a418b9a1f30a) or transform this [rust library](https://gitlab.com/CyanBlob/paprika-api/-/tree/master/src) into Python to figure out how to log into Paprika and upload the recipe.

### User Interface

Implement a simple web interface (using FastAPI with a basic HTML/CSS front end).
