import gzip
import json
from typing import Optional

import requests
from requests.auth import HTTPBasicAuth


class PaprikaAPI:
    """
    Minimal Paprika API client for uploading recipes (reverse-engineered, see PLAN.md).
    """

    BASE_URL = "https://www.paprikaapp.com/api/v1"
    SYNC_URL = "https://www.paprikaapp.com/api/v1/sync/recipe/{uid}/"
    LOGIN_URL = "https://www.paprikaapp.com/api/v1/account/login/"

    def __init__(self, email: str, password: str):
        self.email = email
        self.password = password
        self.token = None

    def login(self) -> Optional[str]:
        resp = requests.post(
            self.LOGIN_URL,
            data={"email": self.email, "password": self.password},
            auth=HTTPBasicAuth(self.email, self.password),
        )
        if resp.status_code == 200:
            self.token = resp.json().get("result", {}).get("token")
            return self.token
        return None

    def upload_recipe(self, recipe: dict) -> bool:
        if not self.token:
            raise Exception("Not authenticated. Call login() first.")
        uid = recipe.get("uid")
        if not uid:
            raise Exception("Recipe must have a 'uid' field.")
        url = self.SYNC_URL.format(uid=uid)
        gzipped = gzip.compress(json.dumps(recipe).encode("utf-8"))
        files = {"data": ("data.json.gz", gzipped)}
        headers = {"Authorization": f"Bearer {self.token}"}
        resp = requests.post(url, files=files, headers=headers)
        return resp.status_code == 200
