import concurrent.futures
import gzip
import os
import shutil
import sys
import time
from datetime import datetime, timedelta

import jsonlines
import requests
from dotenv import load_dotenv
from tqdm.notebook import tqdm
