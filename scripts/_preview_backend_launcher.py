"""Preview-only launcher: sets local-profile env vars before importing the app,
since the Preview tool's launch.json has no way to pass per-process env vars."""
import os
import sys
from pathlib import Path

os.environ.setdefault("PCCT_PROFILE", "local")
os.environ.setdefault("PCCT_PROMPT_LOGIN_ON_STARTUP", "0")
os.environ.setdefault("PCCT_ALLOW_MOCK_USER_HEADER", "1")

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "backend" / "mock_gateway"))

import uvicorn  # noqa: E402

if __name__ == "__main__":
    uvicorn.run("main:app", host="127.0.0.1", port=8000)
