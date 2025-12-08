import telnetlib
import logging

from fastapi import FastAPI, HTTPException
from pydantic import BaseModel

# === CONFIGURE THIS FOR YOUR WSL INSTANCE ===
COG_HOST = "172.23.32.204"  # <- your WSL eth0 IP (you printed this already)
COG_PORT = 17001
TELNET_TIMEOUT = 5  # seconds

SENTINEL = "__REST_DONE__"

logger = logging.getLogger("rest-bridge")

app = FastAPI(
    title="OpenCog REST bridge",
    version="0.1.0",
    description="Tiny REST → Telnet bridge into OpenCog CogServer (Scheme shell).",
)


class AtomeseRequest(BaseModel):
    code: str


def run_atomese_via_telnet(code: str) -> str:
    # 1) connect to CogServer telnet
    try:
        tn = telnetlib.Telnet(COG_HOST, COG_PORT, timeout=TELNET_TIMEOUT)
    except Exception as e:
        raise HTTPException(
            status_code=502,
            detail=f"Could not connect to CogServer at {COG_HOST}:{COG_PORT}: {e!r}",
        )

    try:
        # 2) wait for the main opencog prompt
        tn.read_until(b"opencog> ", timeout=TELNET_TIMEOUT)

        # 3) enter Scheme shell
        tn.write(b"scm\n")
        tn.read_until(b"guile> ", timeout=TELNET_TIMEOUT)

        # 4) send the user's code + a sentinel marker so we know when to stop reading
        payload = code.strip() + f'\n(display "{SENTINEL}")\n(newline)\n'
        tn.write(payload.encode("utf-8"))

        raw = tn.read_until(SENTINEL.encode("utf-8"), timeout=TELNET_TIMEOUT)

        # 5) close connection
        tn.close()

        text = raw.decode("utf-8", errors="ignore")
        # remove the sentinel itself
        text = text.replace(SENTINEL, "")
        return text.strip()

    except EOFError:
        tn.close()
        raise HTTPException(status_code=500, detail="CogServer closed the connection.")
    except Exception as e:
        tn.close()
        raise HTTPException(status_code=500, detail=f"Telnet error: {e!r}")


@app.post("/atomese")
def atomese_endpoint(req: AtomeseRequest):
    """
    Run arbitrary Atomese/Scheme code in CogServer and return the raw text output.
    """
    return run_atomese_via_telnet(req.code)
