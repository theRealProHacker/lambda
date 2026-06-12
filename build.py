"""
Build script for the lambda calculus web app.

Reads pylambda.py (stripping the __main__ block), appends browser-specific code,
and injects the combined Python code into web/template.html to produce
the deployable index.html.
"""

import re
import sys
from pathlib import Path

ROOT = Path(__file__).parent
WEB = ROOT / "web"
OUTPUT = ROOT / "_site"

PLACEHOLDER = "<!-- PYTHON_CODE -->"

BROWSER_CODE = """
from browser import document

def on_input(ev):
    try:
        expr = parse_expr(document["input"].value)
        result = reduce(expr, max_steps=200, max_size=2000)
        document["output"].value = str(result)
    except RuntimeError:
        document["output"].value = "no normal form found (expression diverges)"
    except Exception:
        document["output"].value = ""

document["input"].bind("input", on_input)
"""


def strip_main_block(source: str) -> str:
    """Remove the if __name__ == '__main__': block from the source."""
    return re.sub(
        r'\n*if __name__\s*==\s*["\']__main__["\']\s*:.*',
        "",
        source,
        flags=re.DOTALL,
    ).rstrip() + "\n"


def build():
    template_path = WEB / "template.html"
    pylambda_path = ROOT / "pylambda.py"

    if not template_path.exists():
        sys.exit(f"Error: {template_path} not found.")
    if not pylambda_path.exists():
        sys.exit(f"Error: {pylambda_path} not found.")

    template = template_path.read_text(encoding="utf-8")
    pylambda = pylambda_path.read_text(encoding="utf-8")

    if PLACEHOLDER not in template:
        sys.exit(f"Error: placeholder {PLACEHOLDER!r} missing from {template_path} — "
                 "the Python code would not be injected.")
    if 'onload="brython()"' not in template:
        sys.exit(f"Error: {template_path} never calls brython() on body load — "
                 "the injected Python would not execute.")

    # Strip the __main__ block
    core_code = strip_main_block(pylambda)

    # Combine: core library + browser bindings
    combined = core_code + "\n" + BROWSER_CODE

    # Indent the Python code to match the <script> block indentation (4 spaces)
    indented = "\n".join("    " + line if line.strip() else "" for line in combined.splitlines())

    # Inject into template
    html = template.replace(PLACEHOLDER, indented)

    if "def parse_expr" not in html or "document[\"input\"].bind" not in html:
        sys.exit("Error: built index.html is missing the injected Python code.")

    OUTPUT.mkdir(exist_ok=True)
    (OUTPUT / "index.html").write_text(html, encoding="utf-8")
    print(f"Built {OUTPUT / 'index.html'}")


if __name__ == "__main__":
    build()

