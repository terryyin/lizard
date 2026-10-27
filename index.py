"""The Lizard website: a small HTTP adapter around the public analyzer."""

from flask import Flask, jsonify, render_template, request
from werkzeug.exceptions import BadRequest, RequestEntityTooLarge

from lizard import analyze_file
from lizard_languages import get_reader_for, languages


app = Flask(__name__, static_folder="website/static",
            static_url_path="/website/static")
app.config["MAX_CONTENT_LENGTH"] = 512 * 1024
MAX_SOURCE_BYTES = 64 * 1024
SOURCE_LIMIT_MESSAGE = (
    f"The online demo accepts up to {MAX_SOURCE_BYTES // 1024} KiB of source code. "
    "Use the local CLI for larger files."
)


class InvalidSource(ValueError):
    """A safe, user-facing request validation error."""


@app.get("/")
def homepage():
    options = [(reader.__name__[:-6], "." + reader.ext[0])
               for reader in languages()]
    options = [("C / C++" if name == "CLike" else name, extension)
               for name, extension in options]
    return render_template(
        "index.html", languages=sorted(options),
        max_source_bytes=MAX_SOURCE_BYTES,
        source_limit_message=SOURCE_LIMIT_MESSAGE,
    )


@app.get("/favicon.ico")
def favicon():
    return app.send_static_file("favicon.ico")


def analysis_result(payload):
    """Validate a source request and expose only the public result metrics."""
    if not isinstance(payload, dict):
        raise InvalidSource("Send a JSON object with code and a language.")
    code = payload.get("code")
    filename = payload.get("filename", payload.get("lang", ".c"))
    if not isinstance(code, str) or not code.strip():
        raise InvalidSource("Paste some source code before analyzing.")
    try:
        source_bytes = len(code.encode("utf-8"))
    except UnicodeEncodeError as error:
        raise InvalidSource("Source code must contain valid Unicode text.") from error
    if source_bytes > MAX_SOURCE_BYTES:
        raise RequestEntityTooLarge()
    if (not isinstance(filename, str) or len(filename) > 128
            or get_reader_for(filename) is None):
        raise InvalidSource("Choose a supported language or filename extension.")
    info = analyze_file.analyze_source_code(filename, code)
    fields = ("name", "long_name", "start_line", "end_line", "nloc",
              "cyclomatic_complexity", "token_count", "parameter_count")
    return {
        "filename": info.filename,
        "nloc": info.nloc,
        "token_count": info.token_count,
        "function_list": [
            {field: getattr(function, field) for field in fields}
            for function in info.function_list
        ],
    }


@app.post("/analyze")
@app.post("/analyse")
def analyze():
    if (request.path == "/analyse"
            and request.mimetype == "application/x-www-form-urlencoded"):
        payload = {"code": request.form.get("content"),
                   "lang": request.form.get("lang", ".cpp")}
    elif not request.is_json:
        return jsonify(error="Send source code as application/json."), 415
    else:
        try:
            payload = request.get_json()
        except BadRequest:
            return jsonify(error="The request contains invalid JSON."), 400
    try:
        return jsonify(analysis_result(payload))
    except InvalidSource as error:
        return jsonify(error=str(error)), 400
    except RequestEntityTooLarge:
        raise
    except Exception:  # Never expose source or parser details in responses/logs.
        return jsonify(error="Analysis is unavailable. Please try again or use the local CLI."), 503


@app.errorhandler(413)
def request_too_large(_error):
    return jsonify(error=SOURCE_LIMIT_MESSAGE), 413


@app.after_request
def response_headers(response):
    response.headers["X-Content-Type-Options"] = "nosniff"
    response.headers["Referrer-Policy"] = "strict-origin-when-cross-origin"
    response.headers["Content-Security-Policy"] = (
        "default-src 'self'; script-src 'self'; style-src 'self'; "
        "img-src 'self'; connect-src 'self'; base-uri 'none'; "
        "form-action 'self'; frame-ancestors 'none'"
    )
    if request.method == "POST":
        response.headers["Cache-Control"] = "no-store"
    return response

