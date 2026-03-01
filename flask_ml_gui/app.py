import io
from pathlib import Path

import joblib
import numpy as np
import pandas as pd
from flask import Flask, render_template, request

app = Flask(__name__)

BASE_DIR = Path(__file__).resolve().parent
MODEL_PATH = BASE_DIR / "model.joblib"
DATA_GUIDE_PATH = BASE_DIR / "final_output.csv"
TARGET_COL = "percentage_liked"

pipe = joblib.load(MODEL_PATH)

# -----------------------------
# Infer feature columns
# -----------------------------
feature_cols = None
try:
    if hasattr(pipe, "named_steps") and "model" in pipe.named_steps:
        m = pipe.named_steps["model"]
        if hasattr(m, "feature_names_in_"):
            feature_cols = list(m.feature_names_in_)
except Exception:
    feature_cols = None

df_guide = pd.read_csv(DATA_GUIDE_PATH, nrows=3000)
if TARGET_COL not in df_guide.columns:
    raise ValueError(f"{DATA_GUIDE_PATH.name} must include '{TARGET_COL}' column.")

if feature_cols is None:
    feature_cols = [c for c in df_guide.columns if c != TARGET_COL]

# -----------------------------
# Guided numeric fields (if present)
# -----------------------------
FORM_FIELDS = [
    "price",
    "owners",
    "required_age",
    "english",
    "achievements",
    "average_playtime",
    "median_playtime",
    "age_since_1998",
    "is_free",
]
FORM_FIELDS = [c for c in FORM_FIELDS if c in feature_cols]

# -----------------------------
# Detect binary/one-hot columns from guide
# -----------------------------
binary_cols = set()
for c in feature_cols:
    if c in df_guide.columns:
        s = pd.to_numeric(df_guide[c], errors="coerce")
        uniq = pd.unique(s.dropna())
        if len(uniq) > 0 and set(uniq).issubset({0.0, 1.0}):
            binary_cols.add(c)

# For the dropdown, we want "one-hot candidates" = binary columns EXCEPT obvious numeric toggles
# (keep your main FORM_FIELDS out of the one-hot dropdown)
onehot_candidates = sorted([c for c in binary_cols if c not in set(FORM_FIELDS)])

# Optional: show only the most common one-hots (prevents a 50k-item dropdown)
# We'll rank by frequency of 1s in the guide sample.
onehot_freq = []
for c in onehot_candidates:
    if c in df_guide.columns:
        s = pd.to_numeric(df_guide[c], errors="coerce")
        onehot_freq.append((c, float(np.nansum(s.values == 1.0))))
onehot_freq.sort(key=lambda x: x[1], reverse=True)

MAX_DROPDOWN = 500  # adjust as you want (100–2000 typical)
onehot_dropdown = [c for c, _ in onehot_freq[:MAX_DROPDOWN]]

# -----------------------------
# UI stats for guided numeric fields
# -----------------------------
ui_stats = {}
for c in FORM_FIELDS:
    if c in df_guide.columns:
        s = pd.to_numeric(df_guide[c], errors="coerce")
        if np.isfinite(s).any():
            ui_stats[c] = {
                "min": float(np.nanmin(s)),
                "max": float(np.nanmax(s)),
                "median": float(np.nanmedian(s)),
            }


def make_default_row_df(n_rows: int = 1) -> pd.DataFrame:
    """Binary defaults 0; everything else NaN so imputer fills medians."""
    data = {}
    for c in feature_cols:
        if c in binary_cols:
            data[c] = [0.0] * n_rows
        else:
            data[c] = [np.nan] * n_rows
    return pd.DataFrame(data, columns=feature_cols)


def align_user_df_to_features(user_df: pd.DataFrame) -> pd.DataFrame:
    if TARGET_COL in user_df.columns:
        user_df = user_df.drop(columns=[TARGET_COL])

    user_df = user_df.apply(pd.to_numeric, errors="coerce")

    out = make_default_row_df(n_rows=len(user_df))
    for c in user_df.columns:
        if c in out.columns:
            out[c] = user_df[c].values
    return out


@app.route("/", methods=["GET"])
def index():
    return render_template(
        "index.html",
        form_fields=FORM_FIELDS,
        ui_stats=ui_stats,
        feature_count=len(feature_cols),
        onehot_dropdown=onehot_dropdown,
        onehot_dropdown_count=len(onehot_dropdown),
        onehot_total_count=len(onehot_candidates),
    )


@app.route("/predict", methods=["POST"])
def predict():
    mode = request.form.get("mode", "form")  # form | csv

    try:
        if mode == "form":
            # 1) Numeric guided inputs
            row = {}
            for c in FORM_FIELDS:
                raw = request.form.get(c, "").strip()
                row[c] = np.nan if raw == "" else float(raw)

            user_df = pd.DataFrame([row])
            X_in = align_user_df_to_features(user_df)

            # 2) One-hot selections from UI
            # Hidden field: comma-separated list of column names
            selected_raw = request.form.get("selected_onehots", "").strip()
            selected = [s for s in selected_raw.split(",") if s]

            # Set selected one-hot columns to 1 (only if they are valid binary cols)
            for col in selected:
                if col in X_in.columns and col in binary_cols:
                    X_in.loc[:, col] = 1.0

        elif mode == "csv":
            csv_text = request.form.get("csv_text", "").strip()
            file = request.files.get("csv_file", None)

            if csv_text:
                user_df = pd.read_csv(io.StringIO(csv_text))
            elif file and file.filename:
                user_df = pd.read_csv(file)
            else:
                raise ValueError("CSV mode selected, but no pasted CSV or file was provided.")

            if len(user_df) < 1:
                raise ValueError("CSV input has no rows.")

            X_in = align_user_df_to_features(user_df)

        else:
            raise ValueError("Unknown mode.")

        preds = pipe.predict(X_in)
        preds = [float(p) for p in preds]

        return render_template(
            "result.html",
            mode=mode,
            prediction_first=preds[0],
            prediction_all=preds,
            n_rows=len(preds),
        )

    except Exception as e:
        return render_template("result.html", error=str(e))


if __name__ == "__main__":
    app.run(debug=True)