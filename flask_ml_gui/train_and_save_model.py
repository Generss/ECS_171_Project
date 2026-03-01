import pandas as pd

from sklearn.model_selection import KFold, cross_val_score
from sklearn.pipeline import Pipeline
from sklearn.impute import SimpleImputer
from sklearn.ensemble import HistGradientBoostingRegressor

import joblib


def train_best_pipeline(csv_path: str) -> Pipeline:
    df = pd.read_csv(csv_path)

    if "percentage_liked" not in df.columns:
        raise ValueError("final_output.csv must contain a 'percentage_liked' column.")

    X = df.drop(columns=["percentage_liked"])
    y = df["percentage_liked"]

    X = X.apply(pd.to_numeric, errors="coerce")

    all_nan_cols = X.columns[X.isna().all()].tolist()
    if all_nan_cols:
        raise ValueError(
            "These columns became all-NaN after numeric conversion (likely non-numeric strings):\n"
            + "\n".join(all_nan_cols)
            + "\nFix preprocessing (ensure one-hot is numeric) or drop these columns."
        )

    kf = KFold(n_splits=5, shuffle=True, random_state=42)

    best_rmse = float("inf")
    best_pipe = None
    best_params = None

    for max_depth in [None, 3, 5, 8]:
        for lr in [0.03, 0.06, 0.1]:
            for max_leaf_nodes in [31, 63, 127]:
                model = HistGradientBoostingRegressor(
                    learning_rate=lr,
                    max_depth=max_depth,
                    max_leaf_nodes=max_leaf_nodes,
                    max_iter=500,
                    random_state=42,
                )

                pipe = Pipeline(
                    steps=[
                        ("imputer", SimpleImputer(strategy="median")),
                        ("model", model),
                    ]
                )

                neg_rmse = cross_val_score(
                    pipe, X, y, cv=kf, scoring="neg_root_mean_squared_error"
                )
                rmse_mean = float((-neg_rmse).mean())

                if rmse_mean < best_rmse:
                    best_rmse = rmse_mean
                    best_pipe = pipe
                    best_params = {
                        "max_depth": max_depth,
                        "learning_rate": lr,
                        "max_leaf_nodes": max_leaf_nodes,
                        "rmse_mean": rmse_mean,
                    }

    if best_pipe is None:
        raise RuntimeError("Failed to select a best model (no candidates evaluated).")

    best_pipe.fit(X, y)

    print("Best params:", best_params)
    return best_pipe


def main():
    model_path = "model.joblib"
    csv_path = "final_output.csv"

    pipe = train_best_pipeline(csv_path)
    joblib.dump(pipe, model_path)

    print(f"Saved trained pipeline to {model_path}")


if __name__ == "__main__":
    main()