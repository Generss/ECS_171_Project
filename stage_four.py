import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from sklearn.model_selection import KFold
from sklearn.pipeline import Pipeline
from sklearn.impute import SimpleImputer
from sklearn.ensemble import HistGradientBoostingRegressor
from sklearn.metrics import (
    mean_squared_error,
    mean_absolute_error,
    r2_score,
)

def rmse(y_true, y_pred):
    return np.sqrt(mean_squared_error(y_true, y_pred))

def main():
    new_df = pd.read_csv("final_output.csv")
    if "percentage_liked" not in new_df.columns:
        raise ValueError("final_output.csv must contain a 'percentage_liked' column.")

    X = new_df.drop(columns=["percentage_liked"])
    y = new_df["percentage_liked"].to_numpy()

    # Ensure numeric (as you had)
    X = X.apply(pd.to_numeric, errors="coerce")

    all_nan_cols = X.columns[X.isna().all()].tolist()
    if all_nan_cols:
        raise ValueError(
            "These columns became all-NaN after numeric conversion (likely non-numeric strings):\n"
            + "\n".join(all_nan_cols)
            + "\nFix preprocessing (ensure one-hot is numeric) or drop these columns."
        )

    X = X.to_numpy()

    kf = KFold(n_splits=5, shuffle=True, random_state=42)

    # Grid
    grid = []
    for max_depth in [None, 3, 5, 8]:
        for lr in [0.03, 0.06, 0.1]:
            for max_leaf_nodes in [31, 63, 127]:
                grid.append((max_depth, lr, max_leaf_nodes))

    results = []

    # We'll store the best and later rerun it to get rich diagnostics.
    best_row = None

    for max_depth, lr, max_leaf_nodes in grid:
        fold_rmses = []
        fold_maes = []
        fold_r2s = []

        for train_idx, val_idx in kf.split(X):
            X_train, X_val = X[train_idx], X[val_idx]
            y_train, y_val = y[train_idx], y[val_idx]

            model = HistGradientBoostingRegressor(
                learning_rate=lr,
                max_depth=max_depth,
                max_leaf_nodes=max_leaf_nodes,
                max_iter=500,
                random_state=42,
                # keep this off for grid speed; we'll enable for best model plots
                early_stopping=False,
            )
            pipe = Pipeline([
                ("imputer", SimpleImputer(strategy="median")),
                ("model", model),
            ])

            pipe.fit(X_train, y_train)
            pred = pipe.predict(X_val)

            fold_rmses.append(rmse(y_val, pred))
            fold_maes.append(mean_absolute_error(y_val, pred))
            fold_r2s.append(r2_score(y_val, pred))

        fold_rmses = np.array(fold_rmses)
        fold_maes = np.array(fold_maes)
        fold_r2s = np.array(fold_r2s)

        row = {
            "max_depth": max_depth,
            "learning_rate": lr,
            "max_leaf_nodes": max_leaf_nodes,
            "rmse_mean": float(fold_rmses.mean()),
            "rmse_std": float(fold_rmses.std(ddof=1)),
            "rmse_var": float(fold_rmses.var(ddof=1)),
            "mae_mean": float(fold_maes.mean()),
            "mae_std": float(fold_maes.std(ddof=1)),
            "r2_mean": float(fold_r2s.mean()),
            "r2_std": float(fold_r2s.std(ddof=1)),
        }
        results.append(row)

        if best_row is None or row["rmse_mean"] < best_row["rmse_mean"]:
            best_row = row

    results_df = pd.DataFrame(results).sort_values("rmse_mean")
    print("\nTop 10 by lowest mean RMSE:")
    print(results_df.head(10).to_string(index=False))

    print("\nBest params (lowest mean RMSE):")
    print({
        "max_depth": best_row["max_depth"],
        "learning_rate": best_row["learning_rate"],
        "max_leaf_nodes": best_row["max_leaf_nodes"],
        "rmse_mean": best_row["rmse_mean"],
        "rmse_std": best_row["rmse_std"],
        "r2_mean": best_row["r2_mean"],
    })

    # ---- Rich diagnostics for BEST model ----
    best_max_depth = best_row["max_depth"]
    best_lr = best_row["learning_rate"]
    best_max_leaf_nodes = best_row["max_leaf_nodes"]

    # For curves + OOF predictions
    oof_pred = np.empty_like(y, dtype=float)
    per_fold_curves = []  # list of dicts: {"train_rmse": [...], "val_rmse": [...], "n_iter": int}
    fold_metrics = []

    for fold_i, (train_idx, val_idx) in enumerate(kf.split(X), start=1):
        X_train, X_val = X[train_idx], X[val_idx]
        y_train, y_val = y[train_idx], y[val_idx]

        model = HistGradientBoostingRegressor(
            learning_rate=best_lr,
            max_depth=best_max_depth,
            max_leaf_nodes=best_max_leaf_nodes,
            max_iter=500,
            random_state=42,
            # Turn ON early stopping so we see convergence and best_iteration_
            early_stopping=True,
            validation_fraction=0.1,
            n_iter_no_change=20,
            tol=1e-7,
        )

        pipe = Pipeline([
            ("imputer", SimpleImputer(strategy="median")),
            ("model", model),
        ])

        pipe.fit(X_train, y_train)

        # OOF prediction
        pred_val = pipe.predict(X_val)
        oof_pred[val_idx] = pred_val

        # Build RMSE-vs-iteration curves using staged predictions from the underlying model.
        # We must pass transformed data to the staged_predict generator.
        imputer = pipe.named_steps["imputer"]
        mdl = pipe.named_steps["model"]

        X_train_imp = imputer.transform(X_train)
        X_val_imp = imputer.transform(X_val)

        train_curve = []
        val_curve = []

        for yhat_train in mdl.staged_predict(X_train_imp):
            train_curve.append(rmse(y_train, yhat_train))
        for yhat_val in mdl.staged_predict(X_val_imp):
            val_curve.append(rmse(y_val, yhat_val))

        n_iter = min(len(train_curve), len(val_curve))
        train_curve = train_curve[:n_iter]
        val_curve = val_curve[:n_iter]

        per_fold_curves.append({
            "fold": fold_i,
            "train_rmse": train_curve,
            "val_rmse": val_curve,
            "n_iter": n_iter,
            "best_iteration_": getattr(mdl, "n_iter_", n_iter),
        })

        fold_metrics.append({
            "fold": fold_i,
            "rmse": rmse(y_val, pred_val),
            "mae": mean_absolute_error(y_val, pred_val),
            "r2": r2_score(y_val, pred_val),
        })

    fold_metrics_df = pd.DataFrame(fold_metrics)
    print("\nBest-model per-fold metrics:")
    print(fold_metrics_df.to_string(index=False))

    # Aggregate OOF metrics
    oof_rmse = rmse(y, oof_pred)
    oof_mae = mean_absolute_error(y, oof_pred)
    oof_r2 = r2_score(y, oof_pred)

    print("\nBest-model OOF (out-of-fold) metrics (aggregated):")
    print({
        "oof_rmse": float(oof_rmse),
        "oof_mae": float(oof_mae),
        "oof_r2": float(oof_r2),
    })

    # Rough 95% CI on RMSE mean from folds (t approximation)
    rmse_vals = fold_metrics_df["rmse"].to_numpy()
    rmse_mean = rmse_vals.mean()
    rmse_std = rmse_vals.std(ddof=1)
    rmse_se = rmse_std / np.sqrt(len(rmse_vals))
    # For df=4, t_0.975 ~ 2.776 (hardcode to avoid scipy dependency)
    t975_df4 = 2.776
    rmse_ci = (rmse_mean - t975_df4 * rmse_se, rmse_mean + t975_df4 * rmse_se)

    print("\nBest-model RMSE fold-mean uncertainty:")
    print({
        "rmse_mean": float(rmse_mean),
        "rmse_std": float(rmse_std),
        "rmse_var": float(rmse_vals.var(ddof=1)),
        "rmse_95pct_CI_approx": (float(rmse_ci[0]), float(rmse_ci[1])),
    })

    # ---------------- Plots ----------------

    # 1) RMSE vs iteration, per fold
    plt.figure()
    for c in per_fold_curves:
        iters = np.arange(1, c["n_iter"] + 1)
        plt.plot(iters, c["val_rmse"], label=f"Fold {c['fold']} val")
    plt.xlabel("Boosting iteration")
    plt.ylabel("RMSE")
    plt.title("Validation RMSE vs boosting iteration (best model, per fold)")
    plt.legend()
    plt.tight_layout()
    plt.savefig("best_model_val_rmse_curves.png", dpi=150)

    plt.figure()
    for c in per_fold_curves:
        iters = np.arange(1, c["n_iter"] + 1)
        plt.plot(iters, c["train_rmse"], label=f"Fold {c['fold']} train")
    plt.xlabel("Boosting iteration")
    plt.ylabel("RMSE")
    plt.title("Training RMSE vs boosting iteration (best model, per fold)")
    plt.legend()
    plt.tight_layout()
    plt.savefig("best_model_train_rmse_curves.png", dpi=150)

    # 2) Fold metric distributions
    plt.figure()
    x = np.arange(1, len(fold_metrics_df) + 1)
    plt.plot(x, fold_metrics_df["rmse"], marker="o")
    plt.xlabel("Fold")
    plt.ylabel("RMSE")
    plt.title("Per-fold RMSE (best model)")
    plt.tight_layout()
    plt.savefig("best_model_fold_rmse.png", dpi=150)

    plt.figure()
    plt.plot(x, fold_metrics_df["r2"], marker="o")
    plt.xlabel("Fold")
    plt.ylabel("R²")
    plt.title("Per-fold R² (best model)")
    plt.tight_layout()
    plt.savefig("best_model_fold_r2.png", dpi=150)

    # 3) Pred vs actual (OOF)
    plt.figure()
    plt.scatter(y, oof_pred, s=10)
    plt.xlabel("Actual percentage_liked")
    plt.ylabel("OOF predicted percentage_liked")
    plt.title("Predicted vs Actual (out-of-fold, best model)")
    plt.tight_layout()
    plt.savefig("best_model_pred_vs_actual.png", dpi=150)

    # 4) Residuals vs predicted (OOF)
    residuals = y - oof_pred
    plt.figure()
    plt.scatter(oof_pred, residuals, s=10)
    plt.axhline(0)
    plt.xlabel("OOF predicted percentage_liked")
    plt.ylabel("Residual (actual - predicted)")
    plt.title("Residuals vs Predicted (out-of-fold, best model)")
    plt.tight_layout()
    plt.savefig("best_model_residuals_vs_pred.png", dpi=150)

    # 5) Residual histogram
    plt.figure()
    plt.hist(residuals, bins=40)
    plt.xlabel("Residual (actual - predicted)")
    plt.ylabel("Count")
    plt.title("Residual distribution (out-of-fold, best model)")
    plt.tight_layout()
    plt.savefig("best_model_residual_hist.png", dpi=150)

    print("\nSaved plots:")
    for f in [
        "best_model_val_rmse_curves.png",
        "best_model_train_rmse_curves.png",
        "best_model_fold_rmse.png",
        "best_model_fold_r2.png",
        "best_model_pred_vs_actual.png",
        "best_model_residuals_vs_pred.png",
        "best_model_residual_hist.png",
    ]:
        print(" -", f)

    plt.show()

if __name__ == "__main__":
    main()


## Results
##Top 10 by lowest mean RMSE:
## max_depth  learning_rate  max_leaf_nodes  rmse_mean  rmse_std  r2_mean   r2_std
##       NaN           0.03              63  20.964651  0.203045 0.193977 0.008952
##       NaN           0.03             127  20.979014  0.203886 0.192869 0.009287
##       NaN           0.06              63  20.986771  0.193388 0.192285 0.007342
##       NaN           0.10              63  20.991201  0.207352 0.191928 0.009810
##       NaN           0.06             127  20.991572  0.226517 0.191905 0.010543
##       NaN           0.06              31  21.006810  0.208541 0.190735 0.009137
##       NaN           0.03              31  21.019843  0.215622 0.189735 0.009197
##       NaN           0.10              31  21.030812  0.198053 0.188881 0.008690
##       8.0           0.03              63  21.038200  0.192992 0.188317 0.007735
##       8.0           0.03              31  21.040251  0.204637 0.188159 0.008565