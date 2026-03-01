Gradient Boosted Decision Trees are an ensemble learning method that builds a model as a sequence of decision trees, where each new tree is trained to correct the errors of the previous ones. Instead of fitting one complex model, GBDT incrementally improves predictions by minimizing a loss function using gradient descent. Each tree contributes a small adjustment to the overall prediction, controlled by a learning rate, allowing the model to capture complex nonlinear relationships. 

We selected GBDT because it is particularly well-suited for tabular datasets with mixed feature types and nonlinear relationships. Our dataset contains both continuous features and a large number of one-hot encoded categorical features. Tree-based models naturally handle such data without requiring feature scaling or manual interaction engineering, making them more effective than linear models like the linear regression method we attempted in part 3. 

We used 5-fold cross-validation to evaluate different GBDT configurations. In this process, the dataset is split into five parts, and each model is trained and evaluated five times using different train/test splits. The results are averaged to obtain a stable estimate of performance.

We were comparing different hyperparameter configurations, specifically:

learning_rate (how much each tree contributes)

max_leaf_nodes (controls model complexity)

max_depth (limits tree depth, or left unrestricted)

For each configuration, we measured:

RMSE (Root Mean Squared Error)  (lower is better)

R² (coefficient of determination)   (higher is better)