library(readr)
library(tidyverse)
library(ggplot2)

store_data <- readr::read_csv('./data/steam.csv')
requirements_data <- readr::read_csv('./data/steam_requirements_data.csv') %>% 
  rename("appid"="steam_appid")

steam_data <- inner_join(steam_data, requirements_data, by = "appid")

data <- steam_data %>%
  mutate(total_ratings=(positive_ratings+negative_ratings)) %>% 
  filter(total_ratings>500) %>% 
  mutate(percent_positive=(100*positive_ratings/total_ratings))

one_hot_multilabel <- function(df, col, id_col = "appid") {
  col_name <- rlang::as_name(rlang::enquo(col))
  
  encoded <- df %>%
    select(all_of(id_col), {{ col }}) %>%
    separate_rows({{ col }}, sep = ";") %>%
    mutate(
      value = 1,
      prefixed_name = paste0(col_name, "_", trimws({{ col }}))
    ) %>%
    group_by(across(all_of(id_col)), prefixed_name) %>%
    summarize(value = 1, .groups = "drop") %>%
    pivot_wider(
      names_from = prefixed_name,
      values_from = value,
      values_fill = 0
    )
  
  df %>%
    left_join(encoded, by = id_col)
}

wide_data <- steam_data %>% 
  separate(owners, into = c("low", "high"), sep = "-", convert = TRUE) %>%
  one_hot_multilabel(platforms) %>%
  one_hot_multilabel(genres) %>%
  one_hot_multilabel(categories) %>%
  one_hot_multilabel(steamspy_tags)

owner_cuts <- sort(unique(wide_data$low))
owner_vals <- 1:length(owner_cuts)

final_data <- wide_data %>% 
  select(-high) %>%
  mutate(owners_ord = owner_vals[match(low, owner_cuts)]) %>% 
  select(-low)

final_data %>% write_csv("./data/project_data.csv")

df <- final_data

df <- readr::read_csv("./data/output.csv", show_col_types = FALSE) %>% clean_names()

plots_dir <- "plots"
if (!dir.exists(plots_dir)) dir.create(plots_dir)

create_boxplot <- function(data, column, value_col = "percentage_liked",
                           plot_title = NULL, filename = NULL,
                           min_count = 0,  # <-- new argument
                           out_width = 10, out_height = 8, out_dpi = 150) {
  
  plot_df <- data %>%
    select(all_of(value_col), all_of(column)) %>%
    filter(!is.na(.data[[column]])) %>%
    mutate(temp_col = str_replace_all(.data[[column]], "\\s+", " ")) %>%
    separate_rows(temp_col, sep = ";") %>%
    mutate(temp_col = str_trim(temp_col)) %>%
    filter(temp_col != "")
  
  if (min_count > 0) {
    counts <- plot_df %>% count(temp_col)
    keep_values <- counts %>% filter(n >= min_count) %>% pull(temp_col)
    plot_df <- plot_df %>% filter(temp_col %in% keep_values)
  }
  
  p <- ggplot(plot_df,
              aes(x = fct_reorder(temp_col, .data[[value_col]], .fun = median),
                  y = .data[[value_col]])) +
    geom_boxplot(outlier.alpha = 0.25) +
    coord_flip() +
    labs(
      title = plot_title %||% paste("Percentage Liked by", column),
      x = NULL,
      y = "% liked"
    ) +
    theme_minimal()
  
  if (!is.null(filename)) {
    ggsave(filename = file.path(plots_dir, filename),
           plot = p, width = out_width, height = out_height, dpi = out_dpi)
  }
  
  message("Wrote: ", plots_dir, "/", filename)
  
  return(p)
}

save_plot <- function(plot_obj, filename, width = 10, height = 6) {
  full <- file.path(plots_dir, filename)
  ggsave(full, plot = plot_obj, width = width, height = height, dpi = 150)
  message("Wrote: ", full)
}

plot_numeric_scatter <- function(col, suffix = "", smooth = FALSE) {
  fname_lin <- file.path(plots_dir, paste0("numeric_", col, suffix, "_linear.png"))
  p_lin <- ggplot(df, aes_string(x = col, y = "percentage_liked")) + geom_point(alpha = 0.5, size = 1) + labs(title = paste0("percentage_liked vs ", col), x = col, y = "% liked") + theme_minimal()
  if (smooth) p_lin <- p_lin + geom_smooth(method = smooth_method, se = TRUE)
  save_plot(p_lin, paste0("numeric_", col, suffix, "_linear.png"))
  
  vec <- df[[col]]
  min_pos <- min(vec[vec > 0], na.rm = TRUE)
  offset <- ifelse(min_pos > 0, 0, log_offset)
  tmp <- df %>% mutate(!!sym(col) := ifelse((!!sym(col)) <= 0, (!!sym(col)) + offset + min_pos, (!!sym(col))))
  if (min(tmp[[col]], na.rm = TRUE) > 0) {
    p_log <- ggplot(tmp, aes_string(x = col, y = "percentage_liked")) + geom_point(alpha = 0.5, size = 1) + scale_x_log10(labels = comma_format()) + labs(title = paste0("percentage_liked vs (log) ", col), x = paste0(col, " (log10 scale)"), y = "% liked") + theme_minimal()
    if (smooth) p_log <- p_log + geom_smooth(method = smooth_method, se = TRUE)
    save_plot(p_log, paste0("numeric_", col, suffix, "_log10.png"))
  }
}


# ---- Create & save plots ----
create_boxplot(df, "genres", plot_title = "Percentage Liked by Genre", filename = "genres_boxplot.png")
create_boxplot(df, "categories", plot_title = "Percentage Liked by Category", filename = "categories_boxplot.png")
create_boxplot(df, "steamspy_tags", plot_title = "Percentage Liked by Tag", filename = "tags_boxplot.png", out_height=40, min_count=3)
create_boxplot(df, "developer", plot_title = "Percentage Liked by Developer", filename = "developer_boxplot_all.png", out_height=40, min_count=10)
create_boxplot(df, "developer", plot_title = "Percentage Liked by Developer", filename = "developer_boxplot_big.png", min_count=22)
create_boxplot(df, "publisher", plot_title = "Percentage Liked by Publisher", filename = "publisher_boxplot_all.png", out_height=40, min_count=10)
create_boxplot(df, "publisher", plot_title = "Percentage Liked by Publisher", filename = "publisher_boxplot_big.png", min_count=40)
create_boxplot(df, "required_age", plot_title = "Percentage Liked by Age Rating", filename = "platforms_agerating.png")
plot_numeric_scatter("achievements")
plot_numeric_scatter("price")
