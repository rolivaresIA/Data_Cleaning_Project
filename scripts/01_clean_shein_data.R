#' ---
#' title: "Cleaning and transforming Shein product data with R"
#' output: github_document
#' ---
#'
#' # Cleaning and transforming Shein product data with R
#'
#' **Goal:** combine 21 raw category files of Shein products into **one consolidated,
#' trustworthy dataset**: consistent names and types, no meaningless columns, handled
#' missing values and no duplicated products.
#'
#' **Source:** Kaggle, [Dirty E-Commerce Data (80,000+ products)](https://www.kaggle.com/datasets/oleksiimartusiuk/e-commerce-data-shein/data).
#' The 21 CSV files are in `original_databases/` (one per product category).
#'
#' Required packages: `readr`, `dplyr`, `stringr`, `tidyr`, `naniar`, `ggplot2`, `writexl`.

#+ libraries, message=FALSE, warning=FALSE
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(naniar)
library(ggplot2)

#' ## 1. Load and combine the 21 files
#'
#' Importing the files one by one is slow and repetitive, so every file is read in a
#' loop. The category is taken from the file name (`us-shein-<category>-<4 digits>.csv`)
#' and stored in a new column so products can still be told apart after combining.

#+ load, message=FALSE, warning=FALSE
files <- list.files("original_databases", pattern = "\\.csv$", full.names = TRUE)

raw_list <- lapply(files, function(f) {
  df <- read_csv(f, show_col_types = FALSE)
  df$category <- str_replace(tools::file_path_sans_ext(basename(f)), "^us-shein-(.*)-\\d{4}$", "\\1")
  df[] <- lapply(df, as.character)   # the files disagree on types; harmonise before binding
  df
})
raw <- bind_rows(raw_list)
n_raw <- nrow(raw)

#' The consolidated raw table has **`r format(n_raw, big.mark = ",")` rows** and
#' **`r ncol(raw)` columns** from **`r length(files)` files**.
#'
#' ## 2. Standardise column names
#'
#' Some names use hyphens, some underscores, and some contain spaces. They are
#' normalised to `snake_case`.

names(raw) <- names(raw) %>% str_replace_all("-", "_") %>% str_replace_all(" ", "_") %>% str_replace_all("_+", "_")
names(raw)

#' ## 3. Fix variable types
#'
#' Everything was read as text. `price` ("$12.99", sometimes "1,299.00") and
#' `discount` ("-25%") must become numeric; `color_count` becomes numeric too. A
#' direct conversion would silently create `NA`s, so the problematic text is removed
#' first (currency symbol, thousands separators and percent sign).

base <- raw %>%
  mutate(
    price       = as.numeric(str_replace_all(price, "[$,]", "")),
    discount    = as.numeric(str_replace_all(discount, "%", "")),
    color_count = as.numeric(color_count)
  )

#' Missing values after fixing the types:

miss_var_summary(base)

#+ missing-plot, fig.width=9, fig.height=5
gg_miss_var(base) +
  labs(title = "Missing values per variable", x = "Variable", y = "Number of missing values") +
  theme_minimal()

#' ## 4. Decide what to do with missing values
#'
#' Several variables are almost empty. With more than ~60-90% missing, a variable
#' usually carries too little information to be worth keeping. The decisions:
#'
#' | Variable | Decision |
#' |---|---|
#' | `goods_title_link` (product name) | Fill gaps with `goods_title_link_jump` (an alternative field holding the same name) |
#' | `goods_title_link_jump`, `goods_title_link_jump_href`, `product_locatelabels_img_src`, `blackfridaybelts_bg_src`, `blackfridaybelts_content` | Drop (mostly empty or just URLs/images) |
#' | `rank_title` | Standardise ("#9 Best Seller" and "#9 Best Sellers" are the same) and label missing as `Unknown` |
#' | `rank_sub` | Drop (too many distinct values; `category` is cleaner) |
#' | `color_count` | Missing = 0 (assume no colour options listed) |
#' | `selling_proposition` | Missing = `No proposition` |
#' | `discount` | Missing = 0 (no discount) |

n_title_na_before <- sum(is.na(base$goods_title_link))

base <- base %>%
  mutate(goods_title_link = coalesce(goods_title_link, goods_title_link_jump)) %>%
  select(-goods_title_link_jump, -goods_title_link_jump_href, -product_locatelabels_img_src,
         -blackfridaybelts_bg_src, -blackfridaybelts_content, -rank_sub) %>%
  mutate(
    rank_title          = str_replace(rank_title, "Best Seller$", "Best Sellers"),
    rank_title          = replace_na(rank_title, "Unknown"),
    color_count         = replace_na(color_count, 0),
    selling_proposition = replace_na(selling_proposition, "No proposition"),
    discount            = replace_na(discount, 0)
  )

#' Missing product names went from **`r n_title_na_before`** to
#' **`r sum(is.na(base$goods_title_link))`** after using the alternative field.

miss_var_summary(base) %>% filter(n_miss > 0)

#' ## 5. Remove duplicated products
#'
#' The same product appears several times (the files overlap), and some appear with
#' **different prices**. The rule: keep **one row per product**; when prices differ,
#' keep the **highest price**.

dup_titles <- base %>% count(goods_title_link) %>% filter(n > 1, !is.na(goods_title_link))
price_conflicts <- base %>%
  filter(goods_title_link %in% dup_titles$goods_title_link) %>%
  group_by(goods_title_link) %>%
  summarise(n_prices = n_distinct(price), .groups = "drop") %>%
  filter(n_prices > 1)

#' * Products that appear more than once: **`r format(nrow(dup_titles), big.mark = ",")`**
#' * Of those, products with more than one price: **`r format(nrow(price_conflicts), big.mark = ",")`**

clean <- base %>%
  filter(!is.na(goods_title_link)) %>%
  arrange(goods_title_link, desc(price)) %>%
  distinct(goods_title_link, .keep_all = TRUE)

#' After removing duplicates and the product with no name at all, the clean dataset has
#' **`r format(nrow(clean), big.mark = ",")` unique products** (from
#' `r format(n_raw, big.mark = ",")` raw rows).

#' ## 6. Final names and order
#'
#' Columns are renamed and reordered so the exported file is easy to read for other people.

clean <- clean %>%
  rename(product_name = goods_title_link, discount_pct = discount, rank = rank_title) %>%
  select(product_name, category, price, discount_pct, color_count, rank, selling_proposition)

glimpse(clean)
miss_var_summary(clean)

#' ## 7. Export
#'
#' The clean dataset is saved to `final_database/shein_products_clean.xlsx`.

#+ export
dir.create("final_database", showWarnings = FALSE)
writexl::write_xlsx(clean, "final_database/shein_products_clean.xlsx")
