# Check Variable Correlation and Multi-collinearity

\`corr_check()\` is an optional function for the \`envar\` package
workflow. It allows you to check for variable correlation and
multi-collinearity among the variables over the study area. It can be
used before or after \`extr_check()\`.

## Usage

``` r
corr_check(x, pearson = NULL, vif = NULL)
```

## Arguments

- x:

  A \`SpatRaster\`, \`data.frame\`, or a list containing \`data\` or
  \`extracted_df\` (e.g., output from \`extr_check()\`).

- pearson:

  Numeric or \`NULL\`. Threshold for the absolute Pearson correlation
  coefficient above which variables are flagged with a warning. By
  default (\`NULL\`) a default threshold of \`0.7\` is used; supply a
  value (e.g. \`0.6\`) to be warned about variable pairs whose absolute
  correlation exceeds it instead.

- vif:

  Numeric or \`NULL\`. Threshold for the Variance Inflation Factor above
  which variables are flagged with a warning. By default (\`NULL\`) a
  default threshold of \`3\` is used; supply a value (e.g. \`5\`) to be
  warned about variables whose VIF exceeds it instead.

## Value

A \`list\` object containing:

- \`data\`: The input environmental data used.

- \`correlation_matrix\`: Pearson correlation matrix.

- \`vif\`: Variance Inflation Factor data frame.

- \`summary\`: Character vector highlighting high correlation or VIF,
  based on the supplied thresholds (or the defaults of \`0.7\` and
  \`3\`).

- \`plot_path\`: Path to the saved correlation plot.

- \`vif_path\`: Path to the saved VIF table.

- Any additional elements from input list (e.g., \`extrapolation\` from
  \`extr_check()\`).

## Details

**Citation:**  
Wei T, Simko V (2021). "R package 'corrplot': Visualization of a
Correlation Matrix." GitHub. https://github.com/taiyun/corrplot

Naimi B, Hamm NA, Groen TA, Skidmore AK, Toxopeus AG (2014). "Where is
positional uncertainty a problem for species distribution modelling?"
Ecography 37, 191-203. https://doi.org/10.1111/j.1600-0587.2013.00205.x

Regardless of whether \`pearson\`/\`vif\` thresholds are set, the
function always writes two files to the current working directory: the
correlation plot (\`Corr_plot.png\`) and a table of VIF values
(\`VIF_table.csv\`). Their paths are returned as \`plot_path\` and
\`vif_path\`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Basic usage after environmental variable extraction
processed_bilayer_corr_check <- par_set(country = "Italy", crs=3035, buffer = 10) %>% 
  melc(vars=c("ice")) %>% 
  chelsa(vars=c("pr"), months= 12, year=2015) %>% 
  corr_check()

# Example 2: Chain with extr_check() (corr_check before extr_check)
result <- par_set(country = "Italy") %>% 
  chelsa(vars = c("bio1", "bio12")) %>% 
  corr_check() %>%
  extr_check(calib_points = my_points)

# Example 3: Chain with extr_check() (extr_check before corr_check)
result <- par_set(country = "Italy") %>%
  chelsa(vars = c("bio1", "bio12")) %>%
  extr_check(calib_points = my_points) %>%
  corr_check()

# Example 4: Custom thresholds for high correlation (>0.7) and VIF (>5)
result <- par_set(country = "Italy") %>%
  chelsa(vars = c("bio1", "bio12")) %>%
  corr_check(pearson = 0.7, vif = 5)
} # }
```
