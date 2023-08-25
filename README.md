# TestTrek: Statistical Test Recommender

![TestTrek Logo](path_to_logo.png) *Optional: If you have a logo, replace `path_to_logo.png` with its path.*

`TestTrek` is an R package designed to help researchers and data analysts choose the most appropriate statistical test for their data. Based on the nature of your variables and your dataset's characteristics, `TestTrek` provides recommendations for statistical tests, effect sizes, and post hoc tests when necessary.

## Features

- **Test Recommendations**: Suggests the most suitable statistical test based on the type of data (nominal, ordinal, continuous, etc.) and the relationship between variables.
- **Effect Size Calculations**: Recommends the most appropriate effect size metric for the given data.
- **Post Hoc Test Guidance**: For certain tests like ANOVA, recommends suitable post hoc tests.

## Installation

You can install the development version of `TestTrek` from GitHub using `devtools`:

```R
# If you don't have devtools installed yet, install it first
install.packages("devtools")

# Install TestTrek from GitHub
devtools::install_github("giovannibriganti/TestTrek")
