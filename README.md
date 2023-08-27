# TestTrek: Statistical Test Recommender

![TestTrek Logo](man/figures/logo.png)

`TestTrek` is an R package designed to help researchers and data analysts choose the most appropriate statistical test for their data. Based on the nature of your variables and your dataset's characteristics, `TestTrek` provides recommendations for statistical tests, effect sizes, and post hoc tests when necessary.

## Features

- **Test Recommendations**: Suggests the most suitable statistical test based on the type of data (nominal, ordinal, continuous, etc.) and the relationship between variables.
- **Effect Size Calculations**: Recommends the most appropriate effect size metric for the given data.
- **Post Hoc Test Guidance**: For certain tests like ANOVA, recommends suitable post hoc tests.

## License

This project is licensed under GPL-3

## Installation

You can install the development version of `TestTrek` from GitHub using `devtools`:

```R
# If you don't have devtools installed yet, install it first
install.packages("devtools")

# Install TestTrek from GitHub
devtools::install_github("giovannibriganti/TestTrek")


