# mditools

**Microdata Infrastructure Tools: Analysis Tools for Firm-Level Microdata Research**

`mditools` supports the full analysis pipeline for researchers working with firm-level microdata.

Start with the **data tools** to prepare your panel: import raw files, detect outliers, and harmonize classifications over time. Then run your **analysis** — estimate production functions and capital stock, compute markups, intensity measures, and distributions, or run regressions and clustering. Once results are ready, use the **disclosure tools** to tag outputs with dominance and observation counts, aggregate to industry or country level, and apply suppression rules before publication.

## Installation

Once on CRAN:

```r
install.packages("mditools")
```

Development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("Secretariat-CompNet/mditools")
```

## Main features

| Area | Functions |
|---|---|
| Data tools | `mdi_import_data()`, `mdi_outlier()`, `mdi_make_conc()` |
| Aggregation | `mdi_aggregate()`, `mdi_hier_apply()` |
| Disclosure control | `mdi_disclose_crit()`, `mdi_disclose_reg_tab()` |
| Production functions | `mdi_estimate_prodfun()`, `mdi_acf_prodest()`, `mdi_lp_prodest()`, `mdi_ols_prodest()`, `mdi_wdrg_prodest()`, `mdi_cs_prodest()`, `mdi_dpgmm_prodest()` |
| Analysis functions | `mdi_regress()`, `mdi_clustering()`, `mdi_estimate_markup()`, `mdi_pim_capital()`, `mdi_intensity()`, `mdi_jointdist()`, `mdi_transition()` |


## Usage example

```r
library(mditools)
library(data.table)

DT <- data.table(
  firmid = rep(1:10, each = 2),
  year   = rep(2020:2021, 10),
  nace   = rep(c("A", "B"), 10),
  emp    = sample(10:100, 20)
)

# Aggregate employment by industry, with disclosure criteria
agg <- mdi_aggregate(DT, var_list = "emp", bygroups = c("nace", "year"),
                     agg_type = "sum", disclosure = TRUE)

# Check disclosure criteria (dominance and observation counts)
disc <- mdi_disclose_crit(agg, domVar = "var", domNr = 2L,
                          bygroups = c("nace", "year"), var_list = "emp")
```

## Country-specific usage

`mditools` is designed to run the same research code across multiple NSI environments
without modification. Country-specific disclosure rules are passed as explicit
function arguments rather than being hardcoded inside the package.

The recommended pattern is to set `CountryCode` once at the top of each script,
derive all country-specific parameters from it, and then keep the rest of the
script identical across deployments:

```r
# Set once per deployment — this is the only line that changes between countries
CountryCode <- "DE"

# Derive all country-specific parameters from CountryCode
minNumObs     <- if (CountryCode == "DE") 10L else 5L
domSh         <- if (CountryCode == "DE") 0.70 else 0.85
dom_direction <- if (CountryCode == "DE") "low" else "high"
dom_formula   <- if (CountryCode == "DE") "residual" else "top_share"
disc_method   <- if (CountryCode == "DE") "firm_count" else "obs_df"
count_firms   <- CountryCode %in% c("DE", "DEt")

# The rest of the script is identical across all countries
mdi_disclose_crit(DT, domVar = "var", domNr = 2L, bygroups = c("nace", "year"),
                  var_list = "emp", dom_formula = dom_formula,
                  count_firms = count_firms)

mdi_disclose_reg_tab(reg_output, min_obs = minNumObs,
                     disc_method = disc_method)

mdi_regress(DT, formula = "y ~ x1 + x2", minNumObs = minNumObs,
            count_firms = count_firms)
```

Any parameter that varies by country should follow the same pattern: derive it
from `CountryCode` once at the top, pass it explicitly to each function.
The exact thresholds (`minNumObs`, `domSh`, etc.) depend on your NSI's disclosure
rules — the values above are illustrative only.

The disclosure-specific parameters and their roles are:

| Parameter | Function | What it controls |
|---|---|---|
| `dom_formula` | `mdi_disclose_crit()` | Dominance formula: `"top_share"` or `"residual"` |
| `count_firms` | `mdi_disclose_crit()`, `mdi_regress()` | Whether to compute and report `NumFirms`/`NumEnt` |
| `disc_method` | `mdi_disclose_reg_tab()` | Whether to check obs/df or firm/enterprise counts |

## License

GPL-3.