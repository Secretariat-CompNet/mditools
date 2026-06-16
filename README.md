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

## License

GPL-3.