# PERiskProfileR

Please note: This package is *NOT* intended for clinical use and the user is responsible for any use.

## Installation

```
if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")

devtools::install_github("andjar/PERiskProfileR", ref = "main")
```

## Short demo

The `calculate_pe_risk()` function will return the following values

* MoM for UtPI
* MoM for PlGF
* MoM for MAP
* Risk score based on history
* Combined risk score

```
# Load library
library("PERiskProfileR")

# Load demo data
df <- get_demo_data()

# Calculate risk scores locally
result <- calculate_pe_risk(df, method = "formula")

# Calculate risk scores online. This make take some time, and note that data will be sent to the FMF web site
result <- calculate_pe_risk(df, method = "online")
```
