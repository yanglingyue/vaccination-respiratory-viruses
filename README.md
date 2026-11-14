# Indirect effect of influenza vaccination coverage on the circulation of respiratory viruses

Replication code and supporting data for the manuscript "Indirect effect of influenza vaccination coverage on the circulation of respiratory viruses".

## Directory Structure

- `00_data/` — Input data.
- `01_Fig1/` — Code for Figure 1.
- `02_Fig2_4/` — Code for Figures 2–4, including Empirical Dynamic Modeling (EDM) analyses.
- `03_Fig5/` — Code for Figure 5, including Integrated Nested Laplace Approximation (INLA) and Distributed Lag Nonlinear Model (DLNM) analyses.
- `04_Extended_analysis/` — Code for supplementary analyses.

## Software and Required Packages

All analyses were conducted in R (version 4.5.2). The main packages used were:

- `rEDM` (version 1.14.3) for EDM analyses.
- `INLA` (version 25.10.19), `dlnm` (version 2.4.10), and `splines` for INLA-DLNM analyses.
