# Indirect effect of influenza vaccination coverage on the circulation of respiratory viruses

Replication code and supporting data for the manuscript "Indirect effect of influenza vaccination coverage on the circulation of respiratory viruses".

## Directory Structure

- `00_data/` — Input data.
- `01_Fig1/` — Code for Figure 1.
- `02_Fig2_4/` — Code for Figures 2–4, including Empirical Dynamic Modeling (EDM) analyses.
  - `rEDM_01_vaccine_precovid/` — EDM analysis for the pre-COVID-19 pandemic period.
  - `rEDM_02_vaccine_postcovid/` — EDM analysis for the post-COVID-19 pandemic period.
- `03_Fig5/` — Code for Figure 5, including Integrated Nested Laplace Approximation (INLA) and Distributed Lag Nonlinear Model (DLNM) analyses.
- `04_Extended_analysis/` — Code for supplementary analyses.

## Data Preparation

Before running the analyses, copy the following CSV files from `00_data/` into the corresponding analysis-specific folders. Keep the source files in `00_data/` and preserve all filenames.

| Analysis | Destination folder | Files |
| --- | --- | --- |
| Figures 2–4: Pre-COVID-19 | `02_Fig2_4/rEDM_01_vaccine_precovid/01_data/` | `seasonal_flu_coverage_average.csv`, `multi_vaccine_indirect_precovid19.csv` |
| Figures 2–4: Post-COVID-19 | `02_Fig2_4/rEDM_02_vaccine_postcovid/01_data/` | `seasonal_flu_coverage_average.csv`, `multi_vaccine_indirect_postcovid19.csv` |
| Figure 5 | `03_Fig5/01_data/` | `multi_vaccine_indirect_precovid19.csv`, `multi_vaccine_indirect_postcovid19.csv` |

## Software and Required Packages

All analyses were conducted in R (version 4.5.2). The main packages used were:

- `rEDM` (version 1.14.3) for EDM analyses.
- `INLA` (version 25.10.19), `dlnm` (version 2.4.10), and `splines` for INLA-DLNM analyses.

## Running the Analyses

### Figures 2–4

For the pre- or post-COVID-19 period, open the corresponding folder under `02_Fig2_4/` and run `03_main.Rmd`, which calls the analysis scripts.

Alternatively, run the following commands in a terminal from that folder to execute `03_main_temp.Rmd`:

```bash
chmod +x run_rEDM_script2.csh
./run_rEDM_script2.csh
```

EDM analyses can take substantial time. To reproduce the figures without rerunning the analyses, use the precomputed results in each project's `02_output/` folder and run the plotting scripts below.

#### Visualization of EDM results

| Figures | Pre-COVID-19 | Post-COVID-19 |
| --- | --- | --- |
| Figure 2 and Figures S12–S13 | `11_plot_Fig2_S12_S13_precovid.Rmd` | `11_plot_Fig2_FigS12_S13_postcovid.Rmd` |
| Figure 3 and Figure S14 | `12_plot_Fig3A_S14_precovid.Rmd` | `12_plot_Fig3B_FigS14_postcovid.Rmd` |
| Figure 4 and Figure S15 | `13_plot_Fig4A_S15_precovid.Rmd` | `13_plot_Fig4B_S15_postcovid.Rmd` |
| Figure S16 | `14_plot_FigS16_precovid.Rmd` | `14_plot_FigS16_post_covid.Rmd` |

### Figure 5

For the pre- or post-COVID-19 period, set the working directory to `03_Fig5/` and run the corresponding scripts:

| Analysis | Pre-COVID-19 | Post-COVID-19 |
| --- | --- | --- |
| Fit models | `01_pre_run_models_IAV.R` | `01_post_run_models_IAV.R` |
| Save model outputs for plotting | `02_pre_plot_output_IAV.R` | `02_post_plot_output_IAV.R` |
| Cross-validation | `0x_run_pre_leave_month_out.R` | `0x_run_post_leave_month_out.R` |

INLA-DLNM analyses can take substantial time. To reproduce the figures without rerunning the analyses, use the precomputed results and run the plotting scripts below.

#### Visualization of INLA-DLNM results

The following scripts generate figures for both the pre- and post-COVID-19 periods.

| Figures | Script |
| --- | --- |
| Figure 5 | `03_plot_Fig5.Rmd` |
| Figure S17 | `04_plot_FIgS17.Rmd` |
| Figure S18 | `05_plot_FigS18.Rmd` |
| Figure S19 | `06_plot_FigS19.Rmd` |
