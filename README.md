# ARTIS Model (Aquatic Resource Trade In Species)

This repository contains the ARTIS model codebase and instructions to run the model. ARTIS reconstructs global seafood supply chains by tracing trade flows and production data through a multi-stage allocation process. It enables detailed analysis of seafood consumption by country, species, and product form.

## What’s New in v1.1.0

- Major overhaul of the consumption calculation workflow (see below for high-level summary)
- For all changes see [CHANGELOG](./CHANGELOG.md) for details

---

### High‐Level Changes in `calculate_consumption.R`

2. **Domestic Consumption (Stage 1)**  
   - **Domestic Production → Domestic Exports → Domestic Consumption**  
     1. Aggregate total production of each HS‐6 code (disaggregated from species using `X_long`).  
     2. Subtract “domestic exports” (i.e., what leaves each country under `dom_source == "domestic"`).  
     3. Warn if any country’s exports exceed production.  
   - Result: a per‐country, per‐HS‐6 volume of product that remains for domestic consumption.  

3. **Foreign Consumption (Stage 2)**  
   - **Unprocessed Imports → Processed Exports → Two‐Stage Allocation**  
     1. **Reverse “processing”**: Using `reweight_W_long`, the code “unprocesses” the final‐product HS‐6 back to its original raw‐fish equivalents, creating a pool of what could be consumed or re‐exported.  
     2. **Stage 1 (Import Retention)**: For each importer, estimate how much of the processed product they consume immediately (i.e., “fishmeal,” “other,” or “direct human consumption”). Any remainder becomes a re‐export candidate.  
     3. **Stage 2 (Redistribution to Final Consumer)**: Take those re‐exports and allocate them to the next‐tier consumers based on trade proportions (`artis` → “foreign” flows).  
     4. Throughout, apply the same Data‐Check logic: ensure that “foreign consumption + domestic consumption ≈ total production + error exports”.

4. **Per‐Capita Capping & Debug Mode**  
   - After fully assembling **domestic + foreign consumption**, the function now:  
     1. Joins in population data (`pop`) to calculate per‐capita seafood consumption for “direct human consumption.”  
     2. Identifies outliers (e.g., any country > 100 kg/person) and proportionally “caps” their total consumption back down to the threshold.  
     3. If `dev_mode = TRUE`, writes out a CSV of the largest consumption‐vs.‐production discrepancies so users can inspect and debug (as described under the manual’s “Data Quality & Diagnostics” section).

5. **Smaller, Faster I/O with `.qs` Files**  
   - All intermediate tables (e.g., disaggregated species‐to‐HS volumes, processed/unprocessed flows, final consumption tables) are now serialized as `.qs` rather than RDS/CSV, dramatically reducing file size and read/write time.

6. **Improved Error‐Handling & Data Checks**  
   - **Domestic Check**: Warn if any “domestic export” volume exceeds production (i.e., negative domestic consumption).  
   - **Foreign Check**: Compare ARTIS’s reported “domestic export” volumes against the function’s computed values—warn if they diverge by more than 1 ton.  
   - **NA/Negative Consumption**: Emit a clear warning if any final consumption records are NA or negative, matching the manual’s emphasis on internal consistency checks at each stage.

---

## Model Overview

ARTIS reconstructs seafood supply chains by:

- Integrating production data, international trade flows, and processing factors.
- Disaggregating national production to detailed product codes using trade proportions.
- Tracing each product through exports, imports, processing, and consumption pathways.
- Providing per-country, per-species, and per-product estimates of seafood availability and use.

For full conceptual diagrams and methods, see the [ARTIS Manual](https://seafood-globalization-lab.github.io/artis-manual/).

---

## Run Modes

- **local**: Full model run on your local machine. Computes all HS versions/years as specified.  
  _Requires significant compute resources and is developed/tested on macOS with ARM64 (Apple Silicon) architecture._
- **demo**: Fast, small test dataset for local runs and troubleshooting. (has not been maintained or checked recently) 
- **aws**: Large-scale cloud runs on AWS Batch. See [`artis-hpc`](https://github.com/Seafood-Globalization-Lab/artis-hpc) for details.

## Installation

### Prerequisites

- Python 3.11.x ([Download](https://www.python.org/downloads/release/python-3110/))
- R (tested with R 4.2.2) ([Download](https://www.r-project.org/))
- RStudio ([Download](https://posit.co/download/rstudio-desktop/))

### Python Environment

```
python3 -m venv /path/to/your/venv --without-scm-ignore-files
source venv/bin/activate
pip install -r requirements.txt
pip list  # confirm: qpsolvers, quadprog, cvxopt
```

### R Package Installation

- Open the project in RStudio.
- Use the Build tab to "Clean and Install" the package (see image in `/images/artis_r_build_config_options.png`).

---

## Development Workflow

### Branch Structure

- `main`: Stable releases
- `develop`: Ongoing development
- Task branches: `develop-*` (short-lived, merged back to `develop`)
- Hotfixes: branch from `main` for urgent fixes, merged back to `main`

### Branch Workflow Diagram

```
gitGraph
   commit id: "v1.0"
   branch develop
   commit
   branch develop-ingest-new-data-v2
   commit id: "clean FAO"
   commit id: "resolve sciname"
   checkout develop
   merge develop-ingest-new-data-v2 id: "merge cleaning script"
   checkout develop
   commit id: "update README"
   branch develop-fix-bug
   commit
   checkout develop
   merge develop-fix-bug id: "merge fix-bug"
   commit id: "add documentation"
   checkout main
   merge develop id: "v2.0 Release"
   branch hot-fix
   checkout hot-fix
   commit id: "forgot this tiny thing"
   checkout main
   merge hot-fix id: "v2.0.1 Release"
```

---

## System Requirements

> **Note:** See `requirements.txt` for Python package versions. R package versions still require additional documentation.

- **Platform:** macOS Ventura 13.3.1 (ARM64/M1/M2 strongly recommended)
- **R version:** 4.2.2
- **Python version:** 3.11.x
- **Key R packages:** data.table, dplyr, stringr, tidyverse, reticulate, etc. See `.renv_lock` file for package version details
- **Key Python packages:** qpsolvers, quadprog, cvxopt

---

## Citation

> A formal dataset DOI will be posted here after the v1.1.0 release.  
> For now, cite the software as:

```
Jessica Gephart, Rahul Agrawal Bejarano, Althea Marks, & Kelvin Gorospe. (2024).
ARTIS input data and model. Knowledge Network for Biocomplexity. doi:10.5063/F1862DXT.
```

```bibtex
@software{artis-v1.1.0,
  title        = {ARTIS Model (Aquatic Resource Trade In Species), v1.1.0},
  author       = {Gephart, Jessica and Agrawal Bejarano, Rahul and Marks, Althea and Gorospe, Kelvin},
  year         = {2025},
  version      = {1.1.0},
  url          = {https://github.com/Seafood-Globalization-Lab/artis-model},
  note         = {Accessed: yyyy-mm-XX},
  institution  = {University of Washington},
  organization = {Seafood Globalization Lab},
  howpublished = {GitHub repository}
}
```

---

## More Information

- [ARTIS Manual](https://seafood-globalization-lab.github.io/artis-manual/): Conceptual background, methods, output structure, data access.
- [CHANGELOG](./CHANGELOG.md): Complete change history.

## Model Visual Schematic

The following diagrams illustrate the core logic and processing steps of the ARTIS model:

- **Disaggregating Trade Records**:  
  ![Disaggregating Trade Records](./images/disaggregating_trade_records.png)  
  _Shows how national-level production is mapped onto detailed product (HS) codes using trade information._

- **Aggregating Trade Records Back Up**:  
  ![Aggregating Trade Records Back Up](./images/building_trade_records_back_up.png)  
  _Demonstrates how disaggregated product flows are traced and summed back up to reconstruct consumption and trade balances._

- **Consumption Workflow**:  
  ![Consumption Workflow](./images/consumption_workflow.png)  
  _Depicts the multi-stage allocation process: from production and trade through to final consumption estimates._

- **Codebase Structure Diagrams**:  
  ![Cleaning data diagram](./images/model_inputs_creation.png)  
  ![Mass balance solutions](./images/country_mass_balance_solution_creation.png)  
  ![Creating ARTIS codeflow](./images/create_artis_codeflow.png)  
  _Visuals of the ARTIS codebase organization and major workflow steps._

---
