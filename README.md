# HazardGraph

HazardGraph is an R pipeline for mapping environmental hazards and critical infrastructure near industrial facilities in North Carolina. It began as a Mecklenburg County prototype and was expanded statewide, using Hurricane Ian (2022) as the initial storm case study.

The pipeline uses [`targets`](https://docs.ropensci.org/targets/) to track dependencies, cache completed steps, and rerun only the parts affected by code or data changes.

## Approach

1. Obtain and clean environmental, facility, land-cover, and storm datasets.
2. Assign records to North Carolina counties using coordinates, spatial boundaries, and FIPS codes instead of county-name matching.
3. Create 300 m, 500 m, and 1,000 m buffers around EPA Toxics Release Inventory (TRI) facilities.
4. Calculate nearby hazard and infrastructure features for each facility-buffer pair.
5. Integrate the validated features into one statewide dataset and run quality checks.

The main unit of analysis is **one TRI facility at one buffer distance**.

## Data layers

- **Industrial chemicals:** EPA TRI; Risk Management Program (RMP) facilities
- **Land and environmental sites:** USDA Cropland Data Layer (CDL), USGS Mineral Resources Data System (MRDS), brownfields, Superfund sites
- **Utilities and infrastructure:** wastewater facilities, electric substations, hospitals
- **Agriculture:** livestock and animal-feeding operations
- **Hurricanes:** NOAA storm-track data, initially Hurricane Ian (2022)

Data availability and reference years differ by source. Current facility locations should not be interpreted automatically as verified 2022 conditions. MRDS records represent mineral-resource or deposit records, not necessarily active mines.

## Pipeline organization

| Files | Purpose |
| --- | --- |
| `_targets.R` | Defines the pipeline and dependencies |
| `00_config.R` and early numbered scripts | Configuration, data acquisition, processing, and validation |
| Intermediate numbered scripts | Mecklenburg prototype, source integrations, statewide source QA |
| `41_build_statewide_hazardgraph.R` | Combines validated source features across North Carolina |
| `42_hazardgraph_analysis.R` | Planned analysis, comparison, scoring, and figures |

The pipeline stores data and outputs in Google Drive under:

`Science_Fair/ScienceFairProject2027/HazardGraph/`

Working folders include `raw/`, `processed/`, `metadata/`, `validation/`, `figures/`, and `Amadeus_Cloud_Downloads/`. Large datasets, authentication files, and local `targets` caches do not belong in Git.

## Running the pipeline

The current workflow runs in an R Google Colab session with access to the project's Google Drive folder and required R packages.

1. Open an R runtime, restore or install the required packages, and authenticate Google Drive.
2. Set the working directory to the folder containing `_targets.R`.
3. Run:

```r
library(targets)
tar_make(callr_function = NULL)
```

`callr_function = NULL` keeps execution in the authenticated Colab R session. The pipeline relies on existing source files and configured Drive paths; a fresh environment requires those inputs and dependencies to be available.

To inspect the pipeline status:

```r
tar_outdated()
tar_meta()
```

## Current results

The statewide integration has passed its reported QA checks:

| Check | Result |
| --- | ---: |
| North Carolina TRI facilities | 776 |
| Buffer distances | 300 m, 500 m, 1,000 m |
| Expected facility-buffer rows | 2,328 |
| Integrated rows | 2,328 |
| Duplicate facility-buffer rows | 0 |

The primary statewide output is:

`nc_hazardgraph_features_2022.csv`

The earlier Mecklenburg prototype contains 64 TRI facilities across three buffers (192 rows).

## Work in progress

- Complete and verify statewide CDL feature integration.
- Refine hurricane wind-field calculations using HURDAT2 wind-radius information, beyond the initial distance-based Ian screening.
- Complete Step 42: compare exposed and unexposed facilities, inspect feature distributions, evaluate potential scoring methods, and test sensitivity to buffer size and assumptions.

**Scope:** HazardGraph currently provides a spatial screening dataset. Proximity to a hazard source does not by itself establish chemical exposure, storm damage, or health risk.
