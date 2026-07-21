# Applied Data Science Project

## Overview

This repository is a **showcase** for some selected workflows in Python and R. Its use case is one part of a broader project on service provision upon parliamentary requests in Zimbabwe.

It is a **structured collection of data science workflows** combining LLM-based text classification, spatial data processing, and causal inference methods.

## Research Question

Do parliamentary service requests improve provision of local public goods in a Member of Parliament's (MP) constituency?

The focus of this repository is upon the provision of health facilities (clinics or hospitals).

## Workflow

1. Parliamentary questions are classified using an LLM via API prompting (GPT4.1-mini).
2. The classified data are merged with identifiers for MPs and their constituencies, addressed ministers plus rich background information on MPs, ministers, and constituencies (e.g., electoral results, political affiliation, committee membership, govt portfolio, a district's distance to the capital of Harare, presence of historical missions and other spatial data, etc.).
3. The dataset is balanced and aggregated at the constituency-month level (N=210, T=96).
4. Difference-in-differences, matching methods (included in the DiD script), and augmented synthetic controls are used to analyze the final dataset.
5. Figures and tables are generated to document the overall quality of the research design, average treatment effects of the treated (ATT) in event study plots, long-term effects via coefficient plots, placebo tests, and robustness checks.

## Project Structure

```text
applied-ds-workflow/

├── data/
│   └── Raw and processed datasets (not shown)
│
├── docs/
│   └── Project documentation & some selected figures
│
├── results/
│   └── Generated figures and outputs (not shown)
│
└── src/
    │
    ├── data_processing/
    │   └── R script for data preparation (not shown)
    │
    ├── llm/
    │   └── Python scripts for API-based text classification
    │
    ├── spatial/
    │   └── Python scripts for spatial data processing
    │
    ├── visualization/
    │   └── Python script for a map of the location of clinics
    │
    └── analysis/
        │
        ├── did/
        │   └── R script for difference-in-differences models & matching models
        │
        └── asynth/
            └── R script for augmented synthetic control analysis
```

## Methods

This project combines **natural language processing**, **spatial data analysis**, and **causal inference methods** to investigate patterns in political and infrastructure-related data.

### LLM-based Text Classification

Parliamentary text data are processed using large language models (LLMs) to classify documents according to predefined categories. The workflow includes text preprocessing, prompt-based classification, and integration of classification outputs into structured datasets.

### Spatial Data Processing

Geographic datasets are processed using Python-based spatial analysis workflows. Spatial information is cleaned, transformed, and combined with other datasets to enable constituency-level analysis and visualization.

### Data Integration

Multiple datasets are harmonized through common identifiers and merged into analytical datasets. This step combines textual, electoral, and spatial information into a unified structure suitable for statistical analysis.

### Causal Inference

The repository implements causal inference methods in R, including difference-in-differences (DiD) and synthetic control approaches. These methods are used to estimate treatment effects and evaluate changes over time across units of analysis.

## Data Sources

Raw data are not included in this repository. Users interested in reproducing the analyses should obtain the relevant datasets from their original sources.

The analysis combines **multiple sources** of administrative, electoral, biographical, and spatial data.

- **Parliamentary Questions:** Data on parliamentary questions were collected from [SOURCE WEBSITE](https://www.parlzim.gov.zw).
- **Election Data:** Electoral data were obtained from [SOURCE WEBSITE](https://www.zec.org.zw).
- **Biographical Data:** Biographical data on MPs and ministers were collected from various sources, including [SOURCE WEBSITE](https://www.parlzim.gov.zw).
- **Spatial Data:** Geographic data on infant mortality were obtained from [SOURCE WEBSITE](https://dhsprogram.com/data/dataset/Zimbabwe_Standard-DHS_2015.cfm?flag=0). Geographic data on mission stations were obtained from [SOURCE WEBSITE](https://dataverse.harvard.edu/citation?persistentId=doi:10.7910/DVN/E9EEMQ).
- **Infrastructure Data:** Road and health facility data were retrieved from [SOURCE WEBSITE](https://download.geofabrik.de/africa/zimbabwe.html).

## Reproducibility

The entire workflow is not reproducible based on this repository. The repository's purpose is to showcase some selected workflows in Python in R in a structured format.

## How to Run

The scripts in this repository are organized as independent modules and should be executed individually. Each script corresponds to a specific stage of the analytical workflow (e.g., data processing, LLM-based classification, spatial analysis, or causal inference).

Before running the scripts, ensure that the required dependencies and input data are available.

## Requirements

The Python scripts require the packages listed in `requirements.txt`.

R scripts require the corresponding R packages used in the analysis folders.

## API Prompting

The API key is intentionally **not** included in this repository.

Before running API-based Python scripts, load your local API key into the environment by adding the following code at the beginning of each script:

```python
import os

# Read API key from Google Drive
with open(
    "/content/drive/MyDrive/MYPROJECTFOLDER/MYKEY.txt",
    "r"
) as f:
    os.environ["OPENAI_API_KEY"] = f.read().strip()
```

The path and filename shown above are **placeholders** and should be replaced with the location of your own API key file.

## Results

Due to data protection and project constraints, complete analytical outputs are not included in this repository. 

Selected figures are provided in the `docs/` directory to illustrate key results and visualizations generated by the workflow.

## License

This repository is not released under an open-source license.

The code is provided solely as a portfolio and demonstration of programming and data science workflows. The underlying research data are not included.

No permission is granted to copy, redistribute, or use this code except as permitted by applicable copyright law.