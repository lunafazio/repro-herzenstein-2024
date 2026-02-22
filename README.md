# repro-herzenstein-2024

This repository contains code to facilitate the computational reproduction of
[Herzenstein, Rosario, Oblander and Netze 2024](https://doi.org/10.1177/09567976241254037),
and adds a robustness check for their *over time* analysis.

## Usage

Our scripts are designed to work within the exact structure of the replication
folder shared by the authors, available at https://osf.io/qy8ev/.

The `results/` folder in this repo contains already-generated output from a run
of our robustness analysis for convenience.

After downloading the replication folder, all the R files in this repo should be copied into the `code` subfolder, as shown below:

```
📁 .
├── 📁 Supplemental material
└── 📁 Code and Data
    ├── 📁 code ⬅️
    │   ├── <16 code files from authors>
    │   ├── main-repro.R
    │   ├── main-robustness.R
    │   ├── utils-repro.R
    │   └── utils-robustness.R
    ├── 📁 processed_data
    ├── 📁 readme_files
    └── 📁 results
```

### Reproducing study results

The file `main-repro.R` is a master script that runs all of the author-provided
files in the appropriate order and compares the re-generated outputs with those
in the replication folder.

Running this script with 8 cores on a MacBook Pro (M2, 2022) takes >12 hours.

**NOTE:** Before running the script, some additional manual setup is required.
See the comments in lines 3-12 of `main-repro.R` for details.

### Robustness check of *over time* analysis

The file `main-robustness.R` extends the authors' over time analysis by
considering other possible cut-off years for the test-train split. Minimal
manual setup is required to run the file.

Running this script with 8 cores on a MacBook Pro (M2, 2022) takes over 1 hour.

The `results/` folder in this repo contains already-generated output from a run
of our robustness analysis for convenience.

## Acknowledgements

Some code in this package is adapted from the original R scripts provided by the
authors of the study, available at https://osf.io/qy8ev/.

This code was developed as part of the 2025 Münster Replication Games.
