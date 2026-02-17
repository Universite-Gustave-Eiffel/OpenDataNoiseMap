---
editor_options: 
  markdown: 
    wrap: 72
---

# Bootstrap directory

This directory contains the **bootstrap logic of the project**,
responsible for initializing the execution environment **before any
pipeline logic is run**.

The bootstrap stage is intentionally lightweight and strictly limited
to:

-   environment detection,
-   dependency and library setup,
-   loading shared utility functions,
-   defining global execution context variables.

No data processing, configuration logic, or pipeline execution must
happen here.

------------------------------------------------------------------------

## 📄 `bootstrap.R`

The `bootstrap.R` script is the **first R file executed** by the
pipeline (entry point: `main.R`). Its responsibilities are:

------------------------------------------------------------------------

### 📥 Load utility functions

All R scripts located in the `R/` directory are sourced automatically.

These scripts typically contain:

-   logging utilities (*e.g.* `pipeline_message()`),
-   I/O helpers,
-   generic helper functions shared across pipelines.

This guarantees that all utilities are available globally before any
other script is executed.

------------------------------------------------------------------------

### 💡 Detect execution context

The execution context is determined using the environment variable
`RUN_CONTEXT`, which is set by the shell launcher scripts:

-   `local` → local execution
-   `hpc` → execution on a SLURM-based HPC cluster

The detected context is stored in the global variable:

``` r
RUN_CONTEXT
```

------------------------------------------------------------------------

### 💻 Environment-specific initialization

Local execution (`RUN_CONTEXT = "local"`):

-   Ensures that the `renv` package is installed,
-   Activates the local `renv` environment if `renv/activate.R` exists.

This guarantees dependency reproducibility when running locally.

HPC execution (`RUN_CONTEXT = "hpc"`):

-   Configures a user-level R library directory,
-   Updates `.libPaths()` accordingly,
-   Extends `LD_LIBRARY_PATH` to support system dependencies (*e.g.*
    `units`, `sf`).
-   Disables automatic opening of a graphics device.

This setup ensures compatibility with non-interactive and headless HPC
jobs.

------------------------------------------------------------------------

### 🌱 Define project root

The project root directory is normalized and stored globally as:
`PROJECT_ROOT`.

This variable is used consistently across the project to ensure:

-   stable relative paths,
-   portability between local and HPC environments.

## 🔆 Design principles

-   **Environment-only logic**

No pipeline, no configuration, no data manipulation.

-   **Idempotent**

The bootstrap can be executed multiple times safely.

-   **Portable**

Works identically on local machines and HPC systems.

## ⚠️ Important notes

-   Do not add pipeline logic here.
-   Do not create directories here.
-   Do not load configuration files here.
-   All configuration and directory creation must happen in later stages
    (`project_setup.R`, configuration files, or dedicated utilities).
-   Utility functions are located in the top-level `R/` directory and
    are sourced by `bootstrap.R`. The `bootstrap/` directory does not
    contain utilities itself.

## 📌 Typical execution flow

```         
run_pipeline.sh
└── main.R
├── bootstrap/bootstrap.R
└── R/\*.R (utilities)
```

------------------------------------------------------------------------
