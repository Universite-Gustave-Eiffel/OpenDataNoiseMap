# Pipeline launcher scripts

This folder contains the scripts used to launch the pipelines for the **OpenDataNoiseMap** project, both **locally** and on the **HPC (SLURM)**, from a single entry point:

-   `run_pipeline.sh`: main script that runs the R pipeline (`main.R`) with a given mode,
-   `run_pipeline.slurm`: `Slurm` script for HPC runs.

------------------------------------------------------------------------

## 📌 Main script: `run_pipeline.sh`

### 🎯 Purpose

`run_pipeline.sh` is a unified launcher that:

-   automatically detects the execution context (`local` or `HPC`),
-   configures the environment (`R` / `GDAL` modules on `HPC`),
-   correctly sets the project root,
-   passes the **pipeline mode** to the R script `main.R`,
-   redirects R logs to the `logs/` folder.

------------------------------------------------------------------------

### ▶️ Run the R script pipelines locally

From the **root** of the project:

``` bash
scripts/run_pipeline.sh [MODE]
```

#### 📑 Examples:

-   **Prepare data:**

    ``` bash
    sh scripts/run_pipeline.sh data_prep
    ```

-   **Download Avatar data:**

    ``` bash
    sh scripts/run_pipeline.sh avatar
    ```

-   **Model training:**

    ``` bash
    sh scripts/run_pipeline.sh training
    ```

-   **Forecast for the city of Nantes:**

    ``` bash
    sh scripts/run_pipeline.sh nantes
    ```

-   **Forecast for the city of Paris:**

    ``` bash
    sh scripts/run_pipeline.sh paris
    ```

-   **Forecast for sensors :**

    ``` bash
    sh scripts/run_pipeline.sh sensors
    ```

-   ⚠️ **Default mode** (If no mode is provided)**:**

    ``` bash
    bash scripts/run_pipeline.sh
    ```

    The default mode is: `data_prep`

### 💻 Run the R script pipelines on HPC (Slurm)

Install:

``` bash
module purge || true
module load R/R-4.4.2
module load gcc/gcc-12
```

``` bash
mkdir -p ~/local/src
mkdir -p ~/local/udunits

cd ~/local/src
wget https://downloads.unidata.ucar.edu/udunits/2.2.28/udunits-2.2.28.tar.gz
tar -xzf udunits-2.2.28.tar.gz
cd udunits-2.2.28

./configure --prefix=$HOME/local/udunits
make -j4
make install

export UDUNITS2_INCLUDE=$HOME/local/udunits/include
export UDUNITS2_LIBS=$HOME/local/udunits/lib
export LD_LIBRARY_PATH=$HOME/local/udunits/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=$HOME/local/udunits/lib/pkgconfig:$PKG_CONFIG_PATH
echo $LD_LIBRARY_PATH | tr ":" "\n" | grep udunits

R --vanilla
```

``` r
dyn.load("~/local/udunits/lib/libudunits2.so")
install.packages("units", 
                 configure.args = 
                    c("--with-udunits2-lib=$HOME/local/udunits/lib", 
                      "--with-udunits2-include=$HOME/local/udunits/include"), 
                 dependencies = TRUE, 
                 repos = "https://cran.rstudio.com/", 
                 lib = "~/R/x86_64-pc-linux-gnu-library/4.4.2/")
install.packages(c("Rcpp", "dplyr", "tidyr", "sf", "lwgeom", "httr", "jsonlite", 
                   "lubridate", "randomForest", "data.table", "stringr", 
                   "sfnetworks", "igraph", "tidygraph", "progress", "ggplot2", 
                   "gridExtra", "xgboost", "Matrix"), 
                 configure.args = 
                    c("--with-udunits2-lib=$HOME/local/udunits/lib", 
                      "--with-udunits2-include=$HOME/local/udunits/include"), 
                 dependencies = TRUE, 
                 repos = "https://cran.rstudio.com/", 
                 lib = "~/R/x86_64-pc-linux-gnu-library/4.4.2/")
```

From the **root** of the project:

``` bash
sh scripts/run_pipeline.slurm
```

This script:

-   requests HPC resources (CPU, memory, time),
-   launches `run_pipeline.sh` with the desired mode,
-   writes Slurm logs to `logs/slurm_<jobid>.out`.

The execution mode is defined in `run_pipeline.slurm`:

``` bash
sbatch scripts/run_pipeline.sh data_prep
```

⚠️ Please note: if no mode is given, the default mode (`data_prep`) is used.

⚠️ Avatar data cannot be downloaded via a Slurm job. To download the data from a computing server (HPC), run one of the following two commands:

-   Override manuel :
    
    ``` bash
    sh run_pipeline.sh avatar
    ```

-   Override manuel :
    
    ``` bash
    RUN_CONTEXT=local bash run_pipeline.sh avatar
    ```


### 📂 Generated log files

-   R logs: `logs/<MODE>.Rout` Example: `logs/data_prep.Rout`

-   Slurm logs (HPC only): `logs/slurm_<jobid>.out`

### 🧠 Transmission of the mode to the R pipeline

The mode is passed to the script `main.R` via command line arguments:

``` bash
R --vanilla -f main.R --args <MODE>
```

In `main.R`, the argument is retrieved by:

``` r
args <- commandArgs(trailingOnly = TRUE)
MODE <- if (length(args) >= 1) args[1] else "data_prep"
```

### 📖 Detailed pipeline documentation

The internal logic of each pipeline is documented in the folder:

```         
pipelines/
├── data_preparation/
├── training/
└── forecast/
```

Each subfolder contains a `README.md` file describing:

-   the objectives of the pipeline,
-   the input/output data,
-   the scripts executed,
-   the specific parameters.

------------------------------------------------------------------------
