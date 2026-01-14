# ==============================================================================
# MAIN ENTRY POINT — DO NOT PUT LOGIC ELSEWHERE
# ==============================================================================

# 1. Bootstrap environment (renv / HPC libs)
source("bootstrap.R")

# 2. Set global pipeline configuration
source("config_pipeline.R")

# 3. Run the actual pipeline
source("run_pipeline.R")