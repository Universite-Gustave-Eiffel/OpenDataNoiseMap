#!/bin/bash
# -----------------------------------------------------------------------------
# Local execution of NM_OSM pipeline
# -----------------------------------------------------------------------------

# Choose execution mode: nantes | paris | sensors
MODE="sensors"

# Run the pipeline in R (non-interactive)
R --vanilla -f main.R --args "${MODE}"
