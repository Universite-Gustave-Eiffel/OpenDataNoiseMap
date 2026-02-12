# OpenDataNoiseMap - Noise Exposure Estimation

This repository hosts the methodology and tools used to estimate the noise exposure of individual agents. The project combines road, rail, and air traffic noise modeling to provide a comprehensive assessment of environmental sound levels.

## Methodology

The analysis is conducted at two spatial levels aimed at capturing different aspects of noise exposure:

1.  **Building Level**: Noise levels are calculated around the building closest to the agents postal address, assumed to be their home.
2.  **Neighborhood Level (500m buffer)**: Sound levels are calculated within a 500-meter radius around the agents location. This involves 100 random receiver points selected within this buffer zone (excluding building footprints and road right-of-ways).

### Sound Sources Evaluated

1.  **Road Traffic**: Modeled using **NoiseModelling** and the **CNOSSOS-EU** emission/propagation method.
2.  **Air Traffic**: Based on Airborne Noise Exposure Plans (PEB) from the DGAC.
3.  **Rail Traffic**: Based on Strategic Noise Maps (CBS) for major railway infrastructures (>30,000 trains/year).

## Input Data

The project relies on the following datasets:

*   **Agent Registry**: FID codes and planar coordinates (Lambert 93, EPSG:2154).
*   **Geospatial Data**: **OpenStreetMap (OSM)** for buildings, land use, and digital elevation model (DEM).
*   **Aircraft Noise**: [DGAC Airborne Noise Exposure Plans](https://geoservices.ign.fr/services-web-experts-transports#2314).
*   **Railway Noise**: Strategic Noise Maps (CBS) from the PLAMADE project.
*   **Road Traffic Estimates**: Modeled using the `RoadTrafficEstimation` module in this repository.

## Tools & Processing Chain

The workflow is orchestrated via bash scripts in a Linux (Ubuntu) environment and utilizes the following open-source tools:

*   **[GeoClimate](https://github.com/orbisgis/geoclimate)**: For downloading and formatting OpenStreetMap data.
*   **[NoiseModelling](https://noise-planet.org/noisemodelling.html)**: For calculating road traffic noise levels using the CNOSSOS-EU standard (configured with 800m propagation, first-order reflections, diffraction, etc.).
*   **RoadTrafficEstimation**: A custom R package (included in this repo) for spatiotemporal traffic modeling, integrating OSM network geometry with CEREMA AVATAR API counts.

## Repository Structure

*   **`RoadTrafficEstimation/`**: R pipeline for estimating road traffic flows and speeds from OSM and AVATAR data.
*   **`NoiseMapProcess/`**: Bash scripts and configurations for running the NoiseModelling and GeoClimate chain.

## Outputs

The project generates acoustic indicators for each agent, expressed in dB(A):

*   **Road Traffic**: $L_{DEN}$, $L_{DAY}$ (06-18h), $L_{EVENING}$ (18-22h), $L_{NIGHT}$ (22-06h).
*   **Rail Traffic**: $L_{DEN}$ and $L_{NIGHT}$ (discretized in 5dB bands).
*   **Air Traffic**: $L_{DEN}$.

## Authors & References

*   *Strategic Noise Mapping in France to 2023*, Aumond et al. (2023).
*   *GeoClimate: a Geospatial processing toolbox*, Bocher et al.
*   *NoiseModelling: an open-source software for outdoor noise mapping*.
