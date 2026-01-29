# NoiseMapProcess

This directory contains the orchestration scripts for the noise mapping process.

## Description

The scripts in this folder are responsible for chaining the various tools (GeoClimate, NoiseModelling) to produce the final noise exposure indicators. The workflow is primarily driven by bash scripts designed to run in a Linux environment.

## Workflow

1.  **Data Preparation**: Using `GeoClimate` to fetch and format OSM data.
2.  **Noise Calculation**: Running `NoiseModelling` to compute noise maps and receiver levels.
3.  **Integration**: Merging road, rail, and air traffic data.

## Usage

*(Add specific usage instructions for the bash scripts here once populated)*
