# NoiseMapProcess

This directory contains the orchestration scripts for the noise mapping process.

## Description

The scripts in this folder are responsible for chaining the various tools (GeoClimate, NoiseModelling) to produce the final noise exposure indicators. The workflow is primarily driven by bash scripts designed to run in a Linux environment.

From a practical point of view, this process takes as input a list of points (defined by a pair of X and Y coordinates and a unique identifier), which will serve as the basis for generating receivers on which sound levels will be estimated, based on road traffic, but also rail and air traffic, if the data is available.

## Workflow

1.  **Data Preparation**: Using `GeoClimate` to fetch and format OSM data.
2.  **Noise Calculation**: Running `NoiseModelling` to compute noise maps and receiver levels.
3.  **Integration**: Merging road, rail, and air traffic data.

## Use case

To illustrate the process, we propose a use case linked with the "CHILD" research project. The aim of this project was to assess the sound environments of nearly 20,000 French children from a cohort to see whether their exposure to noise had an impact on their development (from birth to age 12).
For more information, take a look at the `/CHILD/` folder.

### Publication

An article about this project was recently submitted to the journal "[The Lancet Public Health](https://www.thelancet.com/journals/lanpub/home)": 
> Eloi Chazelas; Gwendall Petit; Pierre Aumond; Devin Parker; Marie Aline Charles; Maria Melchior, "*Residential noise exposure and children’s emotional and behavioral difficulties: results from the French ELFE birth cohort*"
