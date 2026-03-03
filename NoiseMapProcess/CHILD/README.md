# CHILD

As part of the CHILD research project, this folder contains the code and the documentation which describes the sequence of processes and operations used to calculate the noise levels to which children from the [ELFE cohort](https://www.santepubliquefrance.fr/etudes-et-enquetes/cohorte-elfe) are exposed.

The CHILD project is led by epidemiologists from [INSERM](https://essma.iplesp.fr/). [UMRAE](https://www.umrae.fr/) is responsible for the "noise exposure" section.

The codes and documentation are therefore adapted to a use case related to children's exposure to noise, which are considered as points (defined via a pair of X and Y coordinates, accompanied by a unique identifier).
The process could very well work for another use case, as long as the object of study is represented by a point.

## File organisation

- `bdd` : contains a .sql script that allows to prepare input data
- `code` : contains the scripts needed to execute the chain
- `docs` : contains the documentation explaining the process
- `geoclimate` : contains the [GeoClimate](https://github.com/orbisgis/geoclimate) opensource library, needed to retreive the OSM input data
- `noisemodelling/5.0/` : contains the `NoiseModelling_without_gui.zip` file, needed to compute noise exposure

## Workflow

1. **Data Preparation**: prepare the data belonging to the CHILD (his position and its bounding box, the road trafic, the airplane and train potential exposure) 
2. **GeoClimate** to fetch and format OSM data.
3. **Noise Calculation**: running `NoiseModelling` to compute noise maps and receiver levels.
4. **Integration**: merging road, rail, and air traffic data.
