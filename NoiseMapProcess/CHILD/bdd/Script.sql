/*
 * Scripts de préparation des données pour le projet CHILD
 * 
 * Auteur : G. Petit (UMRAE - Université Gustave Eiffel)
 * Dernière mise à jour 02/2025
 */

-- jdbc:h2:file:~/Documents/2024_CHILD/bdd/bd_child


DROP TABLE IF EXISTS public.points, public.points_2154, PUBLIC.buffer_500, PUBLIC.buffer_500_2154;

-- Récupération et mise en base des enfants (points) issus d'ELFE
CALL SHPREAD ('/home/gpetit/Documents/2024_CHILD/input_data/2025_02_04/enfants.shp', 'public.points');

CREATE TABLE public.POINTS_2154 AS SELECT 
	ST_TRANSFORM(ST_SetSRID(the_geom,2154), 2154) as the_geom, 
	PK, FID
	FROM PUBLIC.POINTS;
UPDATE public.POINTS_2154 SET THE_GEOM = ST_SETSRID(THE_GEOM, 2154);
CREATE SPATIAL INDEX ON public.POINTS_2154 (the_geom);

-- Génération d'un buffer de 500m autour des points
DROP TABLE IF EXISTS public.buffer_500_2154;
CREATE TABLE public.buffer_500_2154 AS SELECT st_buffer(the_geom, 500) AS the_geom, PK, FID
	FROM PUBLIC.POINTS_2154;
CREATE SPATIAL INDEX ON public.buffer_500_2154 (the_geom);
CALL GeoJsonWrite ('/home/gpetit/Documents/2024_CHILD/input_data/2025_02_04/buffer_500_2154.geojson', 'public.buffer_500_2154');


-- Génération d'un buffer de 1300m autour des points
DROP TABLE IF EXISTS public.BUFFER_1300_2154;
CREATE TABLE public.BUFFER_1300_2154 AS SELECT st_buffer(THE_GEOM, 1300) AS THE_GEOM, PK, FID
	FROM PUBLIC.POINTS_2154;
CALL UpdateGeometrySRID('BUFFER_1300_2154','THE_GEOM',2154);
CREATE SPATIAL INDEX ON public.BUFFER_1300_2154 (THE_GEOM);

CALL SHPWRITE('/home/gpetit/Documents/2024_CHILD/input_data/2025_02_04/BUFFER_1300_2154.shp','BUFFER_1300_2154');
CALL GeoJsonWrite ('/home/gpetit/Documents/2024_CHILD/input_data/2025_02_04/BUFFER_1300_2154.geojson', 'public.BUFFER_1300_2154');


-- Génération des enveloppes des buffers à 1300m
DROP TABLE IF EXISTS BUFFER_1300_ENV;
CREATE TABLE BUFFER_1300_ENV AS SELECT ST_ENVELOPE(THE_GEOM) AS THE_GEOM FROM buffer_1300_2154;
CALL GeoJsonWrite ('/home/gpetit/Documents/2024_CHILD/input_data/2025_02_04/BUFFER_1300_ENV.geojson', 'public.BUFFER_1300_ENV');


DROP TABLE IF EXISTS ENFANTS_BBOX_1300;
CREATE TABLE ENFANTS_BBOX_1300 AS SELECT ST_ENVELOPE(a.THE_GEOM) AS THE_GEOM, a.PK, a.FID, p.THE_GEOM AS GEOM_POINT 
	FROM BUFFER_1300_2154 a, POINTS p
	WHERE a.FID = p.FID ;
ALTER TABLE ENFANTS_BBOX_1300 ADD COLUMN BBOX varchar;
UPDATE ENFANTS_BBOX_1300 SET BBOX = CONCAT( 
ST_Y(ST_TRANSFORM(ST_SetSRID(ST_MAKEPOINT(ST_XMIN(the_geom), ST_YMIN(the_geom)), 2154), 4326)), ',',
ST_X(ST_TRANSFORM(ST_SetSRID(ST_MAKEPOINT(ST_XMIN(the_geom), ST_YMIN(the_geom)), 2154), 4326)), ',',
ST_Y(ST_TRANSFORM(ST_SetSRID(ST_MAKEPOINT(ST_XMAX(the_geom), ST_YMAX(the_geom)), 2154), 4326)), ',',
ST_X(ST_TRANSFORM(ST_SetSRID(ST_MAKEPOINT(ST_XMAX(the_geom), ST_YMAX(the_geom)), 2154), 4326)));
							

-- Export
CALL GeoJsonWrite ('/home/gpetit/Documents/2024_CHILD/input_data/2025_02_04/ENFANTS_BBOX_1300.geojson', 'public.ENFANTS_BBOX_1300');
CALL CSVWRITE('/home/gpetit/Documents/2024_CHILD/input_data/2025_02_04/ENFANTS_BBOX_1300.csv','SELECT fid, geom_point, bbox FROM public.ENFANTS_BBOX_1300');


/*
DROP TABLE IF EXISTS POINT;
CREATE TABLE POINT AS SELECT ST_SetSRID(ST_GeomFromText('POINT (7.2525833946990685 43.711721934854964)'), 2154) as THE_GEOM FROM BUILDING LIMIT 1;
CREATE TABLE POINT AS SELECT ST_Transform(ST_SetSRID(ST_GeomFromText('POINT (7.2525833946990685 43.711721934854964)'), 4326), 2154) as THE_GEOM FROM BUILDING LIMIT 1;
*/

---------------------------------------------------------------------------------
-- Test pour sélection du bâtiment le plus proche du point
---------------------------------------------------------------------------------

DROP TABLE IF EXISTS BUILDING, BUILDING_2154;
CALL GEOJSONREAD ('/home/gpetit/Documents/2024_CHILD/NM/OSM/osm_43.70322028484077_7.239530434208419_43.720222066678765_7.265640230326265/building.geojson', 'BUILDING');
CREATE TABLE BUILDING_2154 AS SELECT ST_TRANSFORM(ST_SetSRID(the_geom,4326), 2154) as the_geom, id_build FROM BUILDING b ;

SELECT st_buffer(a.THE_GEOM, 2.1) AS THE_GEOM , a.ID_BUILD, p.FID FROM BUILDING_2154 a, POINTS_2154 p WHERE p.FID =0 ORDER by st_distance(a.THE_GEOM, p.THE_GEOM) ASC LIMIT 1;




---------------------------------------------------------------------------------
-- Sélection des cbs de train les plus proches des enfants (points)
---------------------------------------------------------------------------------

DROP TABLE IF EXISTS train;
CALL GEOJSONREAD('/home/gpetit/Documents/2024_CHILD/input_data/train/cbs_f_2154_explode.geojson', 'train');

DROP TABLE IF EXISTS train_lden;
CREATE TABLE train_lden AS SELECT THE_GEOM, 
										(CASE 	WHEN category ='Lden5559' THEN '55-59' 
												WHEN category ='Lden6064' THEN '60-64' 
												WHEN category ='Lden6569' THEN '65-69'
												WHEN category ='Lden7074' THEN '70-74'
												WHEN category ='LdenGreaterThan75' THEN 'sup 75' 
												END )AS LDEN,
										(CASE 	WHEN category ='Lden5559' THEN '57' 
												WHEN category ='Lden6064' THEN '62' 
												WHEN category ='Lden6569' THEN '67'
												WHEN category ='Lden7074' THEN '72'
												WHEN category ='LdenGreaterThan75' THEN '75' 
												END )AS LDEN_DB
	FROM train WHERE CBSTYPE = 'A' AND INDICETYPE ='LD';

CREATE SPATIAL INDEX ON train_lden (THE_GEOM);

DROP TABLE IF EXISTS train_lnight;
CREATE TABLE train_lnight AS SELECT THE_GEOM, 
										(CASE 	WHEN category ='Lnight5054' THEN '50-54' 
												WHEN category ='Lnight5559' THEN '55-59' 
												WHEN category ='Lnight6064' THEN '60-64'
												WHEN category ='Lnight6569' THEN '65-69'
												WHEN category ='LnightGreaterThan70' THEN 'sup 70' 
												END )AS LNIGHT,
										(CASE 	WHEN category ='Lnight5054' THEN '52' 
												WHEN category ='Lnight5559' THEN '57' 
												WHEN category ='Lnight6064' THEN '62'
												WHEN category ='Lnight6569' THEN '67'
												WHEN category ='LnightGreaterThan70' THEN '70' 
												END )AS LNIGHT_DB
	FROM train WHERE CBSTYPE = 'A' AND INDICETYPE ='LN';

CREATE SPATIAL INDEX ON train_lnight (THE_GEOM);




DROP TABLE IF EXISTS train_lden_buffer;
CREATE TABLE train_lden_buffer AS SELECT b.FID, st_intersection(a.THE_GEOM, b.THE_GEOM) AS the_geom, a.LDEN, a.LDEN_DB 
	FROM train_lden a, BUFFER_500_2154 b
	WHERE a.THE_GEOM && b.THE_GEOM AND st_intersects(a.THE_GEOM, b.the_geom);

CALL SHPWRITE('/home/gpetit/Documents/2024_CHILD/input_data/train/train_lden_buffer.shp','train_lden_buffer');
CALL GEOJSONWRITE('/home/gpetit/Documents/2024_CHILD/input_data/train/train_lden_buffer.geojson','train_lden_buffer');

DROP TABLE IF EXISTS train_lnight_buffer;
CREATE TABLE train_lnight_buffer AS SELECT b.FID, st_intersection(ST_SETSRID(a.THE_GEOM, 2154), ST_SETSRID(b.THE_GEOM, 2154)) AS the_geom, a.LNIGHT, a.LNIGHT_DB 
	FROM train_lnight a, BUFFER_500_2154 b
	WHERE a.THE_GEOM && b.THE_GEOM AND st_intersects(a.THE_GEOM, b.the_geom);

CALL SHPWRITE('/home/gpetit/Documents/2024_CHILD/input_data/train/train_lnight_buffer.shp','train_lnight_buffer');
CALL GEOJSONWRITE('/home/gpetit/Documents/2024_CHILD/input_data/train/train_lnight_buffer.geojson','train_lnight_buffer');

---------------------------------------------------------------------------------
-- Sélection des cbs des avions les plus proches des enfants (points)
---------------------------------------------------------------------------------

DROP TABLE IF EXISTS public.avion;

CALL SHPREAD ('/home/gpetit/Documents/2024_CHILD/input_data/avion/peb.shp', 'public.avion');
CREATE SPATIAL INDEX ON public.avion (THE_GEOM);

DROP TABLE IF EXISTS avion_2154;
CREATE TABLE avion_2154 AS SELECT st_transform(ST_SETSRID(THE_GEOM, 2154), 2154) AS the_geom, "ZONE", INDLDENEXT, INDLDENINT, CODE_OACI FROM AVION;
CREATE SPATIAL INDEX ON public.avion_2154 (THE_GEOM);

------------------
-- Contrôle des valeurs présentes. Suppression ou correction si besoin
SELECT DISTINCT INDLDENEXT FROM avion_2154 ORDER BY INDLDENEXT;
SELECT DISTINCT INDLDENINT FROM avion_2154 ORDER BY INDLDENINT;

SELECT * FROM AVION_2154 WHERE INDLDENEXT ='Emprise';
SELECT * FROM AVION_2154 WHERE INDLDENINT  ='Emprise';
DELETE FROM AVION_2154 WHERE INDLDENINT  ='Emprise';

SELECT * FROM AVION_2154 WHERE INDLDENEXT ='IP1975';
UPDATE AVION_2154 SET INDLDENEXT ='65' WHERE INDLDENEXT ='IP1975';
------------------

-- Ajout de 2 champs pour le niveau sonore moyen
ALTER TABLE AVION_2154 ADD COLUMN AERO_TXT varchar;
UPDATE AVION_2154 SET AERO_TXT = CONCAT(INDLDENINT, '-', INDLDENEXT);
ALTER TABLE AVION_2154 ADD COLUMN AERO double;
UPDATE AVION_2154 SET AERO = (INDLDENINT ::double + INDLDENEXT :: double)/2

-- Intersection avec les buffer 500m
DROP TABLE IF EXISTS public.avion_buffer;
CREATE TABLE avion_buffer AS SELECT b.FID, st_intersection(a.THE_GEOM, b.the_geom) AS the_geom, a.AERO_TXT, a.AERO
	FROM AVION_2154 a, BUFFER_500_2154 b
	WHERE a.THE_GEOM && b.THE_GEOM AND st_intersects(a.THE_GEOM, b.the_geom);

-- Export en geojson
CALL GEOJSONWRITE('/home/gpetit/Documents/2024_CHILD/input_data/avion/avion.geojson','avion_buffer');


---------------------------------------------------------------------------------
-- Découpage du trafic prédit pour chaque enfants
---------------------------------------------------------------------------------

DROP TABLE IF EXISTS public.trafic_predit;

CALL SHPREAD ('/home/gpetit/Documents/2024_CHILD/2/Predicted_trafic/predictedgo/predictedgo.shp', 'public.trafic_predit');

DROP TABLE IF EXISTS public.trafic_predit_2154;
CREATE TABLE public.trafic_predit_2154 AS SELECT 
	PK, ST_Transform(ST_SETSRID(THE_GEOM, 4326), 2154) as THE_GEOM, FID, OSM_ID, LV_SPD_D, LV_SPD_E, LV_SPD_N, HGV_SPD_D, HGV_SPD_E, HGV_SPD_N, PVMT, LV_D, LV_E, LV_N, HGV_D, HGV_E, HGV_N
	FROM trafic_predit;

CREATE SPATIAL INDEX ON public.trafic_predit_2154 (THE_GEOM);

-- Import de la liste des tunnels à supprimer
DROP TABLE IF EXISTS public.trafic_tunnels;
CREATE TABLE public.trafic_tunnels AS SELECT * FROM CSVRead('/home/gpetit/Documents/2024_CHILD/input_data/Predicted_trafic/tunnels_filt.csv');

-- Suppression des tunnels dans les 2 tables trafic_predit (et 2154)
DELETE FROM trafic_predit WHERE OSM_ID IN (SELECT OSM_ID FROM trafic_tunnels);
DELETE FROM trafic_predit_2154 WHERE OSM_ID IN (SELECT OSM_ID FROM trafic_tunnels);



DROP TABLE IF EXISTS public.BUFFER_1300_L93;
create table BUFFER_1300_L93 as select st_transform(ST_SETSRID(THE_GEOM, 2154), 2154) AS THE_GEOM, PK, FID from BUFFER_1300_2154;
CREATE SPATIAL INDEX ON public.BUFFER_1300_L93 (THE_GEOM);



-- Intersection du trafic predit avec les buffers de 1300m autour des enfants
DROP TABLE IF EXISTS public.trafic_predit_1300;

CREATE TABLE trafic_predit_1300 AS SELECT a.FID as PK, st_intersection(a.THE_GEOM, b.the_geom) AS THE_GEOM, 
	a.OSM_ID, a.LV_SPD_D, a.LV_SPD_E, a.LV_SPD_N, a.HGV_SPD_D, a.HGV_SPD_E, a.HGV_SPD_N, 
	a.PVMT, a.LV_D, a.LV_E, a.LV_N, a.HGV_D, a.HGV_E, a.HGV_N, b.FID
FROM trafic_predit_2154 a, buffer_1300_2154 b
	WHERE a.THE_GEOM && b.the_geom AND st_intersects(a.THE_GEOM, b.the_geom);

CALL GeoJsonWrite ('/home/gpetit/Documents/2024_CHILD/input_data/Predicted_trafic/TRAFIC_PREDICT_1300.geojson', 'public.trafic_predit_1300');



----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------
-- Gestion des enfants qui ont déménagés en 2014 et 2016
----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

DROP TABLE IF EXISTS public.points_2014, public.points_2016, public.POINTS_2014_2016, public.POINTS_2154_2014_2016, PUBLIC.buffer_500_2154_2014_2016;

-- Récupération et mise en base des enfants (points) issus d'ELFE, en 2014 et 2016
CALL SHPREAD ('/home/gpetit/Documents/2024_CHILD/input_data/2025_02_19/enfants_2014.shp', 'public.points_2014');
CALL SHPREAD ('/home/gpetit/Documents/2024_CHILD/input_data/2025_02_19/enfants_2016.shp', 'public.points_2016');

-- Unification des deux tables
DROP TABLE IF EXISTS PUBLIC.POINTS_2014_2016;
CREATE TABLE PUBLIC.POINTS_2014_2016 AS SELECT the_geom, PK, FID FROM public.points_2014 UNION SELECT the_geom, PK, FID FROM public.points_2016;

-- Reprojection en Lambert 93
CREATE TABLE public.POINTS_2154_2014_2016 AS SELECT 
	ST_TRANSFORM(ST_SetSRID(the_geom,4326), 2154) as the_geom, 
	PK, FID
	FROM PUBLIC.POINTS_2014_2016 ORDER BY FID ASC;
UPDATE public.POINTS_2154_2014_2016 SET THE_GEOM = ST_SETSRID(THE_GEOM, 2154);
CREATE SPATIAL INDEX ON public.POINTS_2154_2014_2016 (the_geom);

-- Génération d'un buffer de 500m autour des points
DROP TABLE IF EXISTS public.buffer_500_2154_2014_2016;
CREATE TABLE public.buffer_500_2154_2014_2016 AS SELECT st_buffer(the_geom, 500) AS the_geom, PK, FID
	FROM PUBLIC.POINTS_2154_2014_2016;
CREATE SPATIAL INDEX ON public.buffer_500_2154_2014_2016 (the_geom);
CALL GeoJsonWrite ('/home/gpetit/Documents/2024_CHILD/input_data/2025_02_19/buffer_500_2154_2014_2016.geojson', 'public.buffer_500_2154_2014_2016');


-- Génération d'un buffer de 1300m autour des points
DROP TABLE IF EXISTS public.BUFFER_1300_2154_2014_2016;
CREATE TABLE public.BUFFER_1300_2154_2014_2016 AS SELECT st_buffer(THE_GEOM, 1300) AS THE_GEOM, PK, FID
	FROM PUBLIC.POINTS_2154_2014_2016;
CALL UpdateGeometrySRID('BUFFER_1300_2154_2014_2016','THE_GEOM',2154);
CREATE SPATIAL INDEX ON public.BUFFER_1300_2154_2014_2016 (THE_GEOM);

CALL SHPWRITE('/home/gpetit/Documents/2024_CHILD/input_data/2025_02_19/BUFFER_1300_2154_2014_2016.shp','BUFFER_1300_2154_2014_2016');
CALL GeoJsonWrite ('/home/gpetit/Documents/2024_CHILD/input_data/2025_02_19/BUFFER_1300_2154_2014_2016.geojson', 'public.BUFFER_1300_2154_2014_2016');


-- Génération des enveloppes des buffers à 1300m
DROP TABLE IF EXISTS BUFFER_1300_ENV_2014_2016;
CREATE TABLE BUFFER_1300_ENV_2014_2016 AS SELECT ST_ENVELOPE(THE_GEOM) AS THE_GEOM FROM BUFFER_1300_2154_2014_2016;
CALL GeoJsonWrite ('/home/gpetit/Documents/2024_CHILD/input_data/2025_02_19/BUFFER_1300_ENV_2014_2016.geojson', 'public.BUFFER_1300_ENV_2014_2016');


DROP TABLE IF EXISTS ENFANTS_BBOX_1300_2014_2016;
CREATE TABLE ENFANTS_BBOX_1300_2014_2016 AS SELECT ST_ENVELOPE(a.THE_GEOM) AS THE_GEOM, a.PK, a.FID, p.THE_GEOM AS GEOM_POINT 
	FROM BUFFER_1300_2154_2014_2016 a, POINTS_2014_2016 p
	WHERE a.FID = p.FID ;
ALTER TABLE ENFANTS_BBOX_1300_2014_2016 ADD COLUMN BBOX varchar;
UPDATE ENFANTS_BBOX_1300_2014_2016 SET BBOX = CONCAT( 
ST_Y(ST_TRANSFORM(ST_SetSRID(ST_MAKEPOINT(ST_XMIN(the_geom), ST_YMIN(the_geom)), 2154), 4326)), ',',
ST_X(ST_TRANSFORM(ST_SetSRID(ST_MAKEPOINT(ST_XMIN(the_geom), ST_YMIN(the_geom)), 2154), 4326)), ',',
ST_Y(ST_TRANSFORM(ST_SetSRID(ST_MAKEPOINT(ST_XMAX(the_geom), ST_YMAX(the_geom)), 2154), 4326)), ',',
ST_X(ST_TRANSFORM(ST_SetSRID(ST_MAKEPOINT(ST_XMAX(the_geom), ST_YMAX(the_geom)), 2154), 4326)));
							

-- Export
CALL GeoJsonWrite ('/home/gpetit/Documents/2024_CHILD/input_data/2025_02_19/ENFANTS_BBOX_1300_2014_2016.geojson', 'public.ENFANTS_BBOX_1300_2014_2016');
CALL CSVWRITE('/home/gpetit/Documents/2024_CHILD/input_data/2025_02_19/ENFANTS_BBOX_1300_2014_2016.csv','SELECT fid, geom_point, bbox FROM public.ENFANTS_BBOX_1300_2014_2016');


---------------------------------------------------------------------------------
-- Sélection des cbs de train les plus proches des enfants (points)
---------------------------------------------------------------------------------

DROP TABLE IF EXISTS train_lden_buffer_2014_2016;
CREATE TABLE train_lden_buffer_2014_2016 AS SELECT b.FID, a.THE_GEOM AS the_geom, a.LDEN, a.LDEN_DB 
	FROM train_lden a, BUFFER_500_2154_2014_2016 b
	WHERE a.THE_GEOM && b.THE_GEOM AND st_intersects(a.THE_GEOM, b.the_geom);

CALL SHPWRITE('/home/gpetit/Documents/2024_CHILD/input_data/train/2014_2016/train_lden_buffer.shp','train_lden_buffer_2014_2016');
CALL GEOJSONWRITE('/home/gpetit/Documents/2024_CHILD/input_data/train/2014_2016/train_lden_buffer.geojson','train_lden_buffer_2014_2016');

DROP TABLE IF EXISTS train_lnight_buffer_2014_2016;
CREATE TABLE train_lnight_buffer_2014_2016 AS SELECT b.FID, a.THE_GEOM AS the_geom, a.LNIGHT, a.LNIGHT_DB 
	FROM train_lnight a, BUFFER_500_2154_2014_2016 b
	WHERE a.THE_GEOM && b.THE_GEOM AND st_intersects(a.THE_GEOM, b.the_geom);

CALL SHPWRITE('/home/gpetit/Documents/2024_CHILD/input_data/train/2014_2016/train_lnight_buffer.shp','train_lnight_buffer_2014_2016');
CALL GEOJSONWRITE('/home/gpetit/Documents/2024_CHILD/input_data/train/2014_2016/train_lnight_buffer.geojson','train_lnight_buffer_2014_2016');

---------------------------------------------------------------------------------
-- Sélection des cbs des avions les plus proches des enfants (points)
---------------------------------------------------------------------------------

-- Intersection avec les buffer 500m
DROP TABLE IF EXISTS public.avion_buffer_2014_2016;
CREATE TABLE avion_buffer_2014_2016 AS SELECT b.FID, a.THE_GEOM, a.AERO_TXT, a.AERO
	FROM AVION_2154 a, BUFFER_500_2154_2014_2016 b
	WHERE a.THE_GEOM && b.THE_GEOM AND st_intersects(a.THE_GEOM, b.the_geom);

-- Export en geojson
CALL GEOJSONWRITE('/home/gpetit/Documents/2024_CHILD/input_data/avion/2014_2016/avion.geojson','avion_buffer_2014_2016');


---------------------------------------------------------------------------------
-- Découpage du trafic prédit pour chaque enfants
---------------------------------------------------------------------------------

DROP TABLE IF EXISTS public.BUFFER_1300_L93_2014_2016;
create table BUFFER_1300_L93_2014_2016 as select st_transform(ST_SETSRID(THE_GEOM, 2154), 2154) AS THE_GEOM, PK, FID from BUFFER_1300_2154_2014_2016;
CREATE SPATIAL INDEX ON public.BUFFER_1300_L93_2014_2016 (THE_GEOM);



-- Intersection du trafic predit avec les buffers de 1300m autour des enfants
DROP TABLE IF EXISTS public.trafic_predit_1300_2014_2016;

CREATE TABLE trafic_predit_1300_2014_2016 AS SELECT a.FID as PK, st_intersection(a.THE_GEOM, b.the_geom) AS THE_GEOM, 
	a.OSM_ID, a.LV_SPD_D, a.LV_SPD_E, a.LV_SPD_N, a.HGV_SPD_D, a.HGV_SPD_E, a.HGV_SPD_N, 
	a.PVMT, a.LV_D, a.LV_E, a.LV_N, a.HGV_D, a.HGV_E, a.HGV_N, b.FID
FROM trafic_predit_2154 a, BUFFER_1300_L93_2014_2016 b
	WHERE a.THE_GEOM && b.the_geom AND st_intersects(a.THE_GEOM, b.the_geom);

CALL GeoJsonWrite ('/home/gpetit/Documents/2024_CHILD/input_data/Predicted_trafic/2014_2016/TRAFIC_PREDICT_1300_2014_2016.geojson', 'public.trafic_predit_1300_2014_2016');




