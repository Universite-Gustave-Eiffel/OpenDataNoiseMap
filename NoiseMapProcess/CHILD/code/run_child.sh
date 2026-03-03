#!/bin/bash

# -----------------------------------
# Projet CHILD
# Calcul des niveaux de bruits dans l'environnement des enfants issus de la cohorte ELFE
# ---
# Auteur : Gwendall Petit (UMRAE - Université Gustave Eiffel / Cerema) - gwendall.petit@ec-nantes.fr
# Mise à jour : 02/2025
# -----------------------------------

# ----------------------------------------------------------------------------------------------------------
# Arrête le script si jamais une erreur est remontée
set -e

# ----------------------------------------------------------------------------------------------------------
# Définition des adresses

# Chemin vers le fichier CSV contenant la liste des enfants
#CSV_FILE="/home/gpetit/Documents/2024_CHILD/input_data/2024_09_30/bbox.csv"
CSV_FILE="../input_data/ENFANTS_BBOX_1300.csv"
# Chemin vers le fichier JAR de GeoClimate
#JAR_FILE="/home/gpetit/IdeaProjects/plamade_uge/target/GeoClimate_Tools-1.0.3-SNAPSHOT.jar"
JAR_FILE="../geoclimate/GeoClimate_Tools-1.0.3-SNAPSHOT.jar"

# Chemin vers le dossier des données d'entrée
#INPUTFOLDER="/home/gpetit/Documents/2024_CHILD/input_data/"
INPUTFOLDER="../input_data/"
# Chemin vers le dossier de sortie des fichiers
#OUTPUTFOLDER="/home/gpetit/Documents/2024_CHILD/output_data/"
OUTPUTFOLDER="../output_data/"

# Chemin vers le dossier racine contenant tous les scripts WPS de NoiseModelling
#URLSCRIPTS="/home/gpetit/Documents/2024_CHILD/NM/NoiseModelling_without_gui/scriptrunner/bin/wps_scripts"
URLSCRIPTS="../noisemodelling/5.0/NoiseModelling_without_gui/bin/wps_scripts"
# Raccourci vers le dossier contenant les scripts WPS de NoiseModelling
#URLWPS="/home/gpetit/Documents/2024_CHILD/NM/NoiseModelling_without_gui/scriptrunner/noisemodelling/wps"
URLWPS="../noisemodelling/5.0/NoiseModelling_without_gui/noisemodelling/wps"

# Chemin vers le dossier contenant les scripts Groovy propres au projet CHILD
#CUSTOMWPS="/home/gpetit/Documents/2024_CHILD/code"
CUSTOMWPS="../code"

# ----------------------------------------------------------------------------------------------------------
# Lire le fichier CSV ligne par ligne
premiere_ligne=true
while IFS=';' read -r col1 col2 col3
do
    if [ "$premiere_ligne" = true ]; then
        premiere_ligne=false
        continue
    fi
    echo "---------------------------------------------------------"
    echo "---------------------------------------------------------"
    echo "- Traitement de l'enfant n°$col1"
    echo "- Localisation : $col2"
    echo "- BBOX : $col3"
    echo "---------------------------------------------------------"
    echo "---------------------------------------------------------"


# ----------------------------------------------------------------------------------------------------
# Suppression de la base h2gis, utilisée par GeoClimate 
# (Il s'agit d'une précaution car dans certains cas, la base est corrompue et fait planter le script)

# Le fichier est dans le home (~/)
bddGeoclimate="$HOME/h2gisdb.mv.db"

# Vérifier si le fichier existe
if [ -f "$bddGeoclimate" ]; then
  # Supprimer le fichier
  rm "$bddGeoclimate"
  echo "Le fichier h2gisdb.mv.db de GeoClimate existait. Il a donc a été supprimé."
else
  echo "Le fichier h2gisdb.mv.db de GeoClimate n'existe pas."
fi

# ---------------------------------------------------
# Même chose avec la base h2gis de NoiseModelling
# Le fichier est dans le même dossier que ce script .sh (/code/)
bddNoisemodelling="./h2gisdb.mv.db"

# Vérifier si le fichier existe
if [ -f "$bddNoisemodelling" ]; then
  # Supprimer le fichier
  rm "$bddNoisemodelling"
  echo "Le fichier h2gisdb.mv.db de NoiseModelling existait. Il a donc a été supprimé."
else
  echo "Le fichier h2gisdb.mv.db de NoiseModelling n'existe pas."
fi


# ----------------------------------------------------------------------------------------------------
# Exécution de GeoClimate

# java -Dfile.encoding=UTF-8 -jar "$JAR_FILE" -l ["$col3"] -o "$OUTPUTFOLDER" -s 4326 

until java -Dfile.encoding=UTF-8 -jar "$JAR_FILE" -l ["$col3"] -o "$OUTPUTFOLDER" -s 4326 ; do
    echo "Nouvelle tentative de téléchargement des données OSM dans 30s"
    sleep 30
done

# On récupère la bbox du csv, qui va servir à créer le dossier correspondant
# On remplace les virgules par _ dans le nom des dossiers résultants

RESULTSFOLDER="${col3//,/_}"

# Step 4: Upload files to database
# create (or load existing) database and load a shape file into the database

# ----------------------------------------------------------------------------------------------------
# Import des fichiers geojson issus de GeoClimate dans la base de données et forçage du SRID à 2154
# Buildings
"$URLSCRIPTS" -w ./ -s "${URLWPS}/Import_and_Export/Import_File.groovy" -pathFile "${OUTPUTFOLDER}osm_${RESULTSFOLDER}/building.geojson" -inputSRID 4326
"$URLSCRIPTS" -w ./ -s "${URLWPS}/Geometric_Tools/Change_SRID.groovy" -tableName BUILDING -newSRID 2154
"$URLSCRIPTS" -w ./ -s "${URLWPS}/Database_Manager/Add_Primary_Key.groovy" -tableName BUILDING -pkName PK
# Ground
"$URLSCRIPTS" -w ./ -s "${URLWPS}/Import_and_Export/Import_File.groovy" -pathFile "${OUTPUTFOLDER}osm_${RESULTSFOLDER}/ground_acoustic.geojson" -inputSRID 4326
"$URLSCRIPTS" -w ./ -s "${URLWPS}/Geometric_Tools/Change_SRID.groovy" -tableName GROUND_ACOUSTIC -newSRID 2154
# DEM
"$URLSCRIPTS" -w ./ -s "${URLWPS}/Import_and_Export/Import_File.groovy" -pathFile "${OUTPUTFOLDER}osm_${RESULTSFOLDER}/dem.geojson" -inputSRID 4326
"$URLSCRIPTS" -w ./ -s "${URLWPS}/Geometric_Tools/Change_SRID.groovy" -tableName DEM -newSRID 2154


# ----------------------------------------------------------------------------------------------------
# Import du réseau routier, avec le trafic et les vitesses prédits par l'UMRAE

ROUTE_GEOJSON="${INPUTFOLDER}trafic_enfants/ROUTE_${col1}.geojson"
#/home/gpetit/Documents/2024_CHILD/input_data/Predicted_trafic/trafic_enfants
# On vérifie si le fichier existe

# Si oui, alors on l'importe dans la base de données
if [ -f "$ROUTE_GEOJSON" ]; then
    echo "Le fichier ROUTE_${col1}.geojson existe, importation en cours..."
    # On met en base le geojson
    "$URLSCRIPTS" -w ./ -s "${URLWPS}/Import_and_Export/Import_File.groovy" -pathFile "${ROUTE_GEOJSON}" -inputSRID 2154 -tableName ROAD_TRAFFIC
else
    # S'il n'existe pas, on va en créé un vide, avec le même nom 
    # et on le stockera dans le dossier /creation/ afin de ne pas le mélanger avec les vrais fichiers
    echo "Le fichier ROUTE_${col1}.geojson n'existe pas. On va donc en créer un vide"

    EMPTYROUTE="${INPUTFOLDER}trafic_enfants/creation/ROUTE_${col1}.geojson"

    # Si jamais le fichier vide avait déjà été créé dans le dossier /creation/, alors on le supprime préventivement
    if [ -f "$EMPTYROUTE" ]; then
        rm "$EMPTYROUTE"
        echo "Le fichier ROUTE_${col1}.geojson existait déjà dans le dossier /creation/. Il a donc été supprimé avant d'en créer un nouveau vide."
    else
        echo "Le fichier n'existe pas déjà dans le dossier /creation/."
    fi
    # Créer le fichier avec une structure GeoJSON vide
    echo '{
    "type": "FeatureCollection",
    "name": "FID_'${col1}'",
    "crs": { "type": "name", "properties": { "name": "urn:ogc:def:crs:EPSG::2154" }},
    "features": [{ "type": "Feature", "properties": {"ogc_fid": 'null', "pk": 'null', "osm_id": 'null', "lv_spd_d": 'null', "lv_spd_e": 'null', "lv_spd_n": 'null', "hgv_spd_d": 'null', "hgv_spd_e": 'null', "hgv_spd_n": 'null', "pvmt": 'null', "lv_d": 'null', "lv_e": 'null' "lv_n": 'null', "hgv_d": 'null', "hgv_e": 'null', "hgv_n": 'null', "fid": 'null' }, "geometry": 'null' }]
    }' >> "$EMPTYROUTE"
    # On met en base le geojson vide
    "$URLSCRIPTS" -w ./ -s "${URLWPS}/Import_and_Export/Import_File.groovy" -pathFile "${EMPTYROUTE}" -inputSRID 2154 -tableName ROAD_TRAFFIC

fi


"$URLSCRIPTS" -w ./ -s "${URLWPS}/Geometric_Tools/Set_Height.groovy" -tableName ROAD_TRAFFIC -height 0.05

# ----------------------------------------------------------------------------------------------------
# Import des Plan d'Exposition du Bruit (PEB) pour l'avion
# Source des PEB : https://geoservices.ign.fr/services-web-experts-transports#2314

AVION_GEOJSON="${INPUTFOLDER}avion/avion_enfants/AVION_${col1}.geojson"

# On vérifie si le fichier existe

# Si oui, alors on l'importe dans la base de données
if [ -f "$AVION_GEOJSON" ]; then
    echo "Le fichier AVION_${col1}.geojson existe, importation en cours..."
    # On met en base le geojson
    "$URLSCRIPTS" -w ./ -s "${URLWPS}/Import_and_Export/Import_File.groovy" -pathFile "${AVION_GEOJSON}" -inputSRID 2154
else
    # S'il n'existe pas, on va en créé un vide, avec le même nom 
    # et on le stockera dans le dossier /creation/ afin de ne pas le mélanger avec les vrais fichiers
    echo "Le fichier AVION_${col1}.geojson n'existe pas. On va donc en créer un vide"

    EMPTYAVION="${INPUTFOLDER}avion/avion_enfants/creation/AVION_${col1}.geojson"

    # Si jamais le fichier vide avait déjà été créé dans le dossier /creation/, alors on le supprime préventivement
    if [ -f "$EMPTYAVION" ]; then
        rm "$EMPTYAVION"
        echo "Le fichier AVION_${col1}.geojson existait déjà dans le dossier /creation/. Il a donc été supprimé avant d'en créer un nouveau vide."
    else
        echo "Le fichier n'existe pas déjà dans le dossier /creation/."
    fi
    # Créer le fichier avec une structure GeoJSON vide
    echo '{
    "type": "FeatureCollection",
    "name": "FID_'${col1}'",
    "crs": { "type": "name", "properties": { "name": "urn:ogc:def:crs:EPSG::2154" }},
    "features": [ { "type": "Feature", "properties": {"FID": 'null', "AERO_TXT": 'null', "AERO": 'null'}, "geometry": 'null' }]
    }' >> "$EMPTYAVION"
    # On met en base le geojson vide
    "$URLSCRIPTS" -w ./ -s "${URLWPS}/Import_and_Export/Import_File.groovy" -pathFile "${EMPTYAVION}" -inputSRID 2154

fi

# ----------------------------------------------------------------------------------------------------
# Import des données pour le train
# Source : Couche "cbs_2154" produite dans le cadre du projet Plamade
# Prétraitement :
# - Seules les CBS ferrées (SOURCETYPE='F') de type A (CBSTYPE='A') sont conservées
# - Pour finir, les CBS en LDEN (INDICETYPE ='LD') sont dissociées dans CBS en LNIGHT (INDICETYPE ='LN')


# Import du train en LDEN
# -------------------------
TRAIN_LDEN_GEOJSON="${INPUTFOLDER}train/train_enfants_lden/TRAIN_LDEN_${col1}.geojson"

# On vérifie si le fichier existe

# Si oui, alors on l'importe dans la base de données
if [ -f "$TRAIN_LDEN_GEOJSON" ]; then
    echo "Le fichier TRAIN_LDEN_${col1}.geojson existe, importation en cours..."
    # On met en base le geojson
    "$URLSCRIPTS" -w ./ -s "${URLWPS}/Import_and_Export/Import_File.groovy" -pathFile "${TRAIN_LDEN_GEOJSON}" -inputSRID 2154
else
    # S'il n'existe pas, on va en créé un vide, avec le même nom 
    # et on le stockera dans le dossier /creation/ afin de ne pas le mélanger avec les vrais fichiers
    echo "Le fichier TRAIN_LDEN_${col1}.geojson n'existe pas. On va donc en créer un vide"

    EMPTYTRAIN="${INPUTFOLDER}train/train_enfants_lden/creation/TRAIN_LDEN_${col1}.geojson"

    # Si jamais le fichier vide avait déjà été créé dans le dossier /creation/, alors on le supprime préventivement
    if [ -f "$EMPTYTRAIN" ]; then
        rm "$EMPTYTRAIN"
    else
        echo "Le fichier n'existe pas déjà dans le dossier /creation/."       
    fi
    # Créer le fichier avec une structure GeoJSON vide
    echo '{
    "type": "FeatureCollection",
    "name": "FID_'${col1}'",
    "crs": { "type": "name", "properties": { "name": "urn:ogc:def:crs:EPSG::2154" }},
    "features": [ { "type": "Feature", "properties": { "FID": 'null', "LDEN": 'null', "LDEN_DB": 'null'}, "geometry": 'null' }]
    }' >> "$EMPTYTRAIN"
    # On met en base le geojson vide
    "$URLSCRIPTS" -w ./ -s "${URLWPS}/Import_and_Export/Import_File.groovy" -pathFile "${EMPTYTRAIN}" -inputSRID 2154    
fi

# Import du train en LNIGHT
# -------------------------
TRAIN_LNIGHT_GEOJSON="${INPUTFOLDER}train/train_enfants_lnight/TRAIN_LNIGHT_${col1}.geojson"

# On vérifie si le fichier existe

# Si oui, alors on l'importe dans la base de données
if [ -f "$TRAIN_LNIGHT_GEOJSON" ]; then
    echo "Le fichier TRAIN_LNIGHT_${col1}.geojson existe, importation en cours..."
    # On met en base le geojson
    "$URLSCRIPTS" -w ./ -s "${URLWPS}/Import_and_Export/Import_File.groovy" -pathFile "${TRAIN_LNIGHT_GEOJSON}" -inputSRID 2154
else
    # S'il n'existe pas, on va en créé un vide, avec le même nom 
    # et on le stockera dans le dossier /creation/ afin de ne pas le mélanger avec les vrais fichiers
    echo "Le fichier TRAIN_LNIGHT_${col1}.geojson n'existe pas. On va donc en créer un vide"

    EMPTYTRAIN="${INPUTFOLDER}train/train_enfants_lnight/creation/TRAIN_LNIGHT_${col1}.geojson"

    # Si jamais le fichier vide avait déjà été créé dans le dossier /creation/, alors on le supprime préventivement
    if [ -f "$EMPTYTRAIN" ]; then
        rm "$EMPTYTRAIN"
    else
        echo "Le fichier n'existe pas déjà dans le dossier /creation/."
    fi
    # Créer le fichier avec une structure GeoJSON vide
    echo '{
    "type": "FeatureCollection",
    "name": "FID_'${col1}'",
    "crs": { "type": "name", "properties": { "name": "urn:ogc:def:crs:EPSG::2154" }},
    "features": [ { "type": "Feature", "properties": { "FID": 'null', "LNIGHT": 'null', "LNIGHT_DB": 'null'}, "geometry": 'null' }]
    }' >> "$EMPTYTRAIN"
    # On met en base le geojson vide
    "$URLSCRIPTS" -w ./ -s "${URLWPS}/Import_and_Export/Import_File.groovy" -pathFile "${EMPTYTRAIN}" -inputSRID 2154
fi



# ----------------------------------------------------------------------------------------------------
# Traitement des récepteurs autour du bâtiment ou vit l'enfant
# ----------------------------------------------------------------------------------------------------

# On génère la table des récepteurs, 2m autour du bâtiment le plus proche de l'enfant

# Détection du bâtiment le plus proche
"$URLSCRIPTS" -w ./ -s "${CUSTOMWPS}/Building_Fence.groovy" -inputPoint "$col2" -childID "$col1"
echo "La table FENCE a été créee, avec le point $col2"

# Export des tables POINT (l'enfant), HOME (sa maison) et FENCE (la zone qui englobe le bâtiment - à 10m)
"$URLSCRIPTS" -w ./ -s "${URLWPS}/Import_and_Export/Export_Table.groovy" -exportPath "${OUTPUTFOLDER}osm_${RESULTSFOLDER}/POINT.shp" -tableToExport POINT
"$URLSCRIPTS" -w ./ -s "${URLWPS}/Import_and_Export/Export_Table.groovy" -exportPath "${OUTPUTFOLDER}osm_${RESULTSFOLDER}/HOME.shp" -tableToExport BATIMENT
"$URLSCRIPTS" -w ./ -s "${URLWPS}/Import_and_Export/Export_Table.groovy" -exportPath "${OUTPUTFOLDER}osm_${RESULTSFOLDER}/FENCE.shp" -tableToExport FENCE

# Génération des récepteurs à 2m du bâtiment (on ajoute un récepteur tous les 2m)
"$URLSCRIPTS" -w ./ -s "${URLWPS}/Receivers/Building_Grid.groovy" -tableBuilding BUILDING -height 4 -fenceTableName FENCE -delta 2

# Pour générer les récepteurs, NoiseModelling prend la bbox du fence. Il se peut donc que des capteurs n'ayant rien à faire avec le bâtiment principal soient présents.
# On doit donc les supprimer
"$URLSCRIPTS" -w ./ -s "${CUSTOMWPS}/Remove_wrong_receivers.groovy" -childID "$col1"

# (Optionnel) Export de la table RECEIVERS_HOME
# "$URLSCRIPTS" -w ./ -s "${URLWPS}/Import_and_Export/Export_Table.groovy" -exportPath "${OUTPUTFOLDER}osm_${RESULTSFOLDER}/RECEIVERS_HOME.shp" -tableToExport RECEIVERS_HOME

# Calcul des niveaux sonores, sur les récepteurs, autour du bâtiment où vit l'enfant
"$URLSCRIPTS" -w ./ -s "${URLWPS}/NoiseModelling/Noise_level_from_traffic.groovy" -tableBuilding BUILDING -tableRoads ROAD_TRAFFIC -tableReceivers RECEIVERS_HOME -tableDEM DEM -tableGroundAbs GROUND_ACOUSTIC -confDiffVertical false -confDiffHorizontal true -confMaxSrcDist 800

# Affectation des niveaux de bruit (en dB), issus de l'avion et du train, ramenés sur les récepteurs du bâtiment
"$URLSCRIPTS" -w ./ -s "${CUSTOMWPS}/Rail_Aero_dB_to_receivers.groovy" -childID "$col1" -inputReceiversTable RECEIVERS_HOME

# Unificiation des différentes données au niveau des récepteurs du bâtiment
"$URLSCRIPTS" -w ./ -s "${CUSTOMWPS}/Merge_db_indicators.groovy" -childID "$col1" -inputReceiversTable RECEIVERS_HOME -outputChildTable CHILD_HOME

# Export de la table résultante : CHILD_HOME (en .geojson, .shp et .csv)
"$URLSCRIPTS" -w ./ -s "${URLWPS}/Import_and_Export/Export_Table.groovy" -exportPath "${OUTPUTFOLDER}osm_${RESULTSFOLDER}/CHILD_HOME.geojson" -tableToExport CHILD_HOME
"$URLSCRIPTS" -w ./ -s "${URLWPS}/Import_and_Export/Export_Table.groovy" -exportPath "${OUTPUTFOLDER}osm_${RESULTSFOLDER}/CHILD_HOME.shp" -tableToExport CHILD_HOME
"$URLSCRIPTS" -w ./ -s "${URLWPS}/Import_and_Export/Export_Table.groovy" -exportPath "${OUTPUTFOLDER}osm_${RESULTSFOLDER}/CHILD_HOME.csv" -tableToExport CHILD_HOME


# ----------------------------------------------------------------------------------------------------
# Traitement des 100 récepteurs aléatoires, dans la zone de marchabilité de l'enfant
# ----------------------------------------------------------------------------------------------------

# Génération de la grille irrégulière avec 5000 récepteurs
"$URLSCRIPTS" -w ./ -s "${URLWPS}/Receivers/Random_Grid.groovy" -buildingTableName "BUILDING" -sourcesTableName "ROAD_TRAFFIC" -nReceivers 5000
# Parmi ces 5000 on n'en garde que 100 (de manière aléatoire), qui sont dans la zone de marchabilité
"$URLSCRIPTS" -w ./ -s "${CUSTOMWPS}/Remove_Random_Receivers.groovy" -childID "$col1"
# Ajout d'une clé primaire à la table RECEIVERS_RANDOM
"$URLSCRIPTS" -w ./ -s "${URLWPS}/Database_Manager/Add_Primary_Key.groovy" -tableName RECEIVERS_RANDOM -pkName PK
# (Optionnel) Export de la table RECEIVERS_RANDOM
"$URLSCRIPTS" -w ./ -s "${URLWPS}/Import_and_Export/Export_Table.groovy" -exportPath "${OUTPUTFOLDER}osm_${RESULTSFOLDER}/RECEIVERS_RANDOM.shp" -tableToExport RECEIVERS_RANDOM
# Calcul des niveaux sonores issus du trafic routier
"$URLSCRIPTS" -w ./ -s "${URLWPS}/NoiseModelling/Noise_level_from_traffic.groovy" -tableBuilding BUILDING -tableRoads ROAD_TRAFFIC -tableReceivers RECEIVERS_RANDOM -tableDEM DEM -tableGroundAbs GROUND_ACOUSTIC -confDiffVertical false -confDiffHorizontal true -confMaxSrcDist 800

# Affectation des niveaux de bruit (en dB), issus de l'avion et du train, ramenés sur les récepteurs
"$URLSCRIPTS" -w ./ -s "${CUSTOMWPS}/Rail_Aero_dB_to_receivers.groovy" -childID "$col1" -inputReceiversTable RECEIVERS_RANDOM

# Unificiation des différentes données au niveau des récepteurs
"$URLSCRIPTS" -w ./ -s "${CUSTOMWPS}/Merge_db_indicators.groovy" -childID "$col1" -inputReceiversTable RECEIVERS_RANDOM -outputChildTable CHILD_RANDOM

# Export de la table résultante : CHILD_RANDOM (en .geojson, .shp et .csv)
"$URLSCRIPTS" -w ./ -s "${URLWPS}/Import_and_Export/Export_Table.groovy" -exportPath "${OUTPUTFOLDER}osm_${RESULTSFOLDER}/CHILD_RANDOM.geojson" -tableToExport CHILD_RANDOM
"$URLSCRIPTS" -w ./ -s "${URLWPS}/Import_and_Export/Export_Table.groovy" -exportPath "${OUTPUTFOLDER}osm_${RESULTSFOLDER}/CHILD_RANDOM.shp" -tableToExport CHILD_RANDOM
"$URLSCRIPTS" -w ./ -s "${URLWPS}/Import_and_Export/Export_Table.groovy" -exportPath "${OUTPUTFOLDER}osm_${RESULTSFOLDER}/CHILD_RANDOM.csv" -tableToExport CHILD_RANDOM

# ----------------------------------------------------------------------------------------------------
# Renommage du dossier cible pour remplacer "osm_bbox" par "enfant_fid" (identifiant de l'enfant)
# ----------------------------------------------------------------------------------------------------

if [ -d "${OUTPUTFOLDER}enfant_${col1}" ]; then
    # Supprimez le dossier
    rm -r "${OUTPUTFOLDER}enfant_${col1}"
    echo "Dossier enfant_${col1} existait déjà, il a donc été supprimé."
else
    echo "Le dossier enfant_${col1} n'existe pas."
fi

 mv "${OUTPUTFOLDER}osm_${RESULTSFOLDER}" "${OUTPUTFOLDER}enfant_${col1}"
 echo "Les données ont été déplacées dans le dossier enfant_${col1}."

 # Une bdd H2GIS est générée dans le dossier /home. On la supprime par précaution, sachant qu'elle sera recréée à chaque enfant
 #rm ~/h2gisdb.mv.db

    echo "-----------------------------------------"
    echo "-----------------------------------------"
    echo "- Fin du traitement de l'enfant n°$col1"
    echo "-----------------------------------------"
    echo "-----------------------------------------"

done < "$CSV_FILE"