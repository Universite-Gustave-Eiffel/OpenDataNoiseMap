/**
 * NoiseModelling is an open-source tool designed to produce environmental noise maps on very large urban areas. It can be used as a Java library or be controlled through a user friendly web interface.
 *
 * This version is developed by the DECIDE team from the Lab-STICC (CNRS) and by the Mixt Research Unit in Environmental Acoustics (Université Gustave Eiffel).
 * <http://noise-planet.org/noisemodelling.html>
 *
 * NoiseModelling is distributed under GPL 3 license. You can read a copy of this License in the file LICENCE provided with this software.
 *
 * Contact: contact@noise-planet.org
 *
 */

/**
 * @Author Gwendall Petit, Université Gustave Eiffel
 */

package org.noise_planet.noisemodelling.wps.Database_Manager

import geoserver.GeoServer
import geoserver.catalog.Store
import org.geotools.jdbc.JDBCDataStore
import org.h2gis.utilities.JDBCUtilities
import org.h2gis.utilities.TableLocation
import org.slf4j.Logger
import org.slf4j.LoggerFactory

import java.sql.Connection
import java.sql.Statement

title = 'Affectation des dB avion et train'
description = 'Affectation des niveaux de bruit (en dB), issus de l\'avion et du train, ramenés sur les récepteurs du bâtiment ou vit l\'enfant'

inputs = [
        childID: [
                name       : 'Identifiant de l\'enfant',
                title      : 'Identifiant de l\'enfant',
                description: 'Identifiant de l\'enfant',
                type       : String.class
        ],
        inputReceiversTable: [
                name       : 'Table de récepteurs',
                title      : 'Nom de la table comportant les récepteurs',
                description: 'Nom de la table comportant les récepteurs',
                type       : String.class
        ]
]

outputs = [
        result: [
                name       : 'Result output string',
                title      : 'Result output string',
                description: 'This type of result does not allow the blocks to be linked together.',
                type       : String.class
        ]
]

static Connection openGeoserverDataStoreConnection(String dbName) {
    if (dbName == null || dbName.isEmpty()) {
        dbName = new GeoServer().catalog.getStoreNames().get(0)
    }
    Store store = new GeoServer().catalog.getStore(dbName)
    JDBCDataStore jdbcDataStore = (JDBCDataStore) store.getDataStoreInfo().getDataStore(null)
    return jdbcDataStore.getDataSource().getConnection()
}

def exec(Connection connection, input) {
    // output string, the information given back to the user
    String resultString = null

    // Create a logger to display messages in the geoserver logs and in the command prompt.
    Logger logger = LoggerFactory.getLogger("org.noise_planet.noisemodelling")

    // Récupère le point en WKT
    String childID = input['childID'] as String
    String inputReceiversTable = input['inputReceiversTable'] as String

    // print to command window
    logger.info('Début de la récupération des niveaux sonores issus des avions et des trains')

        Statement stmt = connection.createStatement()
        String dropTableTrain = "DROP TABLE IF EXISTS RECEIVERS_RAIL_LDEN, RECEIVERS_RAIL_LNIGHT;"
        stmt.execute(dropTableTrain)
        // Génère les tables, en récupérant les indicateurs
        String train_lden = "CREATE TABLE RECEIVERS_RAIL_LDEN AS SELECT a.*, b.LDEN, b.LDEN_DB FROM "+ inputReceiversTable +" a LEFT JOIN TRAIN_LDEN_"+ childID +" b ON a.THE_GEOM && b.THE_GEOM AND ST_INTERSECTS(a.THE_GEOM, b.THE_GEOM);"
        stmt.execute(train_lden)        
        String train_lnight = "CREATE TABLE RECEIVERS_RAIL_LNIGHT AS SELECT a.*, b.LNIGHT, b.LNIGHT_DB FROM "+ inputReceiversTable +" a LEFT JOIN TRAIN_LNIGHT_"+ childID +" b ON a.THE_GEOM && b.THE_GEOM AND ST_INTERSECTS(a.THE_GEOM, b.THE_GEOM);"
        stmt.execute(train_lnight)

        String dropTableAvion = "DROP TABLE IF EXISTS RECEIVERS_AVION;"
        stmt.execute(dropTableAvion)
        String avion = "CREATE TABLE RECEIVERS_AVION AS SELECT a.*, b.AERO_TXT, b.AERO FROM "+ inputReceiversTable +" a LEFT JOIN AVION_"+ childID +" b ON a.THE_GEOM && b.THE_GEOM AND ST_INTERSECTS(a.THE_GEOM, b.THE_GEOM);"
        stmt.execute(avion)

    // print to command window
    logger.info('Les tables RECEIVERS_RAIL_LDEN, RECEIVERS_RAIL_LNIGHT et RECEIVERS_AVION ont été créées')

    // print to WPS Builder
    return resultString
}


def run(input) {

    // Get name of the database
    // by default an embedded h2gis database is created
    // Advanced user can replace this database for a postGis or h2Gis server database.
    String dbName = "h2gisdb"

    // Open connection
    openGeoserverDataStoreConnection(dbName).withCloseable {
        Connection connection ->
            return [result: exec(connection, input)]
    }
}