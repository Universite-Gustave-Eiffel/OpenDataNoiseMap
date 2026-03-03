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

title = 'Point vers fence'
description = 'Génère un point et un fence (buffer de 2.1m) autour du bâtiment le plus proche du point'

inputs = [
        inputPoint: [
                name       : 'Point en WKT',
                title      : 'Point en WKT',
                description: 'Point en WKT',
                type       : String.class
        ],
        childID: [
                name       : 'Identifiant de l\'enfant',
                title      : 'Identifiant de l\'enfant',
                description: 'Identifiant de l\'enfant',
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

    // print to command window
    logger.info('Début de la création du point et du fence')

    // Récupère le point en WKT
    String inputPoint = input['inputPoint'] as String
    String childID = input['childID'] as String

        Statement stmt = connection.createStatement()
        String dropTable = "DROP TABLE IF EXISTS POINT, BATIMENT, FENCE;"
        stmt.execute(dropTable)
        // Génère le point
        String createPoint = "CREATE TABLE POINT AS SELECT ST_Transform(ST_SetSRID(ST_GeomFromText('"+ inputPoint +"'), 4326), 2154) as THE_GEOM, "+ childID +" as FID  FROM BUILDING LIMIT 1;"
        stmt.execute(createPoint)
        // Isole le bâtiment le plus proche du point
        String createBat = "CREATE TABLE BATIMENT AS SELECT a.THE_GEOM, a.ID_BUILD, p.FID FROM BUILDING a, POINT p ORDER by ST_Distance(a.THE_GEOM, p.THE_GEOM) ASC LIMIT 1;"
        stmt.execute(createBat)
        // Génère le fence autour du bâtiment le plus proche du point 
        // On applique une distance de 10m pour éviter les soucis avec des petits bâtiments collés à d'autres : soucis avec des récepteurs potentiellement absents
        String createFence = "CREATE TABLE FENCE AS SELECT ST_Buffer(a.THE_GEOM, 10) AS THE_GEOM, a.ID_BUILD FROM BUILDING a, POINT p ORDER by ST_Distance(a.THE_GEOM, p.THE_GEOM) ASC LIMIT 1;"
        stmt.execute(createFence)
                
    // print to command window
    logger.info('Un fence a été créé autour du bâtiment le plus proche')

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