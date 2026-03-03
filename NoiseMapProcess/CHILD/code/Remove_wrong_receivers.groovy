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

title = 'Supprime les récepteurs non nécessaires'
description = 'Pour générer les récepteurs, NM prend la bbox du fence. Il se peut donc que des capteurs n\'ayant rien à faire avec le bâtiment principal soient présents. On doit donc les supprimer'

inputs = [
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
    logger.info('Début de la suppression des récepteurs non liés au bâtiment')

    // Récupère le point en WKT
    String childID = input['childID'] as String

        Statement stmt = connection.createStatement()


        String dropReceiversTable = "DROP TABLE IF EXISTS BAT_PROCHE, RECEIVERS_HOME;"
        stmt.execute(dropReceiversTable)

        String batProche = "CREATE TABLE BAT_PROCHE AS SELECT a.BUILD_PK FROM RECEIVERS a, BATIMENT b ORDER by ST_Distance(a.THE_GEOM, b.THE_GEOM) ASC LIMIT 1;"
        stmt.execute(batProche)          

        String deleteReceivers = "DELETE FROM RECEIVERS WHERE BUILD_PK NOT IN (SELECT BUILD_PK FROM BAT_PROCHE);"
        stmt.execute(deleteReceivers)
        String renameReceivers = "ALTER TABLE RECEIVERS RENAME TO RECEIVERS_HOME;"
        stmt.execute(renameReceivers)
        String dropReceivers = "DROP TABLE IF EXISTS RECEIVERS;"
        stmt.execute(dropReceivers)
        

    // print to command window
    logger.info('Les récepteurs non nécessaires ont été supprimés et la table RECEIVERS a été renommée RECEIVERS_HOME')

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