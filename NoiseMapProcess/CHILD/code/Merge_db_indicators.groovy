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

title = 'Création de la table CHILD_HOME'
description = 'Cette table compile les indicateurs acoustiques calculés pour la route, le train et l\'avion, au niveau des récepteurs autour du bâtiment de l\'enfant'

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
        ],
        outputChildTable: [
                name       : 'Table de récepteurs en sortie',
                title      : 'Nom de la table comportant les récepteurs en sortie',
                description: 'Nom de la table comportant les récepteurs en sortie',
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
    String outputChildTable = input['outputChildTable'] as String

    // print to command window
    logger.info('Début de la création de la table '+ outputChildTable)


        Statement stmt = connection.createStatement()
        String dropTable = "DROP TABLE IF EXISTS "+ outputChildTable +";"
        stmt.execute(dropTable)
        // Génère la table, en compilant les indicateurs
        String createReceivers = "CREATE TABLE "+ outputChildTable +" AS SELECT a.THE_GEOM, "+ childID +" as FID, a.IDRECEIVER, ROUND(a.LAEQ,1) as ROAD_DEN, ROUND(b.LAEQ, 1) as ROAD_DAY, ROUND(c.LAEQ,1) as ROAD_EVE, ROUND(d.LAEQ,1) as ROAD_NIGHT, e.LDEN_DB as RAIL_DEN, f.LNIGHT_DB as RAIL_NIGHT, g.AERO_TXT as AERO, g.AERO as AERO_DB FROM LDEN_GEOM a, LDAY_GEOM b, LEVENING_GEOM c, LNIGHT_GEOM d, RECEIVERS_RAIL_LDEN e, RECEIVERS_RAIL_LNIGHT f, RECEIVERS_AVION g WHERE a.IDRECEIVER = b.IDRECEIVER and a.IDRECEIVER = c.IDRECEIVER and a.IDRECEIVER = d.IDRECEIVER and a.IDRECEIVER = e.PK and a.IDRECEIVER = f.PK and a.IDRECEIVER = g.PK;"
        stmt.execute(createReceivers)
                
    // print to command window
    logger.info('La table '+ outputChildTable +' a été créée')

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