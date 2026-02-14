import org.noise_planet.noisemodelling.emission.road.cnossos.RoadCnossos;
import org.noise_planet.noisemodelling.emission.road.cnossos.RoadCnossosParameters;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

/**
 * Batch CNOSSOS-EU road emission calculator.
 * 
 * Reads CSV lines from stdin: flow,truck_pct,speed[,temperature][,road_surface]
 *   - flow: total vehicles/hour (TV)
 *   - truck_pct: percentage of heavy goods vehicles (0-100)
 *   - speed: vehicle speed in km/h
 *   - temperature: air temperature in °C (optional, default 20)
 *   - road_surface: CNOSSOS road surface code (optional, default "NL08")
 * 
 * Outputs one dB(A) value per line to stdout.
 * 
 * Uses NoiseModelling 5.x CNOSSOS-EU 2020 implementation.
 * The calculation sums over 8 octave bands (63–8000 Hz),
 * applies A-weighting, and returns the global Lw/m in dB(A).
 */
public class CnossosEmissionBatch {

    // CNOSSOS octave band center frequencies (Hz)
    private static final int[] FREQUENCIES = {63, 125, 250, 500, 1000, 2000, 4000, 8000};

    // A-weighting corrections for each octave band (dB)
    private static final double[] A_WEIGHTING = {-26.2, -16.1, -8.6, -3.2, 0.0, 1.2, 1.0, -1.1};

    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        StringBuilder sb = new StringBuilder(1024 * 1024);
        String line;

        while ((line = reader.readLine()) != null) {
            line = line.trim();
            if (line.isEmpty() || line.startsWith("#") || line.startsWith("flow")) {
                continue; // skip headers and comments
            }

            String[] parts = line.split(",");
            if (parts.length < 3) {
                sb.append("NA\n");
                continue;
            }

            try {
                double flow = Double.parseDouble(parts[0].trim());
                double truckPct = Double.parseDouble(parts[1].trim());
                double speed = Double.parseDouble(parts[2].trim());
                double temperature = parts.length >= 4 ? Double.parseDouble(parts[3].trim()) : 20.0;
                String roadSurface = parts.length >= 5 ? parts[4].trim() : "NL08";

                double dbA = computeEmissionDbA(flow, truckPct, speed, temperature, roadSurface);
                sb.append(String.format(java.util.Locale.US, "%.4f\n", dbA));
            } catch (Exception e) {
                sb.append("NA\n");
            }
        }

        System.out.print(sb);
    }

    /**
     * Compute the total road emission level in dB(A)/m using CNOSSOS-EU.
     * 
     * @param flow       Total vehicle flow (vehicles/hour)
     * @param truckPct   Percentage of heavy goods vehicles (0-100)
     * @param speed      Vehicle speed (km/h)
     * @param temperature Air temperature (°C)
     * @param roadSurface CNOSSOS road surface identifier
     * @return Global emission level in dB(A)/m
     */
    public static double computeEmissionDbA(double flow, double truckPct, double speed,
                                             double temperature, String roadSurface) throws IOException {
        // Clamp inputs
        flow = Math.max(flow, 0);
        truckPct = Math.max(0, Math.min(100, truckPct));
        speed = Math.max(speed, 20); // CNOSSOS minimum speed is 20 km/h

        // Split flow into vehicle categories
        double hgvPerHour = flow * (truckPct / 100.0);
        double lvPerHour = flow - hgvPerHour;

        // All categories use the same speed (our model predicts a single speed)
        // Categories: 1=LV, 2=MV (medium vehicles), 3=HGV, 4a=2-wheel-a, 4b=2-wheel-b
        // We assign all trucks to cat 3 (HGV), rest to cat 1 (LV)
        // No medium vehicles, no 2-wheelers in our simplified model
        double mvPerHour = 0;
        double wavPerHour = 0;
        double wbvPerHour = 0;

        // No studded tyres, no junction effect
        double tsStud = 0;
        double pmStud = 0;
        double juncDist = 300; // far from junction = no effect
        int juncType = 1;

        double totalPowerA = 0;

        for (int i = 0; i < FREQUENCIES.length; i++) {
            RoadCnossosParameters params = new RoadCnossosParameters(
                speed, speed, speed, speed, speed,        // speeds: lv, mv, hgv, wav, wbv
                lvPerHour, mvPerHour, hgvPerHour,         // flows: lv, mv, hgv
                wavPerHour, wbvPerHour,                   // flows: wav, wbv
                FREQUENCIES[i],                            // frequency
                temperature,                               // temperature
                roadSurface,                               // road surface
                tsStud, pmStud, juncDist, juncType         // stud/junction params
            );
            params.setFileVersion(2); // CNOSSOS-EU 2020 coefficients
            params.setSlopePercentage(0); // flat road (no slope info available)

            double lvl = RoadCnossos.evaluate(params);

            // Apply A-weighting
            double lvlA = lvl + A_WEIGHTING[i];
            totalPowerA += Math.pow(10, lvlA / 10.0);
        }

        return 10.0 * Math.log10(Math.max(totalPowerA, 1e-12));
    }
}
