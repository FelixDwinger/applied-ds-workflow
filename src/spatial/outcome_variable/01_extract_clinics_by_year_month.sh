#!/bin/bash

# osmosis options:
# --tf accept-nodes amenity=hospital

for YEAR in {2015..2024};
do
    for MONTH in 0{1..9} {10..12};
        do
            echo "${YEAR}-${MONTH}"
            osmium time-filter zimbabwe-internal-2025-10-08.osh.pbf --overwrite  "${YEAR}-${MONTH}-01T00:00:00Z" -o "OSM-Zimbabwe-${YEAR}-${MONTH}-01.pbf"

            osmosis \
                --read-pbf file="OSM-Zimbabwe-${YEAR}-${MONTH}-01.pbf" \
                --tf accept-nodes amenity=hospital \
                --used-node \
                --write-pbf "OSM-Zimbabwe-clinics-${YEAR}-${MONTH}-01.pbf"

            rm "OSM-Zimbabwe-${YEAR}-${MONTH}-01.pbf"
        done
done

for YEAR in {2025..2025};
do
    for MONTH in 0{1..2};
        do
            echo "${YEAR}-${MONTH}"
            osmium time-filter zimbabwe-internal-2025-10-08.osh.pbf --overwrite  "${YEAR}-${MONTH}-01T00:00:00Z" -o "OSM-Zimbabwe-${YEAR}-${MONTH}-01.pbf"

            osmosis \
                --read-pbf file="OSM-Zimbabwe-${YEAR}-${MONTH}-01.pbf" \
                --tf accept-nodes amenity=hospital \
                --used-node \
                --write-pbf "OSM-Zimbabwe-clinics-${YEAR}-${MONTH}-01.pbf"

            rm "OSM-Zimbabwe-${YEAR}-${MONTH}-01.pbf"
        done
done

