CREATE MATERIALIZED VIEW mv_conservation_land_metrics AS

WITH base_parcels AS (
    SELECT DISTINCT
        pid,
        geom
    FROM nsnt_conservation_lands 
),

shoreline AS (
    SELECT
        p.pid,
        SUM(
            ST_Length(
                ST_Intersection(
                    ST_Transform(h.geom, 2961),
                    ST_Buffer(ST_Transform(p.geom, 2961), 10)
                )
            )
        ) AS shoreline_length
    FROM base_parcels p
    JOIN ns_hydro_network_lines h
      ON ST_DWithin(
            ST_Transform(p.geom, 2961),
            ST_Transform(h.geom, 2961),
            25
         )
    WHERE h.feat_code IN (
        'WALK20',
        'WALK25',
        'WALKIS10',
        'WARV10',
        'WARV20',
        'WARVIS10',
        'WARVLK20',
        'WARVLKIS10'
    )
    GROUP BY p.pid
),

coastline AS (
    WITH parcel_boundaries AS (
        SELECT
            pid,
            ST_Boundary(geom) AS geom
        FROM base_parcels
    ),

    parcel_boundary_buffers AS (
        SELECT
            pid,
            ST_Buffer(
                geom::geography,
                20
            )::geometry AS geom
        FROM parcel_boundaries
    )

    SELECT
        p.pid,
        SUM(
            ST_Length(
                ST_Transform(
                    ST_Intersection(c.geom, p.geom),
                    2961
                )
            )
        ) AS coastline_length
    FROM parcel_boundary_buffers p
    JOIN nova_scotia_coastline_segments c
      ON c.geom && p.geom
     AND ST_Intersects(c.geom, p.geom)
    GROUP BY p.pid
),

old_growth AS (
    SELECT
        p.pid,
        SUM(
            ST_Area(
                ST_Transform(
                    ST_Intersection(p.geom, f.geom),
                    2961
                )
            )
        ) / 10000.0 AS old_growth_forest_area
    FROM base_parcels p
    JOIN old_forest_potential_index f
      ON ST_Intersects(p.geom, f.geom)
    GROUP BY p.pid
),

karst_forest AS (
    SELECT
        p.pid,
        SUM(
            ST_Area(
                ST_Transform(
                    ST_Intersection(p.geom, pem.geom),
                    2961
                )
            )
        ) / 10000.0 AS karst_forest_area
    FROM base_parcels p
    JOIN pem
      ON ST_Intersects(p.geom, pem.geom)
    WHERE pem.level5 = 'Acadian Karst Forest'
    GROUP BY p.pid
),

freshwater_islands AS (
    SELECT DISTINCT
        p.pid,
        TRUE AS freshwater_island
    FROM base_parcels p
    JOIN ns_hydro_network_lines h
      ON ST_Intersects(p.geom, h.geom)
    WHERE h.feat_code IN (
        'WARVLKIS10',
        'WALKIS10',
        'WARVIS10'
    )
),

coastal_islands AS (
    SELECT
        p.pid,
        TRUE AS coastal_island,
        string_agg(
            DISTINCT i.island_num::text,
            '; '
            ORDER BY i.island_num::text
        ) AS coastal_island_id
    FROM base_parcels p
    JOIN nova_scotia_islands i
      ON ST_Intersects(p.geom, i.geom)
    GROUP BY p.pid
),

waterbird_colonies AS (
    SELECT
        p.pid,
        string_agg(
            DISTINCT b.colony_id::text,
            '; '
            ORDER BY b.colony_id::text
        ) AS waterbird_colony_id
    FROM base_parcels p
    JOIN nova_scotia_bird_colonies b
      ON ST_DWithin(
            ST_Transform(p.geom, 2961),
            ST_Transform(b.geom, 2961),
            120
         )
    GROUP BY p.pid
)

SELECT
    p.pid,
    COALESCE(s.shoreline_length, 0) AS shoreline_length,
    COALESCE(c.coastline_length, 0) AS coastline_length,
    COALESCE(og.old_growth_forest_area, 0) AS old_growth_forest_area,
    COALESCE(kf.karst_forest_area, 0) AS karst_forest_area,
    COALESCE(fi.freshwater_island, FALSE) AS freshwater_island,
    COALESCE(ci.coastal_island, FALSE) AS coastal_island,
    ci.coastal_island_id,
    wb.waterbird_colony_id

FROM base_parcels p
LEFT JOIN shoreline s
    ON p.pid = s.pid
LEFT JOIN coastline c
    ON p.pid = c.pid
LEFT JOIN old_growth og
    ON p.pid = og.pid
LEFT JOIN karst_forest kf
    ON p.pid = kf.pid
LEFT JOIN freshwater_islands fi
    ON p.pid = fi.pid
LEFT JOIN coastal_islands ci
    ON p.pid = ci.pid
LEFT JOIN waterbird_colonies wb
    ON p.pid = wb.pid;

CREATE UNIQUE INDEX mv_conservation_land_metrics_pid_idx
ON mv_conservation_land_metrics (pid);