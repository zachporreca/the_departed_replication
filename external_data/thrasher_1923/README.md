# Thrasher 1923 gang map — Chicago, independent digitization

**Date:** 2026-10-07
**Source:** F. M. Thrasher, *Chicago's Gangland* (1923–26), Library of Congress `g4104c.ct003854`,
master TIFF (5529 × 7986 px, 300 dpi).
**Built from scratch.** No earlier digitization (including the ChatGPT one) was used at any step.

---

## What to do with it

The data live in this folder (`external_data/thrasher_1923/`, tracked by git because `input_data/` is
ignored). Run `step_3g_merging_of_thrasher_1923_gang_map.R` **after step 3F**. It loads
   `Chicago_hexgrid.rda`, assigns each symbol to a `hex_id`, and adds five columns to `combined_df`
   by `city_hex`. Rows unchanged; Chicago only; the other ten cities get `NA`; values are constant
   across census years.

| new column | meaning |
|---|---|
| `thrasher_gangs_1923` | all gang symbols in the hex (1,158 on the map) |
| `thrasher_italian_area_gangs_1923` | symbols inside an enclosure the map labels ITALIAN — **the main variable (141)** |
| `thrasher_italian_area_gangs_1923_core` | same, excluding symbols within 60 m of a traced boundary (106) |
| `any_thrasher_italian_area_gang_1923` | 1 if the hex has any of the 141 |
| `italian_area_share_1923` | share of the hex's area inside an ITALIAN enclosure |

Tested end-to-end in R 4.5.2 / sf 1.1.3 against the real `combined_df.rda` and a synthetic grid built
like step 2A: row count preserved, NA outside Chicago, all 141 Italian-area symbols assigned.

## What the map shows, and what the variable means

The map has two separate layers. **Red**: gang symbols (● no clubroom, ▲ clubroom) and Thrasher's
gang-district names. **Grey**: thin enclosures with large spaced lettering naming the nationality of
the *neighbourhood* — ITALIAN, POLISH, JEWISH, GERMAN, LITHUANIAN, etc.

Six enclosures are labelled ITALIAN:

| enclosure | symbols inside | core | ● | ▲ |
|---|---|---|---|---|
| Near North (Little Sicily) | 44 | 33 | 29 | 15 |
| Grand Avenue corridor | 44 | 30 | 37 | 7 |
| Near West (Taylor Street) — 'GREEK' pocket excluded | 43 | 34 | 37 | 6 |
| Armour Square (below Chinatown) | 10 | 9 | 5 | 5 |
| Ogden Park | 0 | 0 | — | — |
| Grand Crossing | 0 | 0 | — | — |
| **Total** | **141** | **106** | 108 | 33 |

`thrasher_italian_area_gangs_1923` counts gangs **located in neighbourhoods Thrasher labels Italian**.
It is not gang ethnicity: the symbols carry no names, and the book's nationality table (Table III,
p. 130) has no locations, so the two cannot be linked.

## Method and measured accuracy

**Symbols.** Red-ink mask → template matching against a filled circle and a filled triangle with an
exclusion ring (separates solid symbols from red handwriting) → shape test (a triangle's top rows are
narrow, a circle's are not). Audit of four random windows (~105 symbols): no false positives, no
misses except at window edges, ~3 circle/triangle swaps in tight stacks of triangles. 1,158 symbols
found; Thrasher's book reports 1,313 gangs studied, so not every gang appears to be plotted.

**Italian areas.** Six enclosures traced by hand on a version of the map with red ink removed.
Boundaries are a reading of a hand-drawn line; symbols within 15 px (60 m) of a boundary are
flagged `near_boundary`. 35 of the 141 are near a boundary; 36 symbols just outside are too.
Range: 106 (core) – 141 (main) – 177 (if all near-boundary symbols outside were counted).

**Georeference.** Street lines identified from the map's own edge labels (21 north–south, 34
east–west streets), 512 intersections snapped automatically, 404 retained by RANSAC; cubic
pixel → Chicago-grid-miles fit. Chicago's south-side house numbers are irregular near downtown
(Roosevelt = 1 mile south of Madison, 22nd = 2, 31st = 3, Pershing = 4, then 800/mile); the fit uses
miles, not house numbers. Grid miles → lat/lon by affine fit to 21 OpenStreetMap intersection nodes spread across the city (Foster to 95th, Cicero to Stony Island).

| step | error |
|---|---|
| map pixel → grid (held-out, 20 × 20% splits) | RMS 22 m, p95 37 m |
| grid → lat/lon (leave-one-out, 21 OSM nodes) | RMS 12 m, max 24 m |
| combined | ≈ 25 m |

A hexagon is 1,386 m flat-to-flat (`cellsize = 800`), so positional error is about 2% of a hexagon.

## Files

| file | |
|---|---|
| `thrasher_1923_gang_symbols.csv` | all 1,158 symbols: lon/lat, symbol type, Italian-area flag, distance to boundary, map pixel coords |
| `thrasher_1923_italian_areas.geojson` | the six ITALIAN enclosures, WGS84 |
| `code/step_3_creation_of_additional_data_sets/step_3g_merging_of_thrasher_1923_gang_map.R` | drop-in merge into `combined_df` |
| `thrasher_1923_QA_italian_areas_zoom.png` | QA: green inside, orange near boundary, blue enclosures, magenta excluded |
