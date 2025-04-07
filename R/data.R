#' Murray-Darling Basin Polygon
#'
#' Polygon defining the Murray-Darling Basin boundary. Sourced from [MDBA data
#' website](https://www.mdba.gov.au/publications-and-data/maps-and-spatial-data/spatial-data),
#' with direct link given below.
#'
#' @format ## `basin` An `sf` object with one polygon:
#' \describe{
#'   \item{DDIV_NAME}{Polygon name}
#'   \item{AREA_HA, SHAPE_AREA, SHAPE_LEN}{basic descriptions}
#' }
#' @source
#'   <https://data.gov.au/data/dataset/4ede9aed-5620-47db-a72b-0b3aa0a3ced0/resource/8a6d889d-723b-492d-8c12-b8b0d1ba4b5a/download/sworkingadhocjobsj4430dataoutputsmdb_boundarymdb_boundary.zip>
"basin"

#' Sustainable diversion limits units
#'
#' Polygons defining the boundaries of SDL units. Sourced from [MDBA data
#' website](https://www.mdba.gov.au/publications-and-data/maps-and-spatial-data/spatial-data),
#' with direct link given below. Polygons have been slightly simplified (reduced
#' vertices) to save size.
#'
#' @format ## `sd_units` An `sf` object:
#' \describe{
#'   \item{SWSDLID, SWSDLName}{ID and name of SDL unit}
#'   \item{StateID}{State}
#' }
#' @source
#' <https://data.gov.au/data/dataset/4afa3227-8557-4bb6-944a-6494b28ae160/resource/988ae947-f4dc-4f75-9773-536df71b03ad/download/sworkingadhocjobsp3827dataoutputsupdate-january-2019surface-water-sdl-resource-units.zip>
"sdl_units"

#' Resource plan areas
#'
#' Polygons defining the boundaries of resource plan areas. Sourced from [MDBA data
#' website](https://www.mdba.gov.au/publications-and-data/maps-and-spatial-data/spatial-data),
#' with direct link given below. Polygons have been slightly simplified (reduced
#' vertices) to save size.
#'
#' @format ## `resource_plan_areas` An `sf` object:
#' \describe{
#'   \item{SWWRPANAME, SWWRPACODE}{Name and code for resource plan area}
#'   \item{STATE}{State}
#' }
#' @source
#' <https://data.gov.au/data/dataset/7b0c274f-7f12-4062-9e54-5b8227ca20c4/resource/8369c483-975c-4f54-8eda-a6fbd0ea89ad/download/sworkingadhocjobsp3827dataoutputsupdate-april-2019sw-wrpasurface-water-water-resource-plan-areas.zip>
"resource_plan_areas"

#' Gauge locations
#'
#' Points specifying the locations of gauging stations. Sourced from [MDBA gauge
#' getter](https://pypi.org/project/mdba-gauge-getter/), though a very similar
#' dataset (HydrologicIndicatorSites) is available from the [MDBA data
#' website](https://www.mdba.gov.au/publications-and-data/maps-and-spatial-data/spatial-data).
#' The BOM locations are used here for consistency with the EWR tool. Full list
#' of gauges has been clipped to the `basin` polygon.
#'
#' @format ## `bom_basin_gauges` An `sf` object:
#' \describe{
#'   \item{site, gauge}{Name and code for gauge}
#'   \item{owner}{State from which data is available}
#' }
#' @source
#' CSV within <https://pypi.org/project/mdba-gauge-getter/>
"bom_basin_gauges"

#' Causal relationships for the EWRs
#'
#' List of dataframes specifying the causal relationships from environmental
#' water requirements through the rest of the levels specified in Long-Term
#' Watering Plans. Previously created in HydroBOT, now simply extracted from
#' py_ewr (as of Nov 2024). May not remain here much longer, extracting the EWR
#' tool version with [get_causal_ewr()] is preferable.
#'
#' @format ## `causal_ewr` A list of dataframes:
#' \describe{
#'   \item{ewr2obj}{Mapping from EWRs at each gauge to environmental objectives (`env_obj`)}
#'   \item{obj2target}{Mapping from `env_obj` to `Specific_goal`, `Objective`, and `Target`, which are all defined at the PlanningUnit scale}
#'   \item{obj2yrtarget}{Mapping defined without spatial reference from `env_obj` to `Target`, `Target_Category`, `Objective`, `target_5_year_2024`, `target_10_year_2029`, and `target_20_year_2039`}
#' }
#' @source various Long-Term Watering Plans and State-based tables. Extracted from EWR tool (py_ewr) and reexported here.
"causal_ewr"

#' Catchments within the Murray-Darling Basin
#'
#' Polygons defining catchment boundaries, as defined by the CEWO. Sourced from
#' [MDBA data
#' website](https://www.mdba.gov.au/publications-and-data/maps-and-spatial-data/spatial-data),
#' with direct link given below. The 'Northern Unregulated' polygon has been
#' removed, as it is the combination of several other polygons.
#'
#' @format ## `cewo_valleys` An `sf` object:
#' \describe{
#'   \item{ValleyName, ValleyID, ValleyCode}{Name, ID, and code for each catchment}
#' }
#' @source
#' <https://data.gov.au/data/dataset/75910bc5-6c3e-40e8-9c8a-1e895274badb/resource/a7053ee7-8e20-4f2c-b594-cb88c6ed9406/download/cewo_mdb_valleys.shp.zip>
"cewo_valleys"

#' Planning Units for long-term watering plans
#'
#' Polygons defining planning units. Currently just for NSW, sourced from MDBA
#' Environmental Assets and Functions Database. Have been merged across resource plan areas, allowing them to be split into other, arbitrary polygons
#'
#' @format ## `planning_units` An `sf` object:
#' \describe{
#'   \item{LTWPShortName, PlanningUnitName, geometry}{The long-term watering plan area and planning unit name matching, to the extent possible, the EWR table. Geometry is polygons. Other columns for reference.}
#'   \item{planning_unit_name}{same as PlanningUnitName, but matches HydroBOT naming instead of EWR naming, allowing easier joining to either}
#' }
#' @source
#' MDBA Environmental Assets and Functions Database
"planning_units"

#' River lines in the basin
#'
#' Spatial lines for the major rivers in the basin, from HydroRIVERS. Has been clipped to basin and paths simplified.
#'
#' @format ## `basin_rivers` An `sf` object:
#' \describe{
#'   \item{geometry}{The geometry, all LINESTRING or MULTILINESTRING}
#'   \item{other values}{As documented at https://www.hydrosheds.org/products/hydrorivers}
#' }
#' @source
#' HydroRIVERS, <https://www.hydrosheds.org/products/hydrorivers>
"basin_rivers"


