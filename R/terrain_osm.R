#' Create 3D terrain with OpenStreamMap data overlaying.
#'
#' @param center_ll, Center longitude and latitude, c(lon, lat).
#' @param half_width, Half width of map region in degrees.
#' @param elevat_z, The zoom level to return. The zoom ranges from 1 to 14.
#'                  Resolution of the resultant raster is determined by
#'                  the zoom and latitude.
#' @param add_highway, add highway lines.
#' @param add_waterway, add waterway lines.
#' @param add_buildings, add building polygons, not recommend for large city.
#' @param add_parkings, add parking polygons.
#' @param add_tourisms, add tourism polygons.
#' @param add_places, add place labels.
#' @param plot_3d, plot 3d map, if not, plot 2d map.
#' @param plot_3d_zscale, Default '1'. The ratio between the x and y spacing
#'                        (which are assumed to be equal) and the z axis.
#'                        For example, if the elevation levels are in units
#'                        of 1 meter and the grid values are separated by
#'                        10 meters, 'zscale' would be 10. Adjust the zscale
#'                        down to exaggerate elevation features.
#'
#' @return rayshader graphics.
#' @importFrom magrittr %>%
#' @importFrom raster %in%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' create_terrain_osm()
create_terrain_osm <- function(center_ll      = c(102.929, 30.150),
                               half_width     = 0.05,
                               elevat_z       = 12,
                               add_highway    = TRUE,
                               add_waterway   = TRUE,
                               add_buildings  = FALSE,
                               add_parkings   = FALSE,
                               add_tourisms   = FALSE,
                               add_places     = TRUE,
                               plot_3d        = TRUE,
                               plot_3d_zscale = 20) {

  # Create bounding box---------------------------------------------------------

  med_bbox <- sf::st_bbox(
    c(xmin = center_ll[1]-half_width, xmax = center_ll[1]+half_width,
      ymin = center_ll[2]-half_width, ymax = center_ll[2]+half_width),
      crs = 4326)

  med_bbox_df <- data.frame(
    x = c(center_ll[1]-half_width, center_ll[1]+half_width),
    y = c(center_ll[2]-half_width, center_ll[2]+half_width))

  extent_zoomed <- raster::extent(med_bbox)

  # Get elevation data----------------------------------------------------------

  prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

  elev_med <- elevatr::get_elev_raster(
    med_bbox_df, prj=prj_dd, z=elevat_z, clip="bbox")

  elev_med_mat <- rayshader::raster_to_matrix(elev_med)

  # Get OpenStreetMap(OSM) data-------------------------------------------------

  # https://wiki.openstreetmap.org/wiki/Map_features#Highway
  if (add_highway) {
    med_highway <- med_bbox %>%  osmdata::opq() %>%
      osmdata::add_osm_feature("highway",
        c("motorway", "trunk", "primary", "secondary", "tertiary")) %>%
      osmdata::osmdata_sf()
    med_highway_lines <- med_highway$osm_lines
  }

  # Get river data
  # https://wiki.openstreetmap.org/wiki/Map_features#Waterway
  if (add_waterway) {
    med_rivers <- med_bbox %>% osmdata::opq() %>%
      osmdata::add_osm_feature("waterway",
        c("river", "stream")) %>%
      osmdata::osmdata_sf()
    med_river_lines <- med_rivers$osm_lines
  }

  # Get buildings data
  if (add_buildings) {
    med_buildings <- med_bbox %>% osmdata::opq() %>%
      osmdata::add_osm_feature("building") %>%
      osmdata::osmdata_sf()
    med_building_polygons <- med_buildings$osm_polygons
  }

  # Get parking data
  if (add_parkings) {
    med_parkdings <- med_bbox %>% osmdata::opq() %>%
      osmdata::add_osm_feature("parking") %>%
      osmdata::osmdata_sf()
    med_parking_polygons <- med_parkdings$osm_polygons
  }

  # Get tourism data
  if (add_tourisms) {
    med_tourisms <- med_bbox %>% osmdata::opq() %>%
      osmdata::add_osm_feature("tourism") %>%
      osmdata::osmdata_sf()
    med_tourism_polygons <- med_tourisms$osm_polygons
  }

  # Get place data
  if (add_places) {
    med_places <- med_bbox %>% osmdata::opq() %>%
      osmdata::add_osm_feature("place") %>%
      osmdata::osmdata_sf()
    med_places_points <- med_places$osm_points
  }

  # Create base map-------------------------------------------------------------

  base_map <- elev_med_mat %>%
    rayshader::height_shade() %>%
    rayshader::add_overlay(rayshader::sphere_shade(
        elev_med_mat, texture = "desert", colorintensity = 5),
      alphalayer=0.5) %>%
    rayshader::add_shadow(rayshader::lamb_shade(elev_med_mat), 0) %>%
    rayshader::add_shadow(rayshader::ambient_shade(elev_med_mat),0) %>%
    rayshader::add_shadow(rayshader::texture_shade(
        elev_med_mat, detail=8/10, contrast=9, brightness = 11), 0.1)

  # Overlay OpenStreetMap(OSM) layer--------------------------------------------

  # Add highway layer
  if (add_highway) {
    med_road_01 <- med_highway_lines %>%
      dplyr::filter(.data$highway %in% c("motorway","trunk"))
    if (length(med_road_01$osm_id) > 0) {
      base_map <- base_map %>%
        rayshader::add_overlay(rayshader::generate_line_overlay(
          med_road_01, extent=extent_zoomed,
          linewidth=3, color="white", heightmap = elev_med_mat))}

    med_road_02 <- med_highway_lines %>%
      dplyr::filter(.data$highway %in% c("primary", "secondary"))
    if (length(med_road_02$osm_id) > 0) {
      base_map <- base_map %>%
        rayshader::add_overlay(rayshader::generate_line_overlay(
          med_road_02, extent=extent_zoomed,
          linewidth = 1, color="white", lty=3, heightmap=elev_med_mat))}

    med_road_03 <- med_highway_lines %>%
      dplyr::filter(.data$highway %in% c("tertiary"))
    if (length(med_road_03$osm_id) > 0) {
      base_map <- base_map %>%
        rayshader::add_overlay(rayshader::generate_line_overlay(
          med_road_03, extent=extent_zoomed,
          linewidth=2, color="white", heightmap=elev_med_mat))}
  }

  # Add waterway layer
  if (add_waterway) {
    med_river_01 <- med_river_lines %>%
      dplyr::filter(.data$waterway %in% c("river"))
    if (length(med_river_01$osm_id) > 0) {
      base_map <- base_map %>%
        rayshader::add_overlay(rayshader::generate_line_overlay(
          med_river_01, extent = extent_zoomed,
          linewidth = 2, color="skyblue2",
          heightmap = elev_med_mat))}

    med_river_02 <- med_river_lines %>%
      dplyr::filter(.data$waterway %in% c("stream"))
    if (length(med_river_02$osm_id) > 0) {
      base_map <- base_map %>%
        rayshader::add_overlay(rayshader::generate_line_overlay(
          med_river_02, extent = extent_zoomed,
          linewidth = 1, color="skyblue2", lty=3,
          heightmap = elev_med_mat))}
  }

  # Add polygon layer
  if (add_buildings) {
    if (length(med_building_polygons$osm_id) >0) {
      base_map <- base_map %>%
        rayshader::add_overlay(rayshader::generate_polygon_overlay(
          med_building_polygons, extent = extent_zoomed,
          palette="darkred", heightmap = elev_med_mat))}
  }

  if (add_parkings) {
    if (length(med_parking_polygons$osm_id) >0) {
      base_map <- base_map %>%
        rayshader::add_overlay(rayshader::generate_polygon_overlay(
          med_parking_polygons, extent = extent_zoomed,
          palette="grey30", heightmap = elev_med_mat))}
  }

  if (add_tourisms) {
    if (length(med_tourism_polygons$osm_id) >0) {
      base_map <- base_map %>%
        rayshader::add_overlay(rayshader::generate_polygon_overlay(
          med_tourism_polygons, extent = extent_zoomed,
          palette="darkgreen", heightmap = elev_med_mat))}
  }

  # Add place label
  if (add_places) {
    med_places_points <- med_places_points %>%
      dplyr::filter(.data$place %in% c("town","city"))
    if (length(med_places_points$osm_id) >0) {
      base_map <- base_map %>%
        rayshader::add_overlay(rayshader::generate_label_overlay(
          med_places_points, extent = extent_zoomed,
          text_size = 1, point_size = 1, color = "white",
          halo_color = "black", halo_expand = 10,
          halo_blur = 20, halo_alpha = 0.8,
          seed=1, heightmap = elev_med_mat, data_label_column = "name.en"))}
  }

  # Display plot----------------------------------------------------------------
  if (plot_3d) {
    rayshader::plot_3d(base_map, elev_med_mat,
                       windowsize=c(1200,1100), zscale=plot_3d_zscale,
                       theta=20, phi=30, fov=45, zoom=0.6)
  } else {
    rayshader::plot_map(base_map)
  }

  return(base_map)
}
