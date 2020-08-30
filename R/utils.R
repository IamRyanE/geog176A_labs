get_conus = function(data, var){
  filter(data, !get(var) %in% 
           c("Hawaii", "Puerto Rico", "Alaska",
             "Guam", "District of Columbia")) %>% 
    st_transform(5070)}

point_in_polygon = function(points, polygon, nam){
  st_join(polygon, points) %>% 
    st_drop_geometry() %>% 
    count(get(nam)) %>% 
    setNames(c(nam, "n")) %>% 
    left_join(polygon, by = nam) %>% 
    st_as_sf()
}

plot_pip = function(data){
  ggplot() + 
    geom_sf(data = data, aes(fill = log(n)), alpha = .9, size = .2) + 
    scale_fill_gradient(low = "white", high = "darkgreen") + 
    theme_map() + 
    theme(legend.position = 'none',
          plot.title = element_text(face = "bold", color = "darkblue", hjust = .5, size = 24)) +
    labs(title = paste0("Number of Cities in ", data$state_name),
         caption = paste0(sum(data$n), " cities")) 
}
map_nearest_states = function(name){

states = USAboundaries::us_states() %>% 
  st_transform(4326)

state.of.interest = name

soi = filter(states, state_name == state.of.interest)

adjoining = st_filter(states, soi, .predicate = st_touches)

closest = st_make_grid(soi, n = 70, square = TRUE) %>% 
  st_centroid() %>% 
  st_as_sf() %>% 
  st_join(adjoining, join = st_nearest_feature)

vor = closest %>% 
  st_union() %>% 
  st_voronoi() %>%
  st_cast() %>% 
  st_sf() %>% 
  st_join(closest) %>% 
  group_by(state_name) %>% 
  summarise() %>% 
  st_intersection(soi)


leaflet() %>% 
  addProviderTiles(providers$CartoDB) %>% 
  addPolygons(data = st_transform(vor, 4326), 
              fillColor = ~colorFactor("YlOrRd",state_name)(state_name), 
              weight = .5, 
              color = "black") %>% 
  addPolygons(data = st_transform(soi, 4326),
              fillColor = "transparent",
              color = "black",
              group = "SOI") %>% 
  addPolygons(data = st_transform(adjoining, 4326),
              fillColor = ~colorFactor("YlOrRd",state_name)(state_name),
              col = NA) %>% 
  addLayersControl(overlayGroups = c("SOI"))
}