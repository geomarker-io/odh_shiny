library(shiny)
library(tidyverse)
library(bslib)
library(leaflet)
library(gt)
library(sf)

{
  options(tigris_use_cache = TRUE)
  d.fit <- read_rds('tract_rates_obs_pred.rds')

  # load ohio census tracts
  oh_tracts <- tigris::tracts('ohio',
                              year = 2020) |>
    select(census_tract_fips = GEOID) |>
    st_transform(5072)

  # load ohio zip codes
  oh_zips <- tigris::zctas(starts_with = c('43', '44', '45'),
                           year = 2020) |>
    select(zcta = ZCTA5CE20) |>
    st_transform(5072)
}

ui <- page_navbar(

  theme = bs_theme(
    version = 5,
    "bg" = "#FFFFFF",
    "primary" = "#ee1d25",
    "fg" = "#700d1b",
    "grid-gutter-width" = "0.0rem",
    "border-radius" = "0.5rem",
    "btn-border-radius" = "0.25rem"
  ),

  title = "ODH Blood Lead At-Risk Zip Codes",

  sidebar = sidebar(
    sliderInput('slider', "Select Threshold:",
                min = 0,
                max = .1,
                step = 0.005,
                value = 0.05),
    hr(),
    gt_output('table')
  ),

  nav_panel("",
    card(
      height = '100vh',
      card_header("Map"),
      leafletOutput("map")
    )
  )

)


server <- function(input, output, session) {

    cut_off <- reactive({
      input$slider
    })

    observe({

    d <- d.fit |>
      mutate(risk.pred = ifelse(predicted > cut_off(), 'high', 'low'),
             risk.measured = ifelse(observed > cut_off(), 'high', 'low'))

    d <- left_join(oh_tracts, d, by = 'census_tract_fips')

    high_risk_tracts.pred <- d |>
      filter(risk.pred == 'high') |>
      select(census_tract_fips) |>
      st_drop_geometry() |>
      pull()

    high_risk_tracts.measured <- d |>
      filter(risk.measured == 'high') |>
      select(census_tract_fips) |>
      st_drop_geometry() |>
      pull()

    # use st_overlaps to exclude tracts that only touch at boundaries
    oh_zips_tracts <- sf::st_join(oh_zips, d, join = st_overlaps) |>
      rbind(sf::st_join(oh_zips, d, join = st_covered_by))

    # predicted
    high_risk_zips.pred <- oh_zips_tracts |>
      filter(census_tract_fips %in% high_risk_tracts.pred) |>
      select(zcta) |>
      st_drop_geometry()

    high_risk_zips.pred <- unique(high_risk_zips.pred$zcta)

    # measured
    high_risk_zips.measured <- oh_zips_tracts |>
      filter(census_tract_fips %in% high_risk_tracts.measured) |>
      select(zcta) |>
      st_drop_geometry()

    high_risk_zips.measured <- unique(high_risk_zips.measured$zcta)

    oh_zips_predrisk <- oh_zips |>
      mutate(risk.pred = ifelse(zcta %in% high_risk_zips.pred, 'high', 'low'),
             risk.measured = ifelse(zcta %in% high_risk_zips.measured, 'high', 'low'))

    oh_zips_predrisk <- oh_zips_predrisk |>
      mutate(hr_cat = case_when(
        risk.pred == "high" ~ "Predicted high risk",
        risk.pred == "low" & risk.measured == "high" ~ "Predicted low risk, measured high risk",
        risk.pred == "low" & risk.measured == "low" ~ "Low risk"
      )
      )

    output$map <- renderLeaflet({

      pal <- c(
        "Predicted high risk" = "firebrick3",
        "Predicted low risk, measured high risk" = "orange",
        "Low risk" = "grey70"
      )

      map_pal <- colorFactor(pal, factor(oh_zips_predrisk$hr_cat,
                                         levels = c(
                                           "Predicted high risk",
                                           "Predicted low risk, measured high risk",
                                           "Low risk"
                                         )))

      oh_zips_predrisk <- st_transform(oh_zips_predrisk, 4326)

      map <-
        leaflet(oh_zips_predrisk) |>
        addProviderTiles(provider = providers$CartoDB.Positron) |>
        addPolygons(fillColor = ~map_pal(hr_cat),
                    fillOpacity = 0.7,
                    stroke = T,
                    label = ~zcta,
                    weight = .5, color = "#333333") |>
        removeLayersControl() |>
        addLegend("topright",
                  pal = map_pal,
                  values = ~hr_cat,
                  title = "Category")

      map

    })

    output$table <- render_gt({
      nhgis.in <- read_csv("nhgis0033_ds254_20215_zcta.csv")

      n_pop_under_5.nhgis2021 <- nhgis.in |>
        transmute(zcta = ZCTA5A,
                  n_pop_under_5 = AONTE003 + AONTE027) |>
        filter(zcta %in% oh_zips$zcta)

      oh_zips_predrisk |>
        left_join(n_pop_under_5.nhgis2021, by = "zcta") |>
        st_drop_geometry() |>
        group_by(hr_cat) |>
        summarize(n_pop_under_5 = sum(n_pop_under_5)) |>
        mutate(pct = round(n_pop_under_5 / sum(n_pop_under_5) * 100 ))

    })

    })







}


shinyApp(ui = ui, server = server)
