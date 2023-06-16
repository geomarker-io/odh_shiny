library(shiny)
library(tidyverse)
library(bslib)
library(leaflet)
library(gt)
library(sf)
library(waiter)

{
  options(tigris_use_cache = TRUE)
  #d.fit <- read_rds('tract_rates_obs_pred.rds')
  d.fit <- read_csv('ODH_lead_ct_obs_pred.csv', show_col_types = F) |>
    rename(observed = "fraction_elevated_measured",
           predicted = "fraction_elevated_predicted") |>
    mutate(census_tract_fips = as.character(census_tract_fips))

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

  d_slider <- d.fit |>
    mutate(pred_pct = round(predicted*100,2),
           obs_pct = round(observed*100,2))

  #
  # d_slider_t <- left_join(oh_tracts, d.fit, by = 'census_tract_fips')
  #
  # d_slider_z <- sf::st_join(oh_zips, d_slider_t, join = st_overlaps) |>
  #   rbind(sf::st_join(oh_zips, d_slider_t, join = st_covered_by)) |>
  #   st_drop_geometry()
  #
  # d_slider_z_fin <- rbind(
  #   d_slider_z |>
  #     group_by(zcta) |>
  #     slice_min(predicted),
  #   d_slider_z |>
  #     group_by(zcta) |>
  #     slice_max(predicted)
  # )


}

ui <- page_fillable(

  theme = bs_theme(
    version = 5,
    "bg" = "#FFFFFF",
    "primary" = "#ee1d25",
    "fg" = "#700d1b",
    "grid-gutter-width" = "0.0rem",
    "border-radius" = "0.5rem",
    "btn-border-radius" = "0.25rem"
  ),


  use_waiter(),

  title = "Ohio Lead Risk Threshold",

  h2("Ohio Lead Risk Thresholds", class = "bg-dark p-2 mb-0"),

  layout_sidebar(

    sidebar = sidebar(
      width = '20%',


      histoslider::input_histoslider("slider",
                                     "Select Percent Predicted Children with Elevated Tests: ",
                                     d_slider$pred_pct,
                                     end = 10,
                                     breaks = seq(0, 34, by = 1),
                                     options = list(
                                       selectedColor = "#ee1d25"
                                     )
      ),
      splitLayout(
      downloadButton('download_zcta', HTML("Download High Risk </br> ZCTAs")),
      downloadButton('download_tract', HTML("Download High Risk </br> Census Tracts"))
      ),


      # sliderInput('slider', "Select Threshold:",
      #             min = 0,
      #             max = .1,
      #             step = 0.005,
      #             value = 0.05),
      hr(),
      gt_output('table')
    ),

    card(
      height = '100vh',
      card_header("Map"),
      leafletOutput("map")
    )
  )


)


server <- function(input, output, session) {

  w <- Waiter$new(id = c("map", "table"),
                  html = spin_3(),
                  color = transparent(.5))

  cut_off <- reactive({
    input$slider[[2]]/100
  })

  observe({

    w$show()

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
        risk.pred == "low" & risk.measured == "low" ~ "Low risk")
      )

    d_zip_down <- oh_zips_predrisk |>
      sf::st_drop_geometry() |>
      mutate(high_risk = ifelse(risk.pred == 'high' | risk.measured == 'high', TRUE, FALSE)) |>
      select(zcta, high_risk)

    output$download_zcta <- downloadHandler(
      filename = function() {
        paste("high_risk_zctas_", round(input$slider[[2]]/100,2), "_threshold.csv", sep = "")
      },
      content = function(file) {
        write.csv(d_zip_down, file, row.names = FALSE)
      }
    )

    d_tract_down <- d |>
      sf::st_drop_geometry() |>
      mutate(high_risk = ifelse(risk.pred == 'high' | risk.measured == 'high', TRUE, FALSE)) |>
      select(census_tract_fips, high_risk)

    output$download_tract <- downloadHandler(
      filename = function() {
        paste("high_risk_census_tracts_", round(input$slider[[2]]/100,2), "_threshold.csv", sep = "")
      },
      content = function(file) {
        write.csv(d_tract_down, file, row.names = FALSE)
      }
    )

    output$map <- renderLeaflet({

      pal <- c(
        "Predicted high risk" = "#ff0000",
        "Predicted low risk, measured high risk" = "#ff8800",
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

      #w$hide()

      map

    })

    output$table <- render_gt({

      nhgis.in <- read_csv("nhgis0033_ds254_20215_zcta.csv", show_col_types = F)

      n_pop_under_5.nhgis2021 <- nhgis.in |>
        transmute(zcta = ZCTA5A,
                  n_pop_under_5 = AONTE003 + AONTE027) |>
        filter(zcta %in% oh_zips$zcta)

      oh_zips_predrisk |>
        left_join(n_pop_under_5.nhgis2021, by = "zcta") |>
        st_drop_geometry() |>
        group_by(hr_cat) |>
        summarize(n_pop_under_5 = sum(n_pop_under_5),
                  n_zctas_cat = n()) |>
        mutate(pct_kids = round(n_pop_under_5 / sum(n_pop_under_5) * 100 ),
               pct_zctas = round(n_zctas_cat / n_distinct(oh_zips_predrisk$zcta)*100)) |>
        relocate(pct_kids, .before = n_zctas_cat) |>
        gt() |>
        tab_header(title = "Summary") |>
        fmt_number(use_seps = T,
                   columns = c(n_pop_under_5, n_zctas_cat),
                   decimals = 0) |>
        fmt_percent(columns = c(pct_kids, pct_zctas),
                    decimals = 0,
                    scale_values = F) |>
        tab_style(
          style = list(
            cell_fill(
              color = "#ffb3b3"
            )),
          locations = cells_body(
            rows = 2
          )
        ) |>
        tab_style(
          style = list(
            cell_fill(
              color = "#ffdbb3"
            )),
          locations = cells_body(
            rows = 3
          )
        ) |>
        tab_style(
          style = list(
            cell_fill(
              color = "grey90"
            )),
          locations = cells_body(
            rows = 1
          )
        ) |>
        cols_label(hr_cat = "Category",
                   n_pop_under_5 = "Total Kids <5",
                   pct_kids = "Percent",
                   n_zctas_cat = "Number of ZCTAs",
                   pct_zctas = "Percent")

    })

  })
}


shinyApp(ui = ui, server = server)
