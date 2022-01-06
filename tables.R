rr_table <- returner_rankings %>%
  group_by(kickoff_returner_name, season) %>%
  select(kickoff_returner_name, season, ryoe, return_avg, return_med) %>%
  mutate(ryoe = round(ryoe*1000)/1000) %>%
  arrange(desc(ryoe)) %>%
  ungroup()

tr_table <- touchback_rankings %>%
  group_by(kickoff_returner_name, season) %>%
  select(kickoff_returner_name, season, ryoe, return_avg, return_median) %>%
  mutate(ryoe = round(ryoe*1000)/1000) %>%
  arrange(desc(ryoe)) %>%
  ungroup()

k_table <- kicker_data %>%
  ungroup() %>%
  select(kicker_name, season, total_XRY, touchback_percentage, bad_kick_percentage) %>%
  mutate(total_XRY = round(total_XRY*1000)/1000, touchback_percentage = round(touchback_percentage*1000)/10, 
         bad_kick_percentage = round(bad_kick_percentage*1000)/10) %>%
  arrange((total_XRY))

colnames(rr_table) <- c("Returner", "Season", "RYOE",  'Return Average', "Return Median")

colnames(k_table) <- c("Kicker", "Season", "Expected Return Yards",  'Touchback Percentage', "Bad Kick Percentage")

gt(rr_table[1:10,], rowname_col = c("kickoff_returner_name")) %>%
  tab_header(title = "NFL Return Yards over Expected", subtitle = "All Returned Kickoffs 2018-2020") %>%
  tab_stubhead(label = "Returner") %>%
  tab_spanner(label = "Return Yard Stats", columns = c("RYOE",  "Return Average", "Return Median")) %>%
  data_color(
    columns = c("RYOE", "Return Average", "Return Median"),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = c("#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL
    )
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = vars(RYOE)
      )
    )
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  )



gt(k_table[1:10,]) %>%
  tab_header(title = "NFL Kickers", subtitle = "All Kickoffs 2018-2020") %>%
  tab_stubhead(label = "Kicker") %>%
  tab_spanner(label = "Kicking Performance Metrics", columns = c("Expected Return Yards",  "Touchback Percentage", "Bad Kick Percentage")) %>%
  data_color(
    columns = c("Expected Return Yards", "Bad Kick Percentage"),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = c("#35b0ab", "#93d3ab", "#c9ecb4",  "#f2fbd2"),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c("Touchback Percentage"),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = c("#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL
    )
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c("Expected Return Yards")
      )
    )
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  )