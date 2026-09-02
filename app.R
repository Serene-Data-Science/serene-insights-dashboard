##### Packages #####
library(dplyr)
library(tidyr)
library(shiny)
library(bslib)
library(arrow)
library(ggplot2)
library(scales)
library(plotly)

##### Brand Theme #####
#-- from serene-brand-guidelines.pdf, matching the local-authority-dashboard styling
v_brand_black <- '#1D1D1B'
v_brand_purple <- '#4C00DA'
v_brand_blue <- '#5429FF'
v_brand_grey <- '#DFE3FF'

theme_serene <- bs_theme(
  version = 5,
  bg = '#FFFFFF',
  fg = v_brand_black,
  primary = v_brand_purple,
  secondary = v_brand_grey,
  base_font = font_google('Montserrat'),
  heading_font = font_google('Montserrat', wght = '700')
) |>
  bs_add_rules('
    .navbar.navbar-static-top {
      background: linear-gradient(90deg, #4C00DA 0%, #5429FF 100%);
    }
    .navbar-brand.bslib-page-title {
      color: #FFFFFF !important;
      font-weight: 700 !important;
    }
    .card-header {
      font-weight: 700;
      color: #1D1D1B;
    }
    .bslib-value-box {
      background-color: #DFE3FF !important;
      color: #1D1D1B !important;
    }
    .bslib-value-box .value-box-title {
      color: #1D1D1B !important;
      opacity: 0.75;
    }
  ')

#----- Helpers

#-- display-only case conversion (e.g. 'cash_reliant' -> 'Cash Reliant'); never applied to the
#-- underlying snake_case values used for filtering/joins or hardcoded input comparisons.
#-- tools::toTitleCase() lowercases leading "small words" (e.g. 'over', 'a') even when they're the
#-- first word, so the first letter is force-capitalized afterward
format_title_case <- function(x) {
  x <- gsub('_', ' ', x)
  x <- tools::toTitleCase(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#-- imd_dec's segment_value is numeric-as-string ('1.0'..'10.0'); sorting that alphabetically would
#-- put '10.0' before '2.0', so numeric segments are sorted numerically first. Levels must be the
#-- original strings (not reformatted from the numeric value), or they won't match segment_value's
#-- actual values and factor() will turn everything NA.
#-- salary_band ('< 10K', '10K to 20K', ..., '> 80K') isn't fully numeric, but ordering by the first
#-- number found in each label still gets it right, as long as '< 10K' (which shares its only number,
#-- 10, with '10K to 20K') is nudged just below it. Anything with no numbers at all (gender, etc.)
#-- falls back to alphabetical, same as before.
order_segment_levels <- function(x) {
  v_unique <- unique(x)
  v_numeric <- suppressWarnings(as.numeric(v_unique))
  if (!anyNA(v_numeric)) {
    return(v_unique[order(v_numeric)])
  }

  v_leading_num <- suppressWarnings(vapply(v_unique, function(s) {
    m <- regmatches(s, regexpr('[0-9]+\\.?[0-9]*', s))
    if (length(m) == 0) NA_real_ else as.numeric(m)
  }, numeric(1), USE.NAMES = FALSE))

  if (!anyNA(v_leading_num)) {
    v_key <- v_leading_num - 0.5 * grepl('^\\s*<', v_unique)
    return(v_unique[order(v_key)])
  }

  sort(v_unique)
}

##### Data Reads #####

#-- data pulled from Databricks by data-prep.R and cached here as local files, so the deployed app
#-- never hits Databricks (or needs mixtape) at launch -- rerun data-prep.R to refresh
ds_id_summary <- read_parquet('data/id_summary.parquet')
ds_id_risk_bands <- read_parquet('data/id_risk_bands.parquet')
ds_id_distribution <- read_parquet('data/id_distribution.parquet')
ds_id_summary_snapshot <- read_parquet('data/id_summary_snapshot.parquet')
ds_transaction_distribution <- read_parquet('data/transaction_distribution.parquet')
ds_macro <- read_parquet('data/macro_indicators.parquet')

mix_palette <- readRDS('data/mix_palette.rds')

#-- floor also enforced here (not just in data-prep.R's queries), so a stale cache built before this
#-- floor existed can't leak pre-2019 data into the app
v_min_month <- '2019-01'
ds_id_summary <- ds_id_summary |> filter(month >= v_min_month)
ds_id_risk_bands <- ds_id_risk_bands |> filter(month >= v_min_month)
ds_id_distribution <- ds_id_distribution |> filter(month >= v_min_month)
ds_transaction_distribution <- ds_transaction_distribution |> filter(month >= v_min_month)
ds_macro <- ds_macro |> filter(month >= v_min_month)

##### Data Transforms #####

#----- Filters

v_segment_choices <- c(
  'Age Band' = 'age_band',
  'Salary Band' = 'salary_band',
  'Gender' = 'derived_gender',
  'IMD Decile' = 'imd_dec'
)

#-- month range selector; built from every month present across both source tables so the
#-- selector covers full coverage even if the tables' ranges differ
v_month_choices <- c(ds_id_summary$month, ds_transaction_distribution$month) |>
  unique() |>
  sort()

v_month_dates <- as.Date(paste0(v_month_choices, '-01'))

#----- SereneScore

#-- ordinal factor drives legend order (1-Critical -> 4-Low)
v_risk_band_levels <- c('1-Critical', '2-High', '3-Medium', '4-Low')

ds_id_risk_bands <- ds_id_risk_bands |>
  mutate(p_risk_band_serene_score = factor(p_risk_band_serene_score, levels = v_risk_band_levels))

#-- risk severity read as status colour (mix_palette), green (safe) -> red (critical)
v_risk_band_colors <- c(
  '1-Critical' = mix_palette$red[1],
  '2-High' = mix_palette$orange[1],
  '3-Medium' = mix_palette$yellow[1],
  '4-Low' = mix_palette$green[1]
)

#----- Transactions

v_tag_choices <- ds_transaction_distribution$serene_tag_final |> unique() |> sort()

v_tag_default <- if ('overall' %in% v_tag_choices) 'overall' else v_tag_choices[1]

#----- Macro

#-- ordered so each indicator's variants stay grouped, Level first, in the dropdown
v_series_order <- c('Level', '1-Month Lag', 'M/M Diff', '3-Month Avg')

ds_macro <- ds_macro |>
  mutate(indicator_series = paste0(indicator, ' — ', series))

v_macro_choices <- ds_macro |>
  distinct(indicator, series, indicator_series) |>
  arrange(indicator, match(series, v_series_order)) |>
  pull(indicator_series)
names(v_macro_choices) <- v_macro_choices

##### UI #####
ui <- page_sidebar(
  title = span(
    style = 'color: #FFFFFF; font-weight: 700;',
    'Serene Insights'
  ),
  theme = theme_serene,
  sidebar = sidebar(
    bg = v_brand_grey,
    selectInput(
      inputId = 'segment_type',
      label = 'Segment By',
      choices = v_segment_choices,
      selected = v_segment_choices[1]
    ),
    selectInput(
      inputId = 'min_month',
      label = 'From Month',
      choices = setNames(v_month_choices, format(v_month_dates, '%B %Y')),
      selected = v_month_choices[1]
    ),
    selectInput(
      inputId = 'max_month',
      label = 'To Month',
      choices = setNames(v_month_choices, format(v_month_dates, '%B %Y')),
      selected = v_month_choices[length(v_month_choices)]
    )
  ),
  navset_tab(
    id = 'main_tabs',
    nav_panel(
      title = 'SereneScore',
      layout_column_wrap(
        width = '250px',
        value_box(
          title = 'Unique Customers',
          value = textOutput('score_customers')
        ),
        value_box(
          title = '% Users with a Serene ID',
          value = textOutput('score_pct_id')
        ),
        value_box(
          title = 'Financial Distress Rate',
          value = textOutput('score_fin_distress')
        )
      ),
      card(
        card_header('Risk Band Mix by Month'),
        plotlyOutput('risk_band_stack', height = '380px')
      ),
      card(
        card_header(textOutput('fin_distress_header', inline = T)),
        plotlyOutput('fin_distress_trend', height = '380px')
      ),
      card(
        card_header(textOutput('pct_id_header', inline = T)),
        plotlyOutput('pct_id_trend', height = '380px')
      ),
      card(
        card_header('Top Serene IDs (Latest Month)'),
        tableOutput('top_serene_ids_table')
      )
    ),
    nav_panel(
      title = 'Transactions',
      div(
        style = 'min-width: 260px; margin-bottom: 12px;',
        selectInput(
          inputId = 'serene_tag',
          label = 'Serene Tag',
          choices = setNames(v_tag_choices, format_title_case(v_tag_choices)),
          selected = v_tag_default,
          width = '100%'
        )
      ),
      card(
        card_header('Avg. Debit vs. Credit per User by Month'),
        plotlyOutput('debit_credit_trend', height = '380px')
      ),
      card(
        card_header(textOutput('debit_header', inline = T)),
        plotlyOutput('debit_trend', height = '380px')
      ),
      card(
        card_header(textOutput('cashflow_header', inline = T)),
        plotlyOutput('cashflow_trend', height = '380px')
      ),
      card(
        card_header(textOutput('tr_breakdown_header', inline = T)),
        tableOutput('tr_breakdown_table')
      )
    ),
    nav_panel(
      title = 'Macro Overlay',
      div(
        style = 'min-width: 260px; margin-bottom: 12px;',
        selectInput(
          inputId = 'macro_indicator_series',
          label = 'Macro Indicator',
          choices = v_macro_choices,
          selected = v_macro_choices[1],
          width = '100%'
        )
      ),
      card(
        card_header(textOutput('macro_overlay_header', inline = T)),
        plotlyOutput('macro_overlay_trend', height = '460px')
      ),
      card(
        card_header(textOutput('macro_overlay_segmented_header', inline = T)),
        plotlyOutput('macro_overlay_segmented_trend', height = '460px')
      )
    )
  )
)

##### Server #####
server <- function(input, output, session) {

  #----- Filters

  #-- sorted so a user picking From > To still filters a valid (swapped) range rather than an empty one
  v_month_range <- reactive({
    req(input$min_month, input$max_month)
    sort(c(input$min_month, input$max_month))
  })

  v_segment_label <- reactive({
    names(v_segment_choices)[v_segment_choices == input$segment_type]
  })

  #----- SereneScore

  ds_score_overall <- reactive({
    ds_id_summary |>
      filter(
        segment_type == 'overall',
        month >= v_month_range()[1],
        month <= v_month_range()[2]
      )
  })

  #-- from id_summary_snapshot (one row per user, at their own latest month) rather than filtering
  #-- id_summary to the latest month across the whole table -- see data-prep.R for why
  output$score_customers <- renderText({
    comma(as.numeric(ds_id_summary_snapshot$total_users[1]))
  })

  output$score_pct_id <- renderText({
    percent(ds_id_summary_snapshot$pct_users_with_id[1], accuracy = 0.1)
  })

  output$score_fin_distress <- renderText({
    percent(ds_id_summary_snapshot$fin_distress_rate[1], accuracy = 0.1)
  })

  ds_score_risk_bands <- reactive({
    ds_id_risk_bands |>
      filter(
        month >= v_month_range()[1],
        month <= v_month_range()[2]
      )
  })

  output$risk_band_stack <- renderPlotly({
    ds_plot <- ds_score_risk_bands() |>
      mutate(
        tooltip_text = paste0(
          p_risk_band_serene_score, '<br>',
          month, '<br>',
          '% of Users: ', percent(users_pct, accuracy = 0.1), '<br>',
          '# Users: ', comma(as.numeric(users))
        )
      )

    v_gg <- ggplot(ds_plot, aes(x = month, y = users_pct, fill = p_risk_band_serene_score, text = tooltip_text)) +
      geom_col(position = position_stack(), width = 0.7, colour = 'white', linewidth = 1) +
      scale_fill_manual(values = v_risk_band_colors, name = 'Risk Band') +
      scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
      labs(x = NULL, y = '% of Users') +
      theme_minimal(base_size = 13, base_family = 'Montserrat') +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(colour = '#1D1D1B'),
        axis.title = element_text(colour = '#1D1D1B')
      )

    ggplotly(v_gg, tooltip = 'text')
  })

  ds_score_segmented <- reactive({
    req(input$segment_type)
    ds_id_summary |>
      filter(
        segment_type == input$segment_type,
        !is.na(segment_value),
        month >= v_month_range()[1],
        month <= v_month_range()[2]
      ) |>
      mutate(segment_value = factor(segment_value, levels = order_segment_levels(segment_value)))
  })

  output$fin_distress_header <- renderText({
    paste0('Financial Distress Rate by Month, by ', v_segment_label())
  })

  output$fin_distress_trend <- renderPlotly({
    ds_plot <- ds_score_segmented() |>
      mutate(
        tooltip_text = paste0(
          v_segment_label(), ': ', segment_value, '<br>',
          month, '<br>',
          'Fin. Distress Rate: ', percent(fin_distress_rate, accuracy = 0.1), '<br>',
          '# Users: ', comma(as.numeric(total_users))
        )
      )

    v_gg <- ggplot(ds_plot, aes(x = month, y = fin_distress_rate, colour = segment_value, group = segment_value, text = tooltip_text)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 2) +
      scale_colour_viridis_d(name = v_segment_label()) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(x = NULL, y = 'Fin. Distress Rate') +
      theme_minimal(base_size = 13, base_family = 'Montserrat') +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(colour = '#1D1D1B'),
        axis.title = element_text(colour = '#1D1D1B')
      )

    ggplotly(v_gg, tooltip = 'text')
  })

  output$pct_id_header <- renderText({
    paste0('% of Users with a Serene ID, by Month, by ', v_segment_label())
  })

  output$pct_id_trend <- renderPlotly({
    ds_plot <- ds_score_segmented() |>
      mutate(
        tooltip_text = paste0(
          v_segment_label(), ': ', segment_value, '<br>',
          month, '<br>',
          '% Users with ID: ', percent(pct_users_with_id, accuracy = 0.1), '<br>',
          '# Users: ', comma(as.numeric(total_users))
        )
      )

    v_gg <- ggplot(ds_plot, aes(x = month, y = pct_users_with_id, colour = segment_value, group = segment_value, text = tooltip_text)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 2) +
      scale_colour_viridis_d(name = v_segment_label()) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(x = NULL, y = '% Users with ID') +
      theme_minimal(base_size = 13, base_family = 'Montserrat') +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(colour = '#1D1D1B'),
        axis.title = element_text(colour = '#1D1D1B')
      )

    ggplotly(v_gg, tooltip = 'text')
  })

  output$top_serene_ids_table <- renderTable({
    v_latest_month <- max(ds_score_overall()$month)
    ds_id_distribution |>
      filter(month == v_latest_month) |>
      arrange(desc(total_pct)) |>
      transmute(
        'Serene ID' = format_title_case(serene_id),
        'Type' = format_title_case(serene_id_type),
        'Category' = format_title_case(serene_id_category),
        'Users' = comma(as.numeric(users_vol)),
        '% of Users' = percent(total_pct, accuracy = 0.1),
        'Fin. Distress Rate' = percent(fin_distress_rate, accuracy = 0.1)
      )
  }, striped = T, hover = T, width = '100%')

  #----- Transactions

  ds_tr_overall <- reactive({
    req(input$serene_tag)
    ds_transaction_distribution |>
      filter(
        segment_type == 'overall',
        serene_tag_final == input$serene_tag,
        month >= v_month_range()[1],
        month <= v_month_range()[2]
      )
  })

  output$debit_credit_trend <- renderPlotly({
    ds_plot <- ds_tr_overall() |>
      transmute(
        month,
        'Avg. Debit' = avg_debit_amount_per_user,
        'Avg. Credit' = avg_credit_amount_per_user
      ) |>
      tidyr::pivot_longer(cols = c('Avg. Debit', 'Avg. Credit'), names_to = 'metric', values_to = 'value') |>
      mutate(
        tooltip_text = paste0(
          metric, '<br>',
          month, '<br>',
          'Amount: ', dollar(value, prefix = '£', accuracy = 1)
        )
      )

    v_gg <- ggplot(ds_plot, aes(x = month, y = value, colour = metric, group = metric, text = tooltip_text)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 2) +
      scale_colour_manual(values = c('Avg. Debit' = mix_palette$red[1], 'Avg. Credit' = v_brand_blue), name = NULL) +
      scale_y_continuous(labels = label_dollar(prefix = '£')) +
      labs(x = NULL, y = 'Amount per User') +
      theme_minimal(base_size = 13, base_family = 'Montserrat') +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(colour = '#1D1D1B'),
        axis.title = element_text(colour = '#1D1D1B')
      )

    ggplotly(v_gg, tooltip = 'text')
  })

  ds_tr_segmented <- reactive({
    req(input$segment_type, input$serene_tag)
    ds_transaction_distribution |>
      filter(
        segment_type == input$segment_type,
        serene_tag_final == input$serene_tag,
        !is.na(segment_value),
        month >= v_month_range()[1],
        month <= v_month_range()[2]
      ) |>
      mutate(segment_value = factor(segment_value, levels = order_segment_levels(segment_value)))
  })

  output$debit_header <- renderText({
    paste0('Avg. Debit per User by Month, by ', v_segment_label())
  })

  output$debit_trend <- renderPlotly({
    ds_plot <- ds_tr_segmented() |>
      mutate(
        tooltip_text = paste0(
          v_segment_label(), ': ', segment_value, '<br>',
          month, '<br>',
          'Avg. Debit: ', dollar(avg_debit_amount_per_user, prefix = '£', accuracy = 1), '<br>',
          '# Users: ', comma(as.numeric(users_vol))
        )
      )

    v_gg <- ggplot(ds_plot, aes(x = month, y = avg_debit_amount_per_user, colour = segment_value, group = segment_value, text = tooltip_text)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 2) +
      scale_colour_viridis_d(name = v_segment_label()) +
      scale_y_continuous(labels = label_dollar(prefix = '£')) +
      labs(x = NULL, y = 'Avg. Debit per User') +
      theme_minimal(base_size = 13, base_family = 'Montserrat') +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(colour = '#1D1D1B'),
        axis.title = element_text(colour = '#1D1D1B')
      )

    ggplotly(v_gg, tooltip = 'text')
  })

  output$cashflow_header <- renderText({
    paste0('Avg. Cashflow per User by Month, by ', v_segment_label())
  })

  output$cashflow_trend <- renderPlotly({
    ds_plot <- ds_tr_segmented() |>
      mutate(
        tooltip_text = paste0(
          v_segment_label(), ': ', segment_value, '<br>',
          month, '<br>',
          'Avg. Cashflow: ', dollar(avg_cashflow_per_user, prefix = '£', accuracy = 1), '<br>',
          '# Users: ', comma(as.numeric(users_vol))
        )
      )

    v_gg <- ggplot(ds_plot, aes(x = month, y = avg_cashflow_per_user, colour = segment_value, group = segment_value, text = tooltip_text)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 2) +
      scale_colour_viridis_d(name = v_segment_label()) +
      scale_y_continuous(labels = label_dollar(prefix = '£')) +
      labs(x = NULL, y = 'Avg. Cashflow per User') +
      theme_minimal(base_size = 13, base_family = 'Montserrat') +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(colour = '#1D1D1B'),
        axis.title = element_text(colour = '#1D1D1B')
      )

    ggplotly(v_gg, tooltip = 'text')
  })

  output$tr_breakdown_header <- renderText({
    paste0('Breakdown by ', v_segment_label(), ' (Latest Month)')
  })

  output$tr_breakdown_table <- renderTable({
    v_latest_month <- max(ds_tr_segmented()$month)
    ds_tr_segmented() |>
      filter(month == v_latest_month) |>
      arrange(desc(users_vol)) |>
      transmute(
        !!v_segment_label() := as.character(segment_value),
        'Users' = comma(as.numeric(users_vol)),
        'Avg. Debit' = dollar(avg_debit_amount_per_user, prefix = '£', accuracy = 1),
        'Avg. Credit' = dollar(avg_credit_amount_per_user, prefix = '£', accuracy = 1),
        'Avg. Cashflow' = dollar(avg_cashflow_per_user, prefix = '£', accuracy = 1)
      )
  }, striped = T, hover = T, width = '100%')

  #----- Macro Overlay

  #-- national trend, unsegmented -- distinct from ds_score_segmented() (used by the SereneScore
  #-- tab's segmented chart), so this stays a single line regardless of the sidebar's segment_type
  ds_macro_selected <- reactive({
    req(input$macro_indicator_series)
    ds_macro |>
      filter(
        indicator_series == input$macro_indicator_series,
        month >= v_month_range()[1],
        month <= v_month_range()[2]
      )
  })

  output$macro_overlay_header <- renderText({
    req(input$macro_indicator_series)
    paste0('Financial Distress Rate (National, Overall) vs. ', input$macro_indicator_series)
  })

  #-- ggplot's sec_axis only supports a fixed linear transform between the two axes, which can't work
  #-- here since the macro series' scale varies arbitrarily by indicator -- built directly in plotly
  #-- instead, which supports two independently-scaled y-axes natively (yaxis / yaxis2)
  ds_macro_overlay <- reactive({
    ds_score_overall() |>
      select(month, fin_distress_rate) |>
      inner_join(
        ds_macro_selected() |> select(month, indicator, series, value),
        by = 'month'
      ) |>
      arrange(month)
  })

  output$macro_overlay_trend <- renderPlotly({
    ds_plot <- ds_macro_overlay()
    req(nrow(ds_plot) > 0)

    v_indicator_label <- unique(ds_plot$indicator)[1]

    plot_ly(ds_plot, x = ~month) |>
      add_lines(
        y = ~fin_distress_rate, name = 'Fin. Distress Rate', yaxis = 'y',
        line = list(color = mix_palette$red, width = 3),
        marker = list(color = mix_palette$red),
        mode = 'lines+markers',
        text = ~paste0(month, '<br>Fin. Distress Rate: ', percent(fin_distress_rate, accuracy = 0.1)),
        hoverinfo = 'text'
      ) |>
      add_lines(
        y = ~value, name = v_indicator_label, yaxis = 'y2',
        line = list(color = v_brand_blue, width = 3),
        marker = list(color = v_brand_blue),
        mode = 'lines+markers',
        text = ~paste0(month, '<br>', indicator, ' (', series, '): ', comma(value, accuracy = 0.01)),
        hoverinfo = 'text'
      ) |>
      layout(
        font = list(family = 'Montserrat', color = '#1D1D1B'),
        xaxis = list(title = '', tickangle = -45),
        yaxis = list(title = 'Fin. Distress Rate', tickformat = '.0%', color = mix_palette$red),
        yaxis2 = list(title = v_indicator_label, overlaying = 'y', side = 'right', color = v_brand_blue, showgrid = F),
        legend = list(orientation = 'h', x = 0, y = 1.12),
        margin = list(t = 40)
      )
  })

  #-- same overlay, but fin_distress_rate is split into one line per segment_value (driven by the
  #-- sidebar's segment_type selector, same as the SereneScore tab's segmented chart) with the macro
  #-- indicator drawn as a single purple line over all of them on the secondary axis
  output$macro_overlay_segmented_header <- renderText({
    req(input$macro_indicator_series)
    paste0('Financial Distress Rate by ', v_segment_label(), ' vs. ', input$macro_indicator_series)
  })

  ds_macro_overlay_segmented <- reactive({
    ds_score_segmented() |>
      select(month, segment_value, fin_distress_rate) |>
      inner_join(
        ds_macro_selected() |> select(month, indicator, series, value),
        by = 'month'
      )
  })

  output$macro_overlay_segmented_trend <- renderPlotly({
    ds_plot <- ds_macro_overlay_segmented()
    req(nrow(ds_plot) > 0)

    v_indicator_label <- unique(ds_plot$indicator)[1]
    v_segments <- levels(droplevels(ds_plot$segment_value))
    v_colors <- setNames(viridisLite::viridis(length(v_segments)), v_segments)

    p <- plot_ly()
    for (v_seg in v_segments) {
      ds_seg <- ds_plot |> filter(segment_value == v_seg) |> arrange(month)
      p <- p |> add_trace(
        data = ds_seg, x = ~month, y = ~fin_distress_rate, name = v_seg, yaxis = 'y',
        type = 'scatter', mode = 'lines+markers',
        line = list(color = v_colors[[v_seg]], width = 2),
        marker = list(color = v_colors[[v_seg]]),
        text = ~paste0(v_segment_label(), ': ', v_seg, '<br>', month, '<br>Fin. Distress Rate: ', percent(fin_distress_rate, accuracy = 0.1)),
        hoverinfo = 'text'
      )
    }

    ds_macro_line <- ds_plot |> distinct(month, indicator, series, value) |> arrange(month)
    p <- p |>
      add_trace(
        data = ds_macro_line, x = ~month, y = ~value, name = v_indicator_label, yaxis = 'y2',
        type = 'scatter', mode = 'lines+markers',
        line = list(color = v_brand_purple, width = 3),
        marker = list(color = v_brand_purple),
        text = ~paste0(month, '<br>', indicator, ' (', series, '): ', comma(value, accuracy = 0.01)),
        hoverinfo = 'text'
      ) |>
      layout(
        font = list(family = 'Montserrat', color = '#1D1D1B'),
        xaxis = list(title = '', tickangle = -45),
        yaxis = list(title = 'Fin. Distress Rate', tickformat = '.0%'),
        yaxis2 = list(title = v_indicator_label, overlaying = 'y', side = 'right', color = v_brand_purple, showgrid = F),
        legend = list(orientation = 'h', x = 0, y = 1.15),
        margin = list(t = 60)
      )

    p
  })
}

##### Run #####
shinyApp(ui = ui, server = server)
