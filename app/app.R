# app.R — Political Donations & Associated Tenders (tables drive filters; recipients added)
library(shiny)
library(bslib)
library(arrow)
library(tidyverse)
library(lubridate)
library(DT)
library(scales)
library(tidytext)  # reorder_within / scale_x_reordered
library(showtext)
library(sysfonts)

options(shiny.useragg = TRUE)   # prefer ragg for crisp text

# Add a local TTF (best for production/offline)
font_add(
  family  = "nsg",
  regular = "www/fonts/bpg-arial-webfont.ttf"
)

showtext_auto()                 # turn on showtext globally
showtext_opts(dpi = 110)        # match your renderPlot res if you change it


theme_ds <- function () { 
  theme_minimal(base_size=10) %+replace%
    theme(
      axis.title = element_blank(),
      title = element_blank(),
      # axis.text.x = element_blank(),
      # panel.grid = element_blank(),
      strip.text = element_text(family = "nsg", face="bold", size = 14),
      text = element_text(family= "nsg"),
      plot.title = element_text(size=10, face="bold", family="nsg", hjust = 0),
      plot.subtitle = element_text(size=8, family="nsg", hjust=0),
      plot.caption = element_text(size=6, family="nsg", face = "italic", hjust=0),
      axis.text = element_text(size=8, family="nsg", color = "black"),
      legend.position = "none"
    )
}


# ---------- File paths ----------
path_don <- "donations_short.rds"     # cols: date, name, idCode, amount, recipient
path_ten <- "tenders_donations.rds"   # cols: buyer, bid_end_time, main_cpv, supplier_id,
                                          #       supplier_name, final_value, currency, name (donor), personal_id[, tender_id?]
stopifnot(file.exists(path_don), file.exists(path_ten))

# ---------- Load ----------
donations <- read_rds(path_don) |>
  mutate(
    date   = as_date(date),
    amount = suppressWarnings(as.numeric(amount))
  )

tenders <- read_rds(path_ten) |>
  mutate(
    award_date = {
      d1 <- suppressWarnings(ymd_hms(bid_end_time, quiet = TRUE))
      d2 <- suppressWarnings(ymd(bid_end_time,  quiet = TRUE))
      as_date(coalesce(d1, d2))
    },
    final_value = suppressWarnings(as.numeric(final_value))
  )

# ---------- Choices ----------
choices_party    <- sort(unique(donations$recipient))
choices_donor    <- sort(unique(donations$name))
choices_buyer    <- sort(unique(tenders$buyer))
choices_supplier <- sort(unique(tenders$supplier_name))
choices_curr     <- sort(unique(tenders$currency))

# ---------- UI ----------
ui <- page_sidebar(
  title = "პოლიტიკური შემოწირულობები და დაკავშირებული ტენდერები",
  theme = bs_theme(bootswatch = "flatly"),
  sidebar = sidebar(
    width = 340,
    # section dividers + optional custom css
    tags$head(tags$style(HTML("
      .filter-divider{
        border-top: 1px solid #e5e7eb; margin:.5rem 0 1rem; padding-top:.5rem;
        color:#6b7280; font-size:.85rem; text-transform:uppercase; letter-spacing:.05em;
      }
    "))),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),

    h4("ფილტრები"),
    div(class = "filter-divider", "პოლიტიკური შემოწირულობები"),
    dateRangeInput("don_date", "შემოწირულობის თარიღი",
      start = min(donations$date, na.rm = TRUE),
      end   = max(donations$date, na.rm = TRUE)
    ),
    selectizeInput("party", "მიმღები (პარტია, ჯგუფი)",
      choices = choices_party, multiple = TRUE,
      options = list(placeholder = "მოძებნეთ მიმღები(ები)…",
                     onInitialize = I('function(){this.setValue("")}'))
    ),
    selectizeInput("donor", "შემწირველი",
      choices = choices_donor, multiple = TRUE,
      options = list(placeholder = "მოძებნეთ შემომწირველი…",
                     onInitialize = I('function(){this.setValue("")}'))
    ),

    div(class = "filter-divider", "ტენდერები"),
    dateRangeInput("aw_date", "ტენდერის დასრულების თარიღი",
      start = min(tenders$award_date, na.rm = TRUE),
      end   = max(tenders$award_date, na.rm = TRUE)
    ),
    selectizeInput("buyer", "მყიდველი",
      choices = choices_buyer, multiple = TRUE,
      options = list(placeholder = "მოძებნეთ მყიდველი(ები)…",
                     onInitialize = I('function(){this.setValue("")}'))
    ),
    selectizeInput("supplier", "მომწოდებელი",
      choices = choices_supplier, multiple = TRUE,
      options = list(placeholder = "მოძებნეთ მომწოდებელი(ები)…",
                     onInitialize = I('function(){this.setValue("")}'))
    ),
    selectizeInput("curr", "ვალუტა",
      choices = c("ყველა" = "All", choices_curr),
      selected = "All", multiple = FALSE
    ),
    hr(),
    actionButton("reset", "ფილტრის მოხსნა", icon = icon("arrow-rotate-left"),
                 class = "btn btn-outline-secondary"),
    hr(),
    downloadButton("dl_don", "ჩამოტვირთვა: შემოწირულობები (გაფილტრული)"),
    downloadButton("dl_ten", "ჩამოტვირთვა: ტენდერები (გაფილტრული)")
  ),

  # ===== Value boxes =====
  layout_columns(
    col_widths = c(6,6),
    value_box(
      title = "პარტიის/ჯგუფისთვის შეწირული თანხა, ₾",
      value = textOutput("kpi_don_selected", inline = TRUE),
      showcase = icon("hand-holding-dollar")
    ),
    value_box(
      title = "მოგებული ტენდერების ღირებულება",
      value = uiOutput("kpi_awards_associated"),
      showcase = icon("sack-dollar")
    )
  ),

  # ===== Tabs =====
  navs_tab_card(
    nav_panel("შემოწირულობები",
#      layout_columns(
#        col_widths = c(6,6),
#        card(card_header("შემოწირულობები დროის მიხედვით"),
#             plotOutput("p_don_time", height = 320)),
#        card(card_header("უმსხვილესი შემომწირველები"),
#             plotOutput("p_top_donors", height = 320))
#      ),
      # NEW: recipients section
      layout_columns(
        col_widths = c(6,6),
        card(card_header("მიმღებები (პარტიები/ჯგუფები) — თანხები"),
             plotOutput("p_top_recipients", height = 320)),
        card(card_header("მიმღებები — შეჯამება"),
             DTOutput("tbl_recipients"))
      ),
      card(card_header("შემოწირულობების ცხრილი"), DTOutput("tbl_don"))
    ),
    nav_panel("დაკავშირებული ტენდერები",
      layout_columns(
        col_widths = c(6,6),
        card(card_header("დაკავშირებული ტენდერები დროის მიხედვით"),
             plotOutput("p_aw_time", height = 320)),
        card(card_header("უმსხვილესი მომწოდებლები"),
             plotOutput("p_top_sup", height = 320))
      ),
      card(card_header("დაკავშირებული ტენდერების ცხრილი"), DTOutput("tbl_ten"))
    ),
    nav_panel("შესახებ",
      card(card_body(
        p("ტენდერების მონაცემები მოიცავს მხოლოდ იმ შემდგარ ტენდერებს, რომლებშიც გაიმარჯვეს პოლიტიკური პარტიების ან ჯგუფების შემწირველებმა ან მათთან დაკავშირებულმა იურიდიულმა პირებმა."),
        p("მონაცემების წყარო: „საერთაშორისო გამჭვირვალობა - საქართველოს“ პროექტები companyinfo.ge, tendermonitor.ge და „პარტიების შემოწირულობები“."),
        p("მომხმარებლის ინტერფეისის ასაწყობად გამოყენებულია ე.წ. დიდი ენობრივი მოდელები (ChatGPT)"),
        p("პარტიების შემოწირულობების მონაცემებში დუბლირებულია ენმ-ის და ევროპული საქართველოს შემომწირველები, რაც ინტერფეისში არაა გათვალისწინებული"),
        p("აშშ. დოლარსა და ევროში დაფიქსირებული ტენდერები კონვერტირებულია ლარში, ტენდერის დასრულების თვის ბოლოს არსებული ეროვნული ბანკის ოფიციალური კურსით.")
      )))
  )
)

# ---------- Server ----------
server <- function(input, output, session){

  # Reset
  observeEvent(input$reset, {
    updateDateRangeInput(session, "don_date",
      start = min(donations$date, na.rm = TRUE),
      end   = max(donations$date, na.rm = TRUE)
    )
    updateDateRangeInput(session, "aw_date",
      start = min(tenders$award_date, na.rm = TRUE),
      end   = max(tenders$award_date, na.rm = TRUE)
    )
    updateSelectizeInput(session, "party",    selected = character(0))
    updateSelectizeInput(session, "donor",    selected = character(0))
    updateSelectizeInput(session, "buyer",    selected = character(0))
    updateSelectizeInput(session, "supplier", selected = character(0))
    updateSelectizeInput(session, "curr",     selected = "All")
  })

  # ----- Donations base (date + party; donor/supplier/buyer applied later) -----
  donations_base <- reactive({
    req(input$don_date)
    d <- donations |>
      filter(between(date, input$don_date[1], input$don_date[2]))
    if (length(input$party)) d <- d |> filter(recipient %in% input$party)
    d
  })

  # ----- Tenders base (award filters) -----
  tenders_f_base <- reactive({
    req(input$aw_date)
    t <- tenders |>
      filter(between(award_date, input$aw_date[1], input$aw_date[2]))
    if (length(input$buyer))    t <- t |> filter(buyer %in% input$buyer)
    if (length(input$supplier)) t <- t |> filter(supplier_name %in% input$supplier)
    if (input$curr != "All")    t <- t |> filter(currency == input$curr)
    t
  })

  # ----- Donor sets implied by selections -----
  donors_from_party <- reactive({
    if (!length(input$party)) return(NULL)
    donations |>
      filter(between(date, input$don_date[1], input$don_date[2]),
             recipient %in% input$party) |>
      distinct(name) |>
      pull(name)
  })

  tenders_by_buyer <- reactive({
    t <- tenders |>
      filter(between(award_date, input$aw_date[1], input$aw_date[2]))
    if (length(input$buyer)) t <- t |> filter(buyer %in% input$buyer)
    if (input$curr != "All") t <- t |> filter(currency == input$curr)
    t
  })
  donors_from_buyer <- reactive({
    if (!length(input$buyer)) return(NULL)
    tenders_by_buyer() |> distinct(name) |> pull(name)
  })

  tenders_by_supplier <- reactive({
    t <- tenders |>
      filter(between(award_date, input$aw_date[1], input$aw_date[2]))
    if (length(input$supplier)) t <- t |> filter(supplier_name %in% input$supplier)
    if (input$curr != "All")    t <- t |> filter(currency == input$curr)
    t
  })
  donors_from_supplier <- reactive({
    if (!length(input$supplier)) return(NULL)
    tenders_by_supplier() |> distinct(name) |> pull(name)
  })

  donors_from_tenders <- reactive({
    unique(c(
      if (!is.null(donors_from_buyer())) donors_from_buyer() else character(0),
      if (!is.null(donors_from_supplier())) donors_from_supplier() else character(0)
    ))
  })

  # Donations tab donor set: (party?) ∩ (buyer/supplier-union?) ∩ (explicit donor?)
  donors_for_donations <- reactive({
    sets <- list()
    p  <- donors_from_party();   if (!is.null(p))  sets <- append(sets, list(p))
    ts <- donors_from_tenders(); if (length(ts))   sets <- append(sets, list(ts))
    if (length(input$donor))     sets <- append(sets, list(input$donor))
    if (length(sets) == 0) return(NULL)
    if (length(sets) == 1) return(sets[[1]])
    Reduce(intersect, sets)
  })

  donations_f <- reactive({
    d <- donations_base()
    keep <- donors_for_donations()
    if (!is.null(keep)) d <- d |> filter(name %in% keep)
    d
  })

  # Tenders tab donor restriction: party donors (∩ explicit donor, if any)
  donors_for_tenders <- reactive({
    p <- donors_from_party()
    d <- if (length(input$donor)) input$donor else NULL
    if (is.null(p) && is.null(d)) return(NULL)
    if (is.null(p)) return(d)
    if (is.null(d)) return(p)
    intersect(p, d)
  })

  tenders_party_f <- reactive({
    t <- tenders_f_base()
    keep <- donors_for_tenders()
    if (!is.null(keep)) t <- t |> filter(name %in% keep)
    t
  })

  # Helper: unique tenders (if tender_id exists)
  unique_tenders <- function(df) {
    if ("tender_id" %in% names(df)) {
      df |> distinct(tender_id, .keep_all = TRUE)
    } else df
  }

  # ===================== KPIs =====================
  output$kpi_don_selected <- renderText({
    label_comma()(sum(donations_f()$amount, na.rm = TRUE))
  })

  output$kpi_awards_associated <- renderUI({
    t <- tenders_party_f()
    if (!nrow(t)) return(HTML("—"))
    by_curr <- t |>
      unique_tenders() |>
      group_by(currency) |>
      summarize(total = sum(final_value, na.rm = TRUE), .groups = "drop") |>
      arrange(currency) |>
      mutate(txt = paste0(currency, " ", label_comma()(total))) |>
      pull(txt)
    HTML(paste(by_curr, collapse = "; "))
  })

  # ===================== Donations tab =====================
  output$p_don_time <- renderPlot({
    d <- donations_f()
    if (!nrow(d)) { plot.new(); text(0.5, 0.5, "მონაცემი არ არის"); return(invisible()) }
    ts <- d |>
      mutate(month = floor_date(date, "month")) |>
      group_by(month) |>
      summarize(total = sum(amount, na.rm = TRUE), .groups = "drop")
    ggplot(ts, aes(month, total)) +
      geom_line() +
      scale_y_continuous(labels = label_comma()) +
      labs(x = NULL, y = "სულ შემოწირულობები") +
      theme_ds()
  })

  output$p_top_donors <- renderPlot({
    d <- donations_f()
    if (!nrow(d)) { plot.new(); text(0.5, 0.5, "მონაცემი არ არის"); return(invisible()) }
    topd <- d |>
      group_by(name) |>
      summarize(total = sum(amount, na.rm = TRUE), .groups = "drop") |>
      slice_max(order_by = total, n = 12)
    ggplot(topd, aes(reorder(name, total), total)) +
      geom_col() + coord_flip() +
      scale_y_continuous(labels = label_comma()) +
      labs(x = NULL, y = "სულ შემოწირულობები") +
      theme_ds()
  })

  # ---- Recipients (top chart + summary table) ----
  output$p_top_recipients <- renderPlot({
    d <- donations_f()
    if (!nrow(d)) { plot.new(); text(0.5, 0.5, "მონაცემი არ არის"); return(invisible()) }
    top_rec <- d |>
      group_by(recipient) |>
      summarize(
        total = sum(amount, na.rm = TRUE),
        n_donations = n(),
        n_donors = n_distinct(name),
        .groups = "drop"
      ) |>
      slice_max(order_by = total, n = 12)
    ggplot(top_rec, aes(reorder(recipient, total), total)) +
      geom_col() + coord_flip() +
      scale_y_continuous(labels = label_comma()) +
      labs(x = NULL, y = "სულ შემოწირულობები") +
      theme_ds()
  })

  recipients_tbl_data <- reactive({
    d <- donations_f()
    if (!nrow(d)) return(tibble(recipient = character(), total = numeric(), n_donations = integer(), n_donors = integer()))
    d |>
      group_by(recipient) |>
      summarize(
        total = sum(amount, na.rm = TRUE),
        n_donations = n(),
        n_donors = n_distinct(name),
        .groups = "drop"
      ) |>
      arrange(desc(total))
  })

  output$tbl_recipients <- renderDT({
    datatable(
      recipients_tbl_data(),
      rownames  = FALSE,
      selection = "multiple",
      options   = list(pageLength = 10, scrollX = TRUE)
    ) |>
      formatCurrency("total", currency = "")
  })

  observeEvent(input$tbl_recipients_rows_selected, ignoreInit = TRUE, {
    sel <- input$tbl_recipients_rows_selected
    if (!length(sel)) return()
    df <- recipients_tbl_data()
    picked <- unique(df$recipient[sel])
    updateSelectizeInput(session, "party", selected = picked)
  })

  # ---- Donations table (multi-select) ----
  donations_tbl_data <- reactive({
    donations_f() |>
      select(date, name, idCode, recipient, amount) |>
      arrange(desc(date))
  })

  output$tbl_don <- renderDT({
    datatable(
      donations_tbl_data(),
      rownames  = FALSE,
      filter    = "top",
      selection = "multiple",
      options   = list(pageLength = 15, scrollX = TRUE)
    ) |>
      formatCurrency("amount", currency = "")
  })

  # ---- Tenders charts ----
  output$p_aw_time <- renderPlot({
    t <- tenders_party_f()
    if (!nrow(t)) { plot.new(); text(0.5, 0.5, "No data"); return(invisible()) }
    ts <- t |>
      unique_tenders() |>
      filter(!is.na(award_date)) |>
      mutate(month = floor_date(award_date, "month")) |>
      group_by(month, currency) |>
      summarize(total = sum(final_value, na.rm = TRUE), .groups = "drop")
    ggplot(ts, aes(month, total)) +
      geom_line() +
      facet_wrap(~ currency, scales = "free_y") +
      scale_y_continuous(labels = label_comma()) +
      labs(x = NULL, y = "დაკავშირებული ტენდერები") +
      theme_ds()
  })

  output$p_top_sup <- renderPlot({
    t <- tenders_party_f()
    if (!nrow(t)) { plot.new(); text(0.5, 0.5, "No data"); return(invisible()) }
    top <- t |>
      unique_tenders() |>
      group_by(currency, supplier_id, supplier_name) |>
      summarize(total = sum(final_value, na.rm = TRUE), .groups = "drop") |>
      group_by(currency) |>
      slice_max(order_by = total, n = 12) |>
      ungroup()
    ggplot(top, aes(reorder_within(supplier_name, total, currency), total)) +
      geom_col() + coord_flip() +
      scale_y_continuous(labels = label_comma()) +
      scale_x_reordered() +
      facet_wrap(~ currency, scales = "free_y") +
      labs(x = NULL, y = "დაკავშირებული ტენდერები") +
      theme_ds()
  })

  # ---- Tenders table (multi-select) ----
  tenders_tbl_data <- reactive({
    tenders_party_f() |>
      select(
        award_date, buyer, main_cpv, supplier_id, supplier_name,
        final_value, currency, donor_name = name, personal_id
      ) |>
      arrange(desc(award_date))
  })

  output$tbl_ten <- renderDT({
    datatable(
      tenders_tbl_data(),
      rownames  = FALSE,
      filter    = "top",
      selection = "multiple",
      options   = list(pageLength = 15, scrollX = TRUE)
    ) |>
      formatCurrency("final_value", currency = "")
  })

  # ===================== Downloads =====================
  output$dl_don <- downloadHandler(
    filename = function() paste0("donations_filtered_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(donations_f(), file)
  )
  output$dl_ten <- downloadHandler(
    filename = function() paste0("donations_tenders_filtered_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(tenders_party_f(), file)
  )

  # ===================== Row selection → drive filters =====================
  # Donations: pick donor(s) → set Donor filter (and clear Party to see all parties)
  observeEvent(input$tbl_don_rows_selected, ignoreInit = TRUE, {
    sel <- input$tbl_don_rows_selected
    df  <- donations_tbl_data()
    if (!length(sel) || !nrow(df)) return()
    picked_donors <- unique(df$name[sel])
    updateSelectizeInput(session, "donor", selected = picked_donors)
    updateSelectizeInput(session, "party", selected = character(0))
  })

  # Tenders: pick supplier(s) → set Supplier filter (and clear Party)
  observeEvent(input$tbl_ten_rows_selected, ignoreInit = TRUE, {
    sel <- input$tbl_ten_rows_selected
    df  <- tenders_tbl_data()
    if (!length(sel) || !nrow(df)) return()
    picked_suppliers <- unique(df$supplier_name[sel])
    updateSelectizeInput(session, "supplier", selected = picked_suppliers)
    updateSelectizeInput(session, "party", selected = character(0))
  })
}

shinyApp(ui, server)
