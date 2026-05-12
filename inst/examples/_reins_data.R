# Reinsurance demo data — synthetic exposure cube + event-loss table.
#
# Sourced by the three reins-* demos. Both cubes are seeded and
# deterministic. Not a real CAT model — shaped to make the live
# crossfilter / exceedance-curve story land in a Swiss Re conversation.

make_reins_data <- function(seed = 42L) {
  set.seed(seed)

  cedants <- sprintf("Cedant_%02d", 1:18)
  perils <- c(
    "Windstorm", "Flood", "Earthquake", "Wildfire",
    "Cyber", "Pandemic", "Motor", "Casualty"
  )
  regions <- c("EMEA", "Americas", "APAC", "Switzerland")
  lobs <- c("Property", "Casualty", "Cyber", "Specialty")
  years <- 2020:2024
  treaty_types <- c("QS", "XoL", "Stop-Loss")

  # peril -> dominant LOB (roughly)
  peril_lob <- c(
    Windstorm = "Property", Flood = "Property", Earthquake = "Property",
    Wildfire = "Property", Cyber = "Cyber", Pandemic = "Specialty",
    Motor = "Casualty", Casualty = "Casualty"
  )
  # peril -> base loss scale (millions USD)
  peril_scale <- c(
    Windstorm = 80, Flood = 40, Earthquake = 120, Wildfire = 30,
    Cyber = 25, Pandemic = 60, Motor = 15, Casualty = 35
  )
  # region multiplier
  region_mult <- c(EMEA = 1.0, Americas = 1.3, APAC = 0.9, Switzerland = 0.6)

  # === Exposure cube ===
  # Sparse: each cedant writes ~6-12 peril/region combos
  exp_rows <- list()
  treaty_counter <- 0L
  for (cd in cedants) {
    n_treaties <- sample(6:12, 1L)
    combos <- expand.grid(
      peril = perils, region = regions, stringsAsFactors = FALSE
    )
    sel <- combos[sample(nrow(combos), n_treaties), ]
    for (i in seq_len(nrow(sel))) {
      p <- sel$peril[i]; r <- sel$region[i]
      ttype <- sample(treaty_types, 1L,
        prob = c(0.5, 0.35, 0.15))
      for (yr in years) {
        treaty_counter <- treaty_counter + 1L
        scale <- peril_scale[[p]] * region_mult[[r]] *
          runif(1, 0.6, 1.6)
        exposure <- round(scale * 1e6 * runif(1, 4, 14))
        premium <- round(exposure * runif(1, 0.012, 0.045))
        exp_loss <- round(premium * runif(1, 0.55, 0.85))
        attachment <- if (ttype == "XoL") {
          round(scale * 1e6 * runif(1, 1.5, 4))
        } else NA_real_
        limit <- if (ttype == "XoL") {
          round(scale * 1e6 * runif(1, 3, 8))
        } else if (ttype == "Stop-Loss") {
          round(exposure * runif(1, 0.3, 0.6))
        } else NA_real_
        share <- switch(ttype,
          QS = round(runif(1, 0.15, 0.5), 3),
          1.0)
        exp_rows[[length(exp_rows) + 1L]] <- data.frame(
          treaty_id = sprintf("T-%d-%04d", yr, treaty_counter),
          underwriting_year = yr,
          cedant = cd,
          peril = p,
          region = r,
          line_of_business = peril_lob[[p]],
          treaty_type = ttype,
          share_assumed = share,
          exposure_usd = exposure,
          premium_assumed_usd = premium,
          expected_loss_usd = exp_loss,
          attachment_usd = attachment,
          limit_usd = limit,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  treaty_exposure <- do.call(rbind, exp_rows)
  rownames(treaty_exposure) <- NULL

  # === Event catalogue ===
  # 4000 simulated events. Each event has a peril/region and a loss to
  # the assumed book. Annual frequency baked in via repeated draws —
  # treat the catalogue as covering ~5 modelled years so the empirical
  # exceedance curve has ~1-in-1000 resolution.
  n_events <- 4000L
  events <- data.frame(
    event_id = sprintf("E%06d", seq_len(n_events)),
    peril = sample(perils, n_events, replace = TRUE,
      prob = c(0.20, 0.18, 0.08, 0.10, 0.12, 0.04, 0.18, 0.10)),
    region = sample(regions, n_events, replace = TRUE,
      prob = c(0.40, 0.30, 0.20, 0.10)),
    stringsAsFactors = FALSE
  )
  events$line_of_business <- peril_lob[events$peril]
  # Loss draw — lognormal with per-peril/region scale; heavy right tail
  mu <- log(peril_scale[events$peril] * region_mult[events$region] * 1e6)
  sigma <- 1.25
  events$gross_loss_usd <- round(exp(rnorm(n_events, mu, sigma)))
  # Some "from cedant N" attribution — share each event across 1-3 cedants
  events$primary_cedant <- sample(cedants, n_events, replace = TRUE)
  # A modelled return period — empirical rank-based, treating catalogue
  # as 5 years of frequency
  modelled_years <- 5
  ord <- order(events$gross_loss_usd, decreasing = TRUE)
  rank_pos <- integer(n_events)
  rank_pos[ord] <- seq_len(n_events)
  events$return_period_years <- round(
    modelled_years * n_events / rank_pos, 1
  )
  events$exceedance_prob <- rank_pos / n_events

  # === Event profile (long-format per-event breakdown) ===
  # For each event, fan out 1-4 cedant share rows + the treaty layers
  # that would respond given matching peril/region. Used by the
  # "Event profile" drill workspace: click an event_id in the bar
  # chart → all downstream filter blocks see only that event's rows.
  ed_cedants <- vector("list", n_events)
  for (i in seq_len(n_events)) {
    n_cd <- sample(1:4, 1L)
    cds <- sample(cedants, n_cd, replace = FALSE)
    shares <- runif(n_cd)
    shares <- shares / sum(shares)
    ed_cedants[[i]] <- data.frame(
      event_id = events$event_id[i],
      peril = events$peril[i],
      region = events$region[i],
      gross_loss_usd = events$gross_loss_usd[i],
      breakdown_type = "cedant",
      breakdown_key = cds,
      breakdown_amount = round(shares * events$gross_loss_usd[i]),
      stringsAsFactors = FALSE
    )
  }
  ed_cedants <- do.call(rbind, ed_cedants)

  # Treaty layer impacts: for each event, find the treaties that
  # match peril+region. XoL pays max(0, min(limit, loss - attach));
  # QS pays share * loss; Stop-Loss pays nothing per-event.
  exp_idx <- split(seq_len(nrow(treaty_exposure)),
    paste(treaty_exposure$peril, treaty_exposure$region, sep = "|"))
  ed_treaties <- vector("list", n_events)
  for (i in seq_len(n_events)) {
    key <- paste(events$peril[i], events$region[i], sep = "|")
    matches <- exp_idx[[key]]
    if (is.null(matches) || length(matches) == 0L) {
      ed_treaties[[i]] <- NULL
      next
    }
    # Cap to ~6 random treaties per event to keep table manageable
    if (length(matches) > 6L) matches <- sample(matches, 6L)
    rows <- treaty_exposure[matches, , drop = FALSE]
    gl <- events$gross_loss_usd[i]
    ceded <- ifelse(rows$treaty_type == "XoL",
      pmax(0, pmin(rows$limit_usd, gl - rows$attachment_usd)),
      ifelse(rows$treaty_type == "QS",
        round(rows$share_assumed * gl),
        0))
    ed_treaties[[i]] <- data.frame(
      event_id = events$event_id[i],
      peril = events$peril[i],
      region = events$region[i],
      gross_loss_usd = gl,
      breakdown_type = "treaty",
      breakdown_key = paste0(rows$treaty_id, " [", rows$treaty_type, "]"),
      breakdown_amount = pmax(0, round(ceded)),
      stringsAsFactors = FALSE
    )
  }
  ed_treaties <- do.call(rbind, Filter(Negate(is.null), ed_treaties))
  event_profile <- rbind(ed_cedants, ed_treaties)
  rownames(event_profile) <- NULL

  # === Cedants metadata ===
  # Small dimension table. Acts as the dm parent so the Cedant-profile
  # drill can semi-filter the whole dm by cedant via FK cascade.
  domiciles <- c(
    "Germany", "Switzerland", "United Kingdom", "France", "Italy",
    "Spain", "United States", "Japan", "Bermuda", "Ireland"
  )
  segments <- c(
    "Large composite", "Regional", "Specialty", "Mutual", "Lloyd's syndicate"
  )
  cedants_meta <- data.frame(
    cedant = cedants,
    domicile = sample(domiciles, length(cedants), replace = TRUE),
    segment = sample(segments, length(cedants), replace = TRUE,
      prob = c(0.30, 0.25, 0.20, 0.15, 0.10)),
    founded = sample(1850:1995, length(cedants), replace = TRUE),
    stringsAsFactors = FALSE
  )

  list(
    treaty_exposure = treaty_exposure,
    treaty_events = events,
    event_profile = event_profile,
    cedants = cedants_meta
  )
}

reins_data <- make_reins_data()
treaty_exposure <- reins_data$treaty_exposure
treaty_events <- reins_data$treaty_events
event_profile <- reins_data$event_profile
cedants <- reins_data$cedants
