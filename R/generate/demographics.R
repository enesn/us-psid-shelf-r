# =====================================================================
# R/generate/demographics.R
# Derives: demo_birth_year, demo_birth_month, demo_death_year_sy,
#          demo_death_year_ll, demo_death_year_ul, demo_death_year,
#          demo_death_rep, demo_age_gen_*
# =====================================================================

.gn  <- nrow(psid_abridged)
.gcol <- function(nm) psid_abridged[[nm]]
.glab <- function(x, nv, set = NULL) {
  x <- set_label(x, var_label(nv))
  if (!is.null(set)) {
    vl <- SPEC$value_labels[which(SPEC$value_labels$label_set %in% set), ]
    if (nrow(vl)) attr(x, "labels") <- setNames(as.numeric(vl$value), vl$label)
  }
  x
}

# --- demo_birth_year: latest reported birth year, else y-age (RP first) ---
by <- rep(-1, .gn)
for (y in rev(year)) { c <- .gcol(paste0("demo_birth_year_sy_", y)); if (!is.null(c)) by <- rc(by, by %in% -1 & !is.na(c), c) }
for (y in rev(year)) { a <- .gcol(paste0("demo_age_rep_", y)); r <- .gcol(paste0("rel_ext_", y))
  if (!is.null(a)) by <- rc(by, by %in% -1 & !is.na(a) & r %in% 100, y - a) }
for (y in rev(year)) { a <- .gcol(paste0("demo_age_rep_", y)); r <- .gcol(paste0("rel_ext_", y))
  if (!is.null(a)) by <- rc(by, by %in% -1 & !is.na(a) & !(r %in% 100), y - a) }
by <- rc(by, by %in% -1, NA)
psid_abridged$demo_birth_year <- .glab(by, "demo_birth_year")

# --- demo_birth_month: latest reported birth month ---
bm <- rep(-1, .gn)
for (y in rev(year)) { c <- .gcol(paste0("demo_birth_month_sy_", y)); if (!is.null(c)) bm <- rc(bm, bm %in% -1 & !is.na(c), c) }
bm <- rc(bm, bm %in% -1, NA)
psid_abridged$demo_birth_month <- .glab(bm, "demo_birth_month")

# --- death year (PSID code -> specific year / range / not deceased) ---
v <- psid_abridged$demo_death_year_psid
dys <- rep(-1, .gn)
dys <- rc(dys, inrange(v, 1967, 2099), v)
dys <- rc(dys, inrange(v, 1, 1966) | inrange(v, 2100, 9998), NA)
dys <- rc(dys, v %in% 0, 0)
dys <- rc(dys, v %in% 9999, NA)
psid_abridged$demo_death_year_sy <- .glab(dys, "demo_death_year_sy", "deathyear_1cat")

# range codes: lower/upper limit parsed from the digit string
trange <- ifelse(inrange(v, 1, 1966) | inrange(v, 2100, 9998), v, NA)
tstr   <- ifelse(is.na(trange), NA_character_, as.character(trange))
f2s <- ifelse(!is.na(trange) & trange >= 100  & trange < 1000, substr(tstr, 1, 1),
       ifelse(!is.na(trange) & trange >= 1000 & trange < 9998, substr(tstr, 1, 2), NA_character_))
f2  <- suppressWarnings(as.numeric(f2s))
ll <- rep(-1, .gn); ll <- rc(ll, f2 >= 67 & f2 <= 99, f2 + 1900); ll <- rc(ll, f2 >= 1 & f2 <= 66, f2 + 2000)
ll <- rc(ll, inrange(v, 1967, 2099) | v %in% 0 | v %in% 9999, NA)
psid_abridged$demo_death_year_ll <- .glab(ll, "demo_death_year_ll")

l2s <- ifelse(is.na(tstr), NA_character_, substr(tstr, nchar(tstr) - 1, nchar(tstr)))
l2  <- suppressWarnings(as.numeric(l2s))
ul <- rep(-1, .gn); ul <- rc(ul, l2 >= 67 & l2 <= 99, l2 + 1900); ul <- rc(ul, l2 >= 1 & l2 <= 66, l2 + 2000)
ul <- rc(ul, inrange(v, 1967, 2099) | v %in% 0 | v %in% 9999, NA)
psid_abridged$demo_death_year_ul <- .glab(ul, "demo_death_year_ul")

# best death-year estimate: specific year, else midpoint of [ll, ul]
dy <- rep(-1, .gn)
dy <- rc(dy, !is.na(dys), dys)
dy <- rc(dy, is.na(dys) & !is.na(ll) & !is.na(ul), round((ll + ul) * 0.5))
dy <- rc(dy, dy %in% -1, NA)
psid_abridged$demo_death_year <- .glab(dy, "demo_death_year", "deathyear_1cat")

# death ever reported?
dr <- rep(-1, .gn); dr <- rc(dr, v %in% 0, 0); dr <- rc(dr, !(v %in% 0), 1)
attr_set <- SPEC$value_labels[SPEC$value_labels$label_set == "demo_death_rep_2cat", ]
dr <- set_label(dr, var_label("demo_death_rep"))
if (nrow(attr_set)) attr(dr, "labels") <- setNames(as.numeric(attr_set$value), attr_set$label)
psid_abridged$demo_death_rep <- dr

# --- demo_age_gen_*: derived age = year - birth_year, gated by alive & in-FU ---
infu_set <- c(0, 101, 102, 103, 104, 105)
for (y in year) {
  rex <- .gcol(paste0("response_ext_", y)); fu <- .gcol(paste0("fuid_", y)); sq <- .gcol(paste0("seqnum_", y))
  alive <- (is.na(dy) | dy %in% 0) | (y <= dy & !(is.na(dy) | dy %in% 0))
  born  <- !is.na(by) & y >= by
  member <- if (y == 1968) (rex %in% infu_set & !is.na(fu))
            else ((sq >= 0 & sq <= 20) | (sq >= 51 & sq <= 59)) & rex %in% infu_set & !is.na(fu)
  out <- rep(-1, .gn)
  out <- rc(out, born & alive & member, y - by)
  out <- rc(out, !born | (!is.na(dy) & !(dy %in% 0) & y > dy) | !member, NA)
  psid_abridged[[paste0("demo_age_gen_", y)]] <- set_label(out, var_label("demo_age_gen", y))
}
