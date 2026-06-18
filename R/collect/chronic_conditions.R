# =====================================================================
# R/collect/chronic_conditions.R
# 120 variables across 13 conditions (arth asth canc diab emop hatt hdis hibp
# ldis lmem lung ocon stro). Seven recode shapes:
#   bin5     care_* & diag_any_* & *_diag_2nd_*   5->0,1->1,8/9->NA,0/.->NA
#   agediag  *_diag_age_*   year-gated age at diagnosis
#   degr     *_degr_*       7->0,5->1,3->2,1->3,8/9->NA,0/.->NA
#   mon12    emop_type_*    1..12 + 97->80
#   mon13    canc_type_*    1..13 + 97->80
#   mon26    ocon_type_*    1..10,20..26 + 97->80
#   cat4     canc_stat_*    4->0,1->1,2->2,3->3,8/9->NA,0/.->NA
# =====================================================================

conds <- c("arth","asth","canc","diab","emop","hatt","hdis","hibp",
           "ldis","lmem","lung","ocon","stro")
who2  <- c("rp", "sp")
cc    <- function(...) as.vector(t(outer(paste0("ccon_", ...), who2,
                                         function(a, b) paste0(a, "_", b))))

bin5 <- function(x, y) recode(x, 5 ~ 0, 1 ~ 1, c(8, 9, 0, NA) ~ NA)
agediag <- function(x, y) {           # age at diagnosis (coding changed in 2017)
  out <- rep(-1, length(x))
  if (y >= 2005 && y <= 2015) {
    out <- rc(out, inlist(x, 1), 1); out <- rc(out, inrange(x, 2, 120), x)
  } else {                            # 2017+
    out <- rc(out, inlist(x, 995), 0); out <- rc(out, inrange(x, 1, 120), x)
  }
  rc(out, inlist(x, 998, 999, 0) | is.na(x), NA)
}
degr <- function(x, y)                # severity / degree (4-cat reversed)
  recode(x, 7 ~ 0, 5 ~ 1, 3 ~ 2, 1 ~ 3, c(8, 9, 0, NA) ~ NA)
mon_fn <- function(hi, extra = NULL) function(x, y) {  # month/type codes -> identity
  out <- recode(x, 1 %..% hi ~ keep, 97 ~ 80, c(98, 99, 0, NA) ~ NA)
  if (!is.null(extra)) out <- rc(out, inrange(x, extra[1], extra[2]), x)  # disjoint 2nd range
  out
}
cat4 <- function(x, y)
  recode(x, 4 ~ 0, 1 ~ 1, 2 ~ 2, 3 ~ 3, c(8, 9, 0, NA) ~ NA)

# build variable lists per shape
care_conds <- setdiff(conds, "canc")                       # canc has no "care"
A_vars <- c(cc(paste0(care_conds, "_care")),
            cc(paste0(conds, "_diag_any")),
            cc("hatt_diag_2nd"), cc("stro_diag_2nd"))
B_vars <- cc(paste0(conds, "_diag_age"))
C_vars <- cc(paste0(conds, "_degr"))
D_vars <- cc(c("emop_type_1m", "emop_type_2m", "emop_type_3m"))
E_vars <- cc(c("canc_type_1m", "canc_type_2m"))
F_vars <- cc("ocon_type")
G_vars <- cc("canc_stat")

apply_set <- function(vars, fn) for (v in vars) psid_abridged <<- collect_tv(psid_abridged, v, fn)
apply_set(A_vars, bin5)
apply_set(B_vars, agediag)
apply_set(C_vars, degr)
apply_set(D_vars, mon_fn(12))
apply_set(E_vars, mon_fn(13))
apply_set(F_vars, mon_fn(10, c(20, 26)))
apply_set(G_vars, cat4)
