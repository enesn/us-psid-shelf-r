# =====================================================================
# R/collect/relationship_id.R
# Time-invariant identifiers built from the merged supplement columns:
#   PID*  (Parent Identification)  -> rel_par_*_{id,lineage,pnum}
#   CHI*_CAH* (Child Assessment History) -> rf_rel_chi, rel_chi{1..20}_*
#   MAR*_MH*  (Marriage History)         -> rf_rel_mar, rel_mar_tot/rep, rel_mar{1..8}_*
# These do not use the year->input map; they read the supplement columns directly.
# =====================================================================

.relid_n   <- nrow(psid_abridged)
.relid_col <- function(nm) if (!is.null(psid_abridged[[nm]])) psid_abridged[[nm]] else rep(NA_real_, .relid_n)
.relid_setv <- function(x, lab, set = NULL) {
  x <- set_label(x, lab)
  if (!is.null(set)) {
    vl <- SPEC$value_labels[which(SPEC$value_labels$label_set %in% set), ]
    if (nrow(vl)) attr(x, "labels") <- setNames(as.numeric(vl$value), vl$label)
  }
  x
}
.relid_rowmax <- function(lst) {
  if (!length(lst)) return(rep(NA_real_, .relid_n))
  m <- do.call(pmax, c(lst, list(na.rm = TRUE))); m[is.infinite(m)] <- NA; m
}

# ---- parent identifiers (PID pairs) ----
.relid_par <- list(bm = c("PID4","PID5"),  bf = c("PID23","PID24"),
                   am1 = c("PID6","PID7"),  am2 = c("PID8","PID9"),
                   af1 = c("PID25","PID26"),af2 = c("PID27","PID28"))
.relid_leg <- c(bm="birth mother", bf="birth father", am1="adoptive mother 1",
                am2="adoptive mother 2", af1="adoptive father 1", af2="adoptive father 2")
for (k in names(.relid_par)) {
  v1 <- .relid_col(.relid_par[[k]][1]); v2 <- .relid_col(.relid_par[[k]][2])
  id <- rep(-1, .relid_n)
  id <- rc(id, !(v1 %in% 0) & !(v2 %in% 0), v1 * 1000 + v2)
  id <- rc(id, (v1 %in% 0) & (v2 %in% 0), NA)
  lin <- rc(rc(rep(-1, .relid_n), !(v1 %in% 0), v1), v1 %in% 0, NA)
  pn  <- rc(rc(rep(-1, .relid_n), !(v2 %in% 0), v2), v2 %in% 0, NA)
  psid_abridged[[paste0("rel_par_",k,"_id")]]      <- .relid_setv(id,  sprintf("Ind's parent, %s, unique ID", .relid_leg[k]))
  psid_abridged[[paste0("rel_par_",k,"_lineage")]] <- .relid_setv(lin, sprintf("Ind's parent, %s, lineage", .relid_leg[k]))
  psid_abridged[[paste0("rel_par_",k,"_pnum")]]    <- .relid_setv(pn,  sprintf("Ind's parent, %s, person number", .relid_leg[k]))
}

# ---- child record flag rf_rel_chi (rowmax of CAH106 split by birth/adopt) ----
.relid_bm <- .relid_am <- list()
for (i in 1:20) {
  c106 <- psid_abridged[[paste0("CHI",i,"_CAH106")]]; c2 <- psid_abridged[[paste0("CHI",i,"_CAH2")]]
  if (is.null(c106) || is.null(c2)) next
  .relid_bm[[length(.relid_bm)+1]] <- ifelse(c2 == 1, c106, NA)
  .relid_am[[length(.relid_am)+1]] <- ifelse(c2 == 2, c106, NA)
}
bm <- .relid_rowmax(.relid_bm); am <- .relid_rowmax(.relid_am)
rf_chi <- rep(-1, .relid_n)
rf_chi <- rc(rf_chi, is.na(bm) & is.na(am), 0)
rf_chi <- rc(rf_chi, ((bm %in% 0) & (is.na(am) | am %in% 0)) | ((is.na(bm) | bm %in% 0) & (am %in% 0)), 1)
rf_chi <- rc(rf_chi, (bm >= 1 & bm <= 20) & (is.na(am) | am %in% 0), 2)
rf_chi <- rc(rf_chi, (bm %in% 98) & (am %in% 0 | is.na(am)), 3)
rf_chi <- rc(rf_chi, (is.na(bm) | bm %in% 0) & (am >= 1 & am <= 20), 4)
rf_chi <- rc(rf_chi, (is.na(bm) | bm %in% 0) & (am %in% 98), 5)
rf_chi <- rc(rf_chi, (bm >= 1 & bm <= 20) & (am >= 1 & am <= 20), 6)
rf_chi <- rc(rf_chi, (bm >= 1 & bm <= 20) & (am %in% 98), 7)
rf_chi <- rc(rf_chi, (bm %in% 98) & (am >= 1 & am <= 20), 8)
rf_chi <- rc(rf_chi, (bm %in% 98) & (am %in% 98), 9)
psid_abridged$rf_rel_chi <- .relid_setv(rf_chi, "Rec flag: Ind's child records (sep by birth/adopt)", "relchiflag_10cat")

# ---- child records rel_chi{i}_* ----
for (i in 1:20) {
  g  <- function(s) psid_abridged[[paste0("CHI",i,"_",s)]]
  c106 <- g("CAH106"); c108 <- g("CAH108"); c10 <- g("CAH10"); c11 <- g("CAH11")
  c2 <- g("CAH2"); c12 <- g("CAH12"); c9 <- g("CAH9"); c15 <- g("CAH15")
  num <- rep(-1, .relid_n); rep_ <- rep(-1, .relid_n); id <- rep(-1, .relid_n); typ <- rep(-1, .relid_n)
  sx <- rep(-1, .relid_n); ord <- rep(-1, .relid_n); by <- rep(-1, .relid_n)
  if (!is.null(c106)) {
    num  <- rc(num,  c108 >= 1 & c108 <= 20 & !(c106 %in% 0), c108)
    num  <- rc(num,  (c108 %in% 1 & c106 %in% 0) | is.na(c108), 0)
    rep_ <- rc(rep_, c106 >= 0 & c106 <= 20, c106)
    rep_ <- rc(rep_, c106 %in% 98 | is.na(c106) | rf_chi %in% 0, NA)
    id   <- rc(id,  !(c10 %in% c(0,9999)) & !is.na(c10) & !(c11 %in% c(0,999)) & !is.na(c11), c10*1000 + c11)
    id   <- rc(id,  ((c10 %in% c(0,9999) | is.na(c10)) & (c11 %in% c(0,999) | is.na(c11))) | rf_chi %in% 0, NA)
    typ  <- rc(typ, c2 >= 1 & c2 <= 2, c2);   typ <- rc(typ, is.na(c2) | rf_chi %in% 0, NA)
    sx   <- rc(sx,  c12 >= 1 & c12 <= 2, c12); sx <- rc(sx,  c12 %in% c(8,9) | is.na(c12) | rf_chi %in% 0, NA)
    ord  <- rc(ord, c9 >= 1 & c9 <= 20, c9);   ord <- rc(ord, c9 %in% c(98,99) | is.na(c9) | rf_chi %in% 0, NA)
    by   <- rc(by,  c15 >= 1900 & c15 <= 2099, c15); by <- rc(by, c15 %in% c(9998,9999) | is.na(c15) | rf_chi %in% 0, NA)
  } else {
    # birth-order slot absent from the CAH supplement (e.g. child 19/20): no
    # input columns, so the slot is empty -> all NA (not the -1 sentinel).
    num <- rep_ <- id <- typ <- sx <- ord <- by <- rep(NA_real_, .relid_n)
  }
  psid_abridged[[sprintf("rel_chi%d_num",i)]]   <- .relid_setv(num,  "Ind's total number of child records (sep by birth/adopt)")
  psid_abridged[[sprintf("rel_chi%d_rep",i)]]   <- .relid_setv(rep_, "Ind's reported number of children, with or without records (sep by birth/adopt)")
  psid_abridged[[sprintf("rel_chi%d_id",i)]]    <- .relid_setv(id,   sprintf("Ind's child %d, unique ID", i))
  psid_abridged[[sprintf("rel_chi%d_type",i)]]  <- .relid_setv(typ,  sprintf("Ind's child %d, type of record", i), "relchitype_2cat")
  psid_abridged[[sprintf("rel_chi%d_sex",i)]]   <- .relid_setv(sx,   sprintf("Ind's child %d, sex of child", i), "sex_2cat")
  psid_abridged[[sprintf("rel_chi%d_ord",i)]]   <- .relid_setv(ord,  sprintf("Ind's child %d, birth order (excl. adoptions)", i))
  psid_abridged[[sprintf("rel_chi%d_byear",i)]] <- .relid_setv(by,   sprintf("Ind's child %d, birth year", i))
}

# ---- marriage record flag + counts ----
.relid_m18 <- list()
for (i in 1:8) { v <- psid_abridged[[paste0("MAR",i,"_MH18")]]; if (!is.null(v)) .relid_m18[[length(.relid_m18)+1]] <- v }
mar_max <- .relid_rowmax(.relid_m18)
rf_mar <- rep(-1, .relid_n)
rf_mar <- rc(rf_mar, is.na(mar_max), 0)
rf_mar <- rc(rf_mar, mar_max %in% 0, 1)
rf_mar <- rc(rf_mar, !(mar_max %in% 0) & !is.na(mar_max), 2)
psid_abridged$rf_rel_mar <- .relid_setv(rf_mar, "Rec flag: Ind's marriage records", "relmarflag_3cat")

m1_20 <- .relid_col("MAR1_MH20"); m1_18 <- .relid_col("MAR1_MH18")
tot <- rep(-1, .relid_n)
tot <- rc(tot, m1_20 >= 1 & m1_20 <= 20 & !(m1_18 %in% 0), m1_20)
tot <- rc(tot, (m1_20 %in% 1 & m1_18 %in% 0) | is.na(m1_20), 0)
psid_abridged$rel_mar_tot <- .relid_setv(tot, "Ind's total number of marriage records")
repm <- rep(-1, .relid_n)
repm <- rc(repm, m1_18 >= 0 & m1_18 <= 20, m1_18)
repm <- rc(repm, m1_18 %in% 98 | is.na(m1_18), NA)
psid_abridged$rel_mar_rep <- .relid_setv(repm, "Ind's reported number of marriages, with or without records")

# ---- marriage records rel_mar{i}_* ----
for (i in 1:8) {
  g <- function(s) .relid_col(paste0("MAR",i,"_",s))
  m7 <- g("MH7"); m8 <- g("MH8"); m9 <- g("MH9"); m12 <- g("MH12")
  m11 <- g("MH11"); m16 <- g("MH16"); m14 <- g("MH14")
  id <- rep(-1, .relid_n)
  id <- rc(id, !(m7 %in% c(0,9999)) & !is.na(m7) & !(m8 %in% c(0,999)) & !is.na(m8), m7*1000 + m8)
  id <- rc(id, (m7 %in% c(0,9999) | is.na(m7)) & (m8 %in% c(0,999) | is.na(m8)), NA)
  ord <- rc(rc(rep(-1,.relid_n), m9 >= 1 & m9 <= 20, m9), m9 %in% c(98,99) | is.na(m9) | rf_mar %in% 0, NA)
  st <- rep(-1, .relid_n)
  st <- rc(st, m12 %in% 1, 1); st <- rc(st, m12 %in% 5, 2); st <- rc(st, m12 %in% 3, 3)
  st <- rc(st, m12 %in% 4, 4); st <- rc(st, m12 %in% 7, 5); st <- rc(st, m12 %in% 8, 6)
  st <- rc(st, m12 %in% 9 | is.na(m12) | rf_mar %in% 0, NA)
  my <- rc(rc(rep(-1,.relid_n), m11 >= 1900 & m11 <= 2099, m11), m11 %in% c(9998,9999) | is.na(m11) | rf_mar %in% 0, NA)
  sy <- rc(rc(rep(-1,.relid_n), m16 >= 1900 & m16 <= 2099, m16), m16 %in% c(9998,9999) | is.na(m16) | rf_mar %in% 0, NA)
  dy <- rc(rc(rep(-1,.relid_n), m14 >= 1900 & m14 <= 2099, m14), m14 %in% c(9998,9999) | is.na(m14) | rf_mar %in% 0, NA)
  psid_abridged[[sprintf("rel_mar%d_id",i)]]    <- .relid_setv(id,  sprintf("Ind's mariage %d, unique ID", i))
  psid_abridged[[sprintf("rel_mar%d_ord",i)]]   <- .relid_setv(ord, sprintf("Ind's mariage %d, order of marriage", i))
  psid_abridged[[sprintf("rel_mar%d_stat",i)]]  <- .relid_setv(st,  sprintf("Ind's mariage %d, status of marriage", i), "relmarstat_6cat")
  psid_abridged[[sprintf("rel_mar%d_myear",i)]] <- .relid_setv(my,  sprintf("Ind's mariage %d, marriage year", i))
  psid_abridged[[sprintf("rel_mar%d_syear",i)]] <- .relid_setv(sy,  sprintf("Ind's mariage %d, separation year", i))
  psid_abridged[[sprintf("rel_mar%d_dyear",i)]] <- .relid_setv(dy,  sprintf("Ind's mariage %d, dissolution year", i))
}

# hand recode one discordant spouse ID (id 1795182)
.relid_m27 <- .relid_col("MAR2_MH7"); .relid_m28 <- .relid_col("MAR2_MH8")
.relid_fix <- psid_abridged$id %in% 1795182 & !(.relid_m27 %in% c(0,9999)) & !is.na(.relid_m27) & (.relid_m28 %in% c(0,999))
if (any(.relid_fix)) psid_abridged$rel_mar2_id[which(.relid_fix)] <- NA
