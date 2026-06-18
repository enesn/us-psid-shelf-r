# =====================================================================
# R/collect/dementia.R
# Builds: dmnt_elig_{ind,rp,sp}_*, dmnt_q{1..8}_any_{ind,rp,sp}_*,
#         dmnt_score_cut_ind_*   (dementia screening battery; all binary 0/1)
# =====================================================================

# binary item without DK/refused mapping (elig, score_cut): 5->0, 1->1, 0/.->NA
dmnt_bin <- function(x, y) recode(x, 5 ~ 0, 1 ~ 1, c(0, NA) ~ NA)
# binary item with DK/refused (the q items): also 8/9 -> NA
dmnt_q   <- function(x, y) recode(x, 5 ~ 0, 1 ~ 1, c(8, 9, 0, NA) ~ NA)

for (who in c("ind", "rp", "sp")) {
  psid_abridged <- collect_tv(psid_abridged, paste0("dmnt_elig_", who), dmnt_bin)
  for (q in 1:8)
    psid_abridged <- collect_tv(psid_abridged,
                                sprintf("dmnt_q%d_any_%s", q, who), dmnt_q)
}
psid_abridged <- collect_tv(psid_abridged, "dmnt_score_cut_ind", dmnt_bin)
