# =====================================================================
# R/generate/employment.R
# Derives: emp_stat_{1m,2m,3m}_* (combined), emp_work[_rp/_sp]_* (employed?),
#          emp_work_mm[_rp/_sp]_* (employed in any of the 1st-3rd mentions).
# =====================================================================

# combined employment status (RP/SP/ind)
for (m in c("emp_stat_1m", "emp_stat_2m", "emp_stat_3m")) combine_roles(m)

g_col <- function(stub, y) psid_abridged[[paste0(stub, "_", y)]]
ework  <- function(s) case_when(s %in% 1 ~ 1, is.na(s) ~ NA_real_, .default = 0)

# emp_work — currently employed (status==1) for combined / rp / sp
for (suf in c("", "_rp", "_sp"))
  gen_tv(paste0("emp_work", suf), function(y) {
    s <- g_col(paste0("emp_stat_1m", suf), y); if (is.null(s)) return(NULL); ework(s)
  })

# emp_work_mm — employed in any of the up-to-3 job mentions (1994+); else emp_work
for (suf in c("", "_rp", "_sp"))
  gen_tv(paste0("emp_work_mm", suf), function(y) {
    s1 <- g_col(paste0("emp_stat_1m", suf), y)
    s2 <- g_col(paste0("emp_stat_2m", suf), y)
    s3 <- g_col(paste0("emp_stat_3m", suf), y)
    if (is.null(s1)) return(NULL)
    if (y >= 1994 && !is.null(s2) && !is.null(s3)) {
      case_when(s1 %in% 1 | s2 %in% 1 | s3 %in% 1 ~ 1,
                is.na(s1) & is.na(s2) & is.na(s3) ~ NA_real_, .default = 0)
    } else ework(s1)
  })
