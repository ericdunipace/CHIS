# DAC Release

Files in this folder were produced by `R/CHIS_private.R` inside the CHIS Data Access Center and released after disclosure review. The local scripts in `R/localCode/` and `Documents/Manuscript/chis_manuscript.qmd` read them from here. Only the most recent release is kept; earlier releases are in git history.

- Release ID: TODO
- Release date: TODO (files dated 2026-09-29)
- Commit of `R/CHIS_private.R` / `R/Functions.R` run in the DAC: TODO
- R version in the DAC: 4.2.3

`table1_raw_basic.csv` is dated 20 minutes after the other files (15:41 vs 15:20–15:22). Note here whether it came from a separate release.

## MD5 checksums

```text
8c7265c424c11549a96b29de056d8c10  climiate_anxiety_map_data.csv
39f00c1e10f2d4848f32ef825d6723bb  fe_model_summary.csv
416b9b7beded78a947c59beebb54937d  fe_vcov.csv
22d4e31c36dba5532672323cdbf9fa88  glm_model_summary.csv
598de177244dd9ab139a7ae3710e3eeb  glm_suf_stat.csv
f0b2cdd802695d47cce7ee5dccda7055  glm_vcov.csv
5c0729e33165f61324bcadf9fc7f6068  heatwave_map.pdf
29afb215c599677e96c46af032605c8d  table1.csv
d341f9890c94e045360db1c32e467214  table1_basic.csv
63b1efd858f070443134f7aa9ea93eb0  table1_display.csv
0463c220b3ba896655baa6bd26b7f533  table1_raw.csv
eb08be73a0cd167222c342a8162f4ef4  table1_raw_basic.csv
1735b9eaadeb60dcdc6c073c378698ae  table1_raw_display.csv
05eca32ec979f2941313ffa593898998  tmax_map.pdf
```

Regenerate with `md5 -r Outputs_DAC/*.csv Outputs_DAC/*.pdf` (macOS) or `md5sum Outputs_DAC/*.csv Outputs_DAC/*.pdf` (Linux).
