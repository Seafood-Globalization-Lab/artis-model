# ARTIS Model — CLAUDE.md

## Package
- **Name:** `artis` | R package | `devtools::load_all()` for development
- **Description:** Estimates bilateral seafood trade flows at species level
  by integrating FAO/SAU production data, BACI trade stats, FishBase/SeaLifeBase
  taxonomy, and HS product codes
- **GitHub:** https://github.com/Seafood-Globalization-Lab/artis-model

## Coding Style
- Tidyverse style, `%>%` perfered 
- `data.table::fread()` / `fwrite()` for file I/O with the data.table = FALSE arguement always for fread
- `cli` package for all user-facing messages — never `message()`, `cat()`, or `print()`
- `dplyr::join_by()` for joins, `.by` over `group_by()`, `across()` for column-wise ops
- Roxygen2 documentation

## Response Style
- When asked for a response in markdown syntax - always bound the code with `~~~` 
- Prefer concise responses — no filler, no sycophancy
- Do not summarize what you just did at the end of a response
- Warnings and errors: quote them verbatim, don't paraphrase