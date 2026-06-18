# AGENT

For developing the ARTIS model package

## Package
- **Name:** `artis` | R package | `devtools` for development
- **Description:** README.md | DESCRIPTION | NAMESPACE
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
- Do not reapeat yourself in a response
- Warnings and errors: quote them verbatim, don't paraphrase