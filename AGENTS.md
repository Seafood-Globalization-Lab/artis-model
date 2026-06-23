# AGENT

For developing the ARTIS model package

## Package
- **Name:** `artis` | R package | `devtools` for development
- **Description:** README.md | DESCRIPTION | NAMESPACE
- **GitHub:** https://github.com/Seafood-Globalization-Lab/artis-model

## Coding Style
- Tidyverse style, `%>%` pipe opperator used in the `artis` package
- `data.table::fread()` / `fwrite()` for file I/O with the `data.table = FALSE` arguement always for fread
- `cli` package for all user-facing messages — never `message()`, `cat()`, or `print()`
- `dplyr::join_by()` for joins, `.by` over `group_by()`, `across()` for column-wise ops
- Roxygen2 documentation

## Response Style
- When asked for a response in markdown syntax OR to summarize for a GitHub issue - always bound the code with `~~~` in a single code chunk
- Prefer concise responses — no filler, no sycophancy
- Do not reapeat yourself in a response
- Warnings and errors: quote them verbatim, don't paraphrase

## Broader Context
- The `artis` package is an open-science open-source piece of research software
- Development and distribution follow the FAIR convention https://www.go-fair.org/fair-principles/
- Design decisions are made to enhance transparencey and reproducibility of the code, assumptions, and resulting data. 
- Documentation is critical at the code and developer level all the way up to user facing documentation. 