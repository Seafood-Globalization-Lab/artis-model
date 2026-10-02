# AGENT

For developing the ARTIS model package

## Package
- **Name:** `artis` | R package | `devtools` for development
- **Description:** README.md | DESCRIPTION | NAMESPACE
- **GitHub:** https://github.com/Seafood-Globalization-Lab/artis-model

## Coding and Syntax Style
- Tidyverse style, `%>%` pipe opperator used in the `artis` package
- `data.table::fread()` / `fwrite()` for file I/O with the `data.table = FALSE` arguement always for fread
- `cli` package for all user-facing messages — never `message()`, `cat()`, or `print()`
- `dplyr::join_by()` for joins, `.by` over `group_by()`, `across()` for column-wise ops
- Roxygen2 documentation
- Default to Tidyverse style syntax when there are multiple options to achieve the same thing. 
- Use R script sections `# <header-title> --------------------------` and subsections 
`## <header-title> --------------------------`to break up sections and tasks within the code or script.
- in-line comments inserted above the relevant code and use the same indentation as the code the are referring to. 
Do NOT insert in-line comments after (to the right) of the code itself. 

## Written English Language Style
- use American z's in words like "Standardizing" rather than "Standardising" or "visualization" vs "visualisation" 
- no emojis unless specifically requested by user

## Response Style
- Prefer concise responses — no filler, no sycophancy
- Do not repeat yourself in a response
- Warnings and errors: quote them verbatim, don't paraphrase

## Markdown Style
- When asked to generate markdown syntax Do NOT use the following:
  - line breaks `---`
  - emojis
  - excessive bold `**example bold text**`
  - em dashes 
- When asked to generate markdown syntax OR to summarize for a GitHub issue or Wiki page - Please use the following:
  - Always bound the entire markdown document or response with `~~~` in a single chunk
  - Use markdown hierarchical headers to organize content or introduce subsections
  - Can use github alert blocks if user specifies the markdown is destine for GitHub (NOTE, TIP, IMPORTANT, WARNING, CAUTION)

## Broader Context
- The `artis` package is an open-science open-source piece of research software
- Development and distribution follow the FAIR convention https://www.go-fair.org/fair-principles/
- Design decisions are made to enhance transparencey and reproducibility of the code, assumptions, and resulting data. 
- Documentation is critical at the code and developer level all the way up to user facing documentation. 

## Skills (lab-genAI-toolbox)
Project skills are in `.lab-genAI-toolbox/skills/`. At the start of any relevant task,
check that directory and load the matching skill with the `skill` tool before proceeding.

## ARTIS specific info
- "taxa" refers to scientific names at any taxonomic classification rank, often used as shorthand for "sciname"
- "sciname" is a data column throughout ARTIS that refers to scientific names at any taxonomic classification rank