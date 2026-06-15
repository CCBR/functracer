
## Plan: R Transitive Dependency Analysis for MOSuite Scripts

**TL;DR**: Use `treesitter` (modern AST parser) to extract function calls from scripts, recursively trace through MOSuite source code to build a complete dependency graph, and output in three formats (data frame for impact analysis, JSON for programmatic use, and SVG graph visualization).

### Steps

**Phase 1: Create a function to Extract Direct Function Calls from a script** *(depends on treesitter availability)*
- Parse a script (example: main.R) using `treesitter::tree_parse()` + walk AST to find function calls
- Optional: Filter for namespace calls from a specific package (example: MOSuite), extract function name + line number
- Output: data frame of direct calls from script

**Phase 2: Build Function Dependency Map for a given R package** *(parallel with Phase 1)*
- Scan all R files in the package (example: `MOSuite/R/`) and extract all function definitions
- For each function, parse its body and record what functions it calls
- Output: adjacency structure mapping function → dependencies

**Phase 3: Transitive Closure** *(depends on Phases 1 & 2)*
- Start with direct calls; recursively add their dependencies
- Track depth/hop count and call path for each transitive dependency
- Continue until reaching leaf functions (no further dependencies)
- Output: full transitive list with metadata

**Phase 4: Multi-Format Output** *(depends on Phase 3)*
- **CSV/TSV**: Sortable by depth/type; columns: function, dep_type (direct/indirect), hop_depth, call_path
- **JSON**: Nested by depth for programmatic queries
- **SVG graph**: Nodes = functions, edges = dependencies, colored by depth for visual impact analysis

### Relevant Files

- MOSuite-plot-venn-diagram/code/main.R — example target script
- R — MOSuite source code for example package
- NAMESPACE — MOSuite public API

### Verification

1. **Phase 1**: Manually confirm all MOSuite calls in main.R are captured (e.g., `load_moo_from_data_dir()`, `plot_volcano_summary()`, `plot_venn_diagram()`)
2. **Phase 2**: Spot-check one function (e.g., `plot_volcano_summary`) — verify its dependencies match manual inspection of source
3. **Phase 3**: Verify transitive closure finds non-obvious dependencies (functions called indirectly)
4. **Output validation**: No duplicates, all fields populated, answers "if I change function X, what's affected?"

### Implementation Language

**R** — leverage treesitter, igraph, readr

### Key Decisions

- **Parser**: Start with `treesitter` for speed; fallback to base R `parse()` if needed
- **Scope**: Analyze MOSuite only (not other packages) for focused impact analysis
- **Recursion**: Follow to completion (no depth limit; will converge)
- **Artifacts**: Save outputs as `venn_diagram_dependencies.{csv,json,svg}` in MOSuite-plot-venn-diagram directory

### Further Considerations

1. **User-defined helpers**: main.R defines `parse_numeric_vector()` — include or exclude? → *Recommend: Include but mark source = "main.R" vs "MOSuite"*
2. **Internal vs. exported functions**: Should output distinguish public API from internal? → *Recommend: Yes, add `is_exported` column*
3. **External dependencies**: Should argparse, dplyr, etc. be traced? → *Recommend: No—focus on MOSuite for clarity. Offer as future enhancement*
