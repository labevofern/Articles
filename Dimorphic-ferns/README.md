# Leaf dimorphism in ferns
### Evolutionary transitions, ecological associations, and morphological diversification

**Manuscript:** In preparation for submission to *New Phytologist*  
**Research area:** Fern evolution · Comparative phylogenetics · Fertile-frond morphology  
**Available here:** R analysis scripts | **Raw data:** Not publicly available before manuscript acceptance

---

## About the study

Ferns display diverse degrees of differentiation between fertile and sterile fronds. This study explores the evolutionary history of **leaf dimorphism in ferns**, comparing three states recorded in the study's trait matrix:

| Code | Dimorphism state |
| --- | --- |
| 0 | Monomorphic |
| 1 | Hemidimorphic |
| 2 | Holodimorphic |

The research uses comparative phylogenetic analyses to investigate transitions among these states, phylogenetic clustering, associations with ecological habit, and the evolution of fertile-frond morphology. It additionally tests how sensitive the main analytical inferences are to alternative phylogenetic trees and evolutionary-rate heterogeneity.

**This repository documents the analytical workflow, not the conclusions of the manuscript.**

## Research questions

1. **Evolutionary transitions:** How do transition rates vary among monomorphism, hemidimorphism, and holodimorphism?
2. **Ancestral states:** What can stochastic character mapping reveal about evolutionary pathways and ancestral-state uncertainty?
3. **Phylogenetic signal:** Do related fern species tend to share the presence or absence of dimorphism?
4. **Ecological associations:** Is dimorphism associated with growth habit or the capacity to occupy more than one habit category?
5. **Morphological diversity:** How do fertile-frond architecture, morphological variation, and relative frond size evolve and covary?
6. **Robustness:** How much do alternative FTOL topologies, hidden-rate models, and model-fit boundaries affect inferences?

## Analytical approach

The pipeline uses the **Fern Tree of Life (FTOL)** through the `ftolr` R package. Species names and character codes are checked, and the trait matrix is aligned with a dated FTOL consensus phylogeny. An alternative maximum-likelihood FTOL tree is used to assess sensitivity while maintaining the same taxonomic sampling.

The methods implemented include:
- Discrete-character evolutionary models (Mk: ER, SYM, ARD, and ordered variants) and estimated transition-rate matrices.
- Stochastic character mapping (SIMMAP) and summaries of ancestral states, transitions, and time spent in states.
- Fritz–Purvis D statistics for selected binary traits.
- Phylogenetic logistic regressions and comparisons across growth habits.
- Pagel tests of correlated evolution between discrete traits.
- Hidden-rate models, topology-sensitivity analyses, and diagnostics of boundary-constrained fits.
- Evolutionary and descriptive analyses of fertile-frond architecture, morphological variation, and size relationships.

These are descriptions of procedures used by the scripts, **not claims about significant results or supported biological hypotheses**.

## Repository structure

```text
Dimorphic-ferns/
├── README.md
└── Scripts/
    ├── 00_CONFIGURACAO_E_FUNCOES.R
    ├── 01_PREPARAR_DADOS_E_FILOGENIA.R
    ├── 02_EVOLUCAO_DIMORFISMO.R
    ├── 03_SIMMAP_DIMORFISMO.R
    ├── 04_SINAL_FILOGENETICO_DIMORFISMO.R
    ├── 05_HABITO_E_DIMORFISMO.R
    ├── 06_TAXAS_OCULTAS.R
    ├── 07_EVOLUCAO_DAS_SOLUCOES_MORFOLOGICAS.R
    ├── 08_CORRELACOES_E_SINAL_MORFOLOGICO.R
    ├── 09_ANALISES_DESCRITIVAS_E_EXPLORATORIAS.R
    ├── 10.1_SENSIBILIDADE_FILOGENETICA.R
    ├── 10.2_CONGRUENCIA_TOPOLOGICA.R
    ├── 11.1_SENSIBILIDADE_PAGEL_HIDDEN_RATES.R
    ├── 11.2_VALIDACAO_LIMITES_MODELOS_OCULTOS.R
    ├── 12_GERAR_FIGURAS.R
    └── RUN_ALL.R
```

### Script guide

| Script | Function |
| --- | --- |
| `00` | Shared paths, character codes, configuration, and utility functions. |
| `01` | Data validation, taxonomic reconciliation, and phylogeny preparation. |
| `02` | Mk model comparison and transition rates for dimorphism. |
| `03` | SIMMAP and ancestral-state reconstructions. |
| `04` | Phylogenetic signal in dimorphism occurrence. |
| `05` | Growth-habit associations and phylogenetic logistic regression. |
| `06` | Hidden evolutionary-rate models. |
| `07` | Evolutionary models for fertile-frond morphological characters. |
| `08` | Correlated evolution and phylogenetic signal of morphology. |
| `09` | Descriptive exploration of morphological combinations. |
| `10.1` | Phylogenetic sensitivity: FTOL consensus vs maximum-likelihood tree. |
| `10.2` | Quantitative topological congruence between the two trees. |
| `11.1` | Sensitivity of Pagel tests to hidden-rate heterogeneity. |
| `11.2` | Targeted validation of fits close to rate boundaries. |
| `12` | Re-creates figures from saved `.rds` results. |
| `RUN_ALL.R` | Core batch-execution driver (see caveat below). |

## Data availability

**The raw research data are not publicly available during manuscript preparation and submission.** The authors intend to make the original species-by-trait dataset publicly available **after acceptance of the associated article in *New Phytologist***. An archival link and persistent identifier will be included here and in the manuscript's Data Availability Statement once the dataset has been released.

The code is publicly shared at this stage to document the analytical procedures. The primary input matrix and generated intermediate objects are not included. **The public repository alone is therefore not presently sufficient to rerun all analyses.**

## Reproducibility and execution

All analyses are written in **R** and can be sourced from **RStudio**. Principal dependencies include `ape`, `caper`, `corHMM`, `dplyr`, `ftolr`, `ggplot2`, `openxlsx`, `patchwork`, `phylolm`, `phytools`, `rgbif`, `scales`, `stringr`, `tidyr`, and `viridisLite`. The batch runner checks packages and includes instructions to install [`ftolr`](https://github.com/fernphy/ftolr).

**Directory naming:** The public GitHub directory is called `Scripts/`, but the current configuration code expects scripts in a local folder called **`Scripts_/`**. For local execution, copy the scripts to `Scripts_/` within your working project, without renaming the public repository directory.

Expected local workspace:

```text
Project/
├── Scripts_/                    # local working copy of the scripts
│   ├── _Logs/                   # generated
│   ├── _Modelos/                # generated
│   └── _Objetos_R/              # generated
├── Tabelas_/
│   ├── Matriz_final_completa.xlsx   # input matrix, not yet public
│   └── _Dados_processados/     # generated
└── Imagens_/                    # generated figures
```

The file `Tabelas_/Matriz_final_completa.xlsx` is required by `01_PREPARAR_DADOS_E_FILOGENIA.R`. The first analytical script prepares and stores aligned data and tree objects; subsequent steps read those generated `.rds` objects. The script headers describe their dependencies and outputs.

**Batch-runner caveat:** `RUN_ALL.R` currently references the former filenames `10_SENSIBILIDADE_FILOGENETICA.R` and `11_SENSIBILIDADE_PAGEL_HIDDEN_RATES.R`. Their actual names in this repository are `10.1_SENSIBILIDADE_FILOGENETICA.R` and `11.1_SENSIBILIDADE_PAGEL_HIDDEN_RATES.R`. Update these two entries in a local copy of the runner before running it. Scripts `10.2`, `11.2`, and `12` are separate follow-up/figure-generation steps not included in its current script list.

**Until the raw input matrix is released, this is a documented workflow rather than a complete publicly executable replication package.**

## Citation

The full manuscript citation and DOI will be added when available. In the meantime, reference this GitHub repository when discussing the publicly shared analysis code.

---

*This repository supports an ongoing research project. Code and documentation may change during manuscript preparation and peer review.*
