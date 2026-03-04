**MASTER PLAN FOR CLAUDE CODE**

---

**CONTEXT**

This is an empirical macroeconomics paper on second-round pass-through of tradable to nontradable inflation in the EMU. The identification strategy uses the shared monetary policy and common exchange rate across EMU19 members as a natural laboratory. The baseline uses LP-IV with Baumeister-Hamilton oil shocks as instrument. The goal is to extend the paper with new robustness checks, new data, and heterogeneity analysis. Work autonomously across all phases but report results and flag issues at the end of each phase before proceeding. GitHub version control is active. You are the only contributor on this repository.

---

**PRELIMINARY: REPOSITORY CHECK**

Your first action before anything else is to read plan.md in the root of this repository. This file contains your complete instructions for all phases. Read it in full and confirm you have understood it by summarizing in one paragraph what this project is about, what the goal of the work is, and what the phase structure looks like. Do not proceed until the author confirms your summary is correct.

Then run git status to confirm you are in the correct directory and the repository is clean. Report the current branch, last commit message, and any uncommitted changes. Do not proceed past this point until both the plan summary and the repository status are confirmed by the author.

---

**PHASE 1: AUDIT AND MAPPING**

Before touching any code or data, do the following:

Locate the current version of the paper in the paper/ folder. Read it in full — every section, every table, every figure description. This is your primary reference document for understanding the empirical setup, the narrative, the contribution, and what results already exist. If there are multiple versions in paper/, read the most recently modified file and flag that you are doing so.

Read every do-file in the folder and map the full pipeline — data cleaning, variable construction, estimation, figure and table production.

Read the current dataset and document every variable, its source, frequency, coverage, and which countries are included.

Produce a written audit report covering:
- Current data sources and download locations
- Variables already constructed
- What the baseline LP-IV specification looks like exactly — dependent variable, instrument, controls, fixed effects, clustering, horizon range
- What results already exist and where they appear in the paper
- What is missing relative to the robustness plan below
- Any inconsistencies between the paper text, the do-files, and the data
- Any issues or errors in the existing code

Do not modify anything in Phase 1. Only read and report. Do not commit anything at the end of Phase 1 since nothing was modified. Wait for author confirmation before proceeding to Phase 2.

---

**PHASE 2: DATA EXTENSION**

Based on the audit, extend the dataset as follows:

Download and merge EU27 data for all variables currently available for EMU19 only. The eight non-EMU EU countries needed are: Bulgaria, Croatia, Czech Republic, Denmark, Hungary, Poland, Romania, Sweden. Sources are Eurostat for HICP components and ECB SDW for monetary variables where available.

Download the following new instruments:
- Global Supply Chain Pressure Index from the New York Fed public data page at https://www.newyorkfed.org/research/gscpi
- IMF Primary Commodity Price Index excluding energy from https://www.imf.org/en/Research/commodity-prices. Use the non-energy commodity price index or construct a food and agricultural sub-index explicitly excluding energy components
- Baltic Dry Index from FRED or the Baltic Exchange public data

Download country-level trade openness data from Eurostat or World Bank for all 27 countries.

Before merging any new series: print the exact URL, variable description, frequency, coverage period, and first five rows of raw data for every downloaded series. Wait for author confirmation before merging into the master dataset.

Harmonize all new series to match the existing dataset frequency, base period, and panel structure. Document every download with source URL and download date.

Produce a clean merged master dataset covering EMU19 plus EU8 with all variables needed for all phases below. Overwrite the existing master dataset freely — GitHub version control is active.

At phase completion: commit all changes with message "Phase 2: extended dataset to EU27, added GSCPI, BDI and IMF commodity series." Report a summary of what was downloaded, merged, and any issues encountered. Wait for author confirmation before proceeding.

---

**PHASE 3: BASELINE REPLICATION AND EXTENSION**

Replicate the existing baseline LP-IV results exactly to confirm the pipeline works on the extended dataset. If results differ from the paper, flag immediately and do not proceed until resolved.

Extend the baseline specification to the full EU27 sample. This is the core identification robustness check — the EU27 vs EMU19 comparison. Run identical specifications on both samples and produce a comparison table showing coefficients and confidence bands side by side.

Run the placebo test on the EU8 non-EMU countries only. Document whether pass-through is weaker, insignificant, or structurally different outside the monetary union.

Produce publication-quality impulse response figures with 90 and 68 percent confidence bands. Use a consistent color scheme and formatting throughout all figures in the paper.

At phase completion: commit all changes with message "Phase 3: baseline replicated, EU27 extension and placebo test complete." Report all results and flag anything surprising. Wait for author confirmation before proceeding.

---

**PHASE 4: INSTRUMENT ROBUSTNESS**

Run the baseline LP-IV three additional times replacing Baumeister-Hamilton with:

GSCPI shocks as instrument. Construct the instrument as innovations from a simple autoregressive model on the GSCPI series. Document the AR order selection explicitly.

IMF non-energy commodity price index shocks. Construct innovations analogously. This is the cleanest instrument for addressing the energy-only critique without requiring HICP reclassification.

Baltic Dry Index shocks. Construct innovations analogously.

For each instrument:
- Report first stage F-statistic
- Run Olea-Pflueger weak instrument robust confidence intervals
- Flag any instrument failing weak instrument diagnostics before including in the robustness table

Produce a single robustness table showing baseline and all three alternative instrument results side by side with first stage statistics in the bottom panel.

At phase completion: commit all changes with message "Phase 4: instrument robustness complete, three alternative instruments." Report first stage statistics for all instruments and flag any failures. Wait for author confirmation before proceeding.

---

**PHASE 5: HETEROGENEITY ANALYSIS**

Split the EMU19 into three economically motivated groups:

Core: Germany, France, Netherlands, Austria, Finland, Belgium.

Periphery: Italy, Spain, Portugal, Greece.

Small open economies: Ireland, Luxembourg, Estonia, Latvia, Lithuania, Slovakia, Slovenia, Malta, Cyprus.

Run the baseline LP-IV specification separately for each group. Produce impulse response figures for all three groups on the same axes for direct visual comparison.

Test formally whether impulse responses differ significantly across groups using a Wald test or equivalent. Report test statistics and interpretation.

Produce a heterogeneity results table and corresponding figures.

At phase completion: commit all changes with message "Phase 5: three-way EMU heterogeneity analysis complete." Report results and flag any group with insufficient observations or identification failures. Wait for author confirmation before proceeding.

---

**PHASE 6: STATISTICAL ROBUSTNESS**

Run the baseline at alternative lag structures: 2, 4, and 8 quarters. Produce a figure overlaying impulse responses across lag choices on the same axes.

Confirm the existing state dependence specifications — high vs low GSCPI, inflation volatility, depreciation vs appreciation — are stable on the extended dataset.

Confirm the pre vs post COVID sample split results are stable on the extended dataset.

At phase completion: commit all changes with message "Phase 6: statistical robustness complete, lag sensitivity and state dependence confirmed." Report all results. Wait for author confirmation before proceeding.

---

**PHASE 7: OUTPUT COMPILATION**

Produce a clean organized folder structure with:
- One do-file per phase clearly labeled and extensively commented so the author can follow every step
- All figures in publication-quality PDF or EPS format with consistent formatting
- All tables compiled into a single LaTeX file ready for insertion into the paper
- A written summary of all new results flagging anything surprising, unstable, or requiring author attention before writing

Do not write or modify the paper itself. Only produce results, tables, figures, and the summary report.

At phase completion: commit all changes with message "Phase 7: all outputs compiled, LaTeX tables and figures finalized."

---

**GENERAL INSTRUCTIONS**

Work phase by phase. Complete and report each phase fully before starting the next. Wait for explicit author confirmation at each phase boundary before proceeding.

If you encounter missing data, broken download links, ambiguous variable definitions, or unexpected results, flag them clearly, state your assumption, and wait for author guidance before proceeding.

GitHub version control is active and you are the only contributor. Overwrite files freely. Commit at the end of every phase with a descriptive commit message as specified above. Never commit mid-phase.

Comment every do-file extensively. Every regression, every variable construction, every merge should have a comment explaining what it does and why.

If a specification fails or produces unexpected results, document exactly what you tried, what the error or unexpected output was, and what you think the cause might be. Do not silently move on.

When in doubt about an economic judgment call — variable construction, instrument choice, sample restriction, how to handle missing observations, which base period to use — flag it explicitly and ask the author rather than deciding unilaterally.