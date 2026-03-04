# Results Summary — EMU Second-Round Pass-Through
*Generated at end of Phases 3–6. For author review before writing.*

---

## Phase 3: Baseline Replication and EU Extension

**EMU20 baseline (KP F = 208.6):** Replicates the paper's main result.
Peak θ = 0.964 at h=6, reverts toward 0.42 by h=13. Close match to the
paper's reported 0.93.

**EU27 extension (KP F = 230.5):** Adding the 7 non-EMU EU countries barely
changes the estimate (peak 0.887 at h=6). The pass-through is present even in
the broader sample, which slightly weakens the EMU-specific identification
narrative unless the EU7 placebo is handled carefully.

**EU7 placebo (KP F = 50.3):** Does NOT confirm absence of pass-through.
Insignificant at h=0–5, then large and significant at h=7–13 (peak 1.32 at
h=11, exceeding EMU20). Two plausible explanations: (1) Denmark and Bulgaria
have de facto EUR pegs; (2) CEE countries have deep EUR trade linkages that
produce delayed transmission. The placebo comparison is not clean as written.

**⚠️ ACTION REQUIRED:** The EU7 placebo result needs to be addressed in the
paper. Either (a) re-run the placebo dropping EUR-pegged countries (DK, BG)
to sharpen the test, or (b) reframe the identification so the EU7 comparison
is not presented as a falsification test.

**Figures:** `g_lp_emu20_replication.pdf`, `g_lp_eu27_comparison.pdf`,
`g_lp_eu7_placebo.pdf`, `g_lp_emu20_vs_eu7.pdf`
**Table:** `tab_phase3_comparison.tex`

---

## Phase 4: Instrument Robustness

**BH oil shock (baseline, KP F = 208.6):** Strong. Peak θ = 0.964 at h=6.

**IMF non-fuel commodity AR(1) (KP F ≈ 176):** Strong. Qualitatively confirms
the baseline but with systematically larger estimates (peak 2.45 at h=9).
The divergence likely reflects broader commodity price transmission — the IMF
basket includes agricultural commodities and metals beyond the oil channel,
which may transmit with longer lags through wage and input-cost channels.

**GSCPI AR(2) — EXCLUDED (KP F ≈ 2):** Fails the weak instrument test in
all forms (Bartik-weighted and direct). Estimates explode at long horizons
(θ₁₃ ≈ 14). Root cause: GSCPI shocks have no cross-country variation in
tradable sector exposure, so the Bartik wrapper creates no first-stage power.

**BDI:** Unavailable (FRED series discontinued).

**⚠️ NOTE FOR PAPER:** The IMF non-fuel result implies larger and more
persistent pass-through than BH. If you interpret the BH result as conservative
(oil-channel only), the IMF result gives the upper bound of the commodity
price transmission channel. The text should acknowledge this.

**Figures:** `g_lp_rob_instruments.pdf`
**Table:** `tab_rob_instruments.tex`

---

## Phase 5: Heterogeneity Across EMU Groups

**Core (DEU/FRA/NLD/AUT/FIN/BEL, KP F = 83.0):** Peak θ = 1.04 at h=6–7.

**Periphery (ITA/ESP/PRT/GRC, KP F = 36.5):** Largest pass-through at
short-to-medium horizons. Peak θ = 1.68 at h=6. Consistent with higher
inflation persistence and weaker credibility in peripheral EMU countries.

**Small open economies (10 countries, KP F = 112.8):** Peak θ = 1.00 at h=6,
nearly identical to Core.

**Formal test:** No pairwise group difference is statistically significant at
5% (max |t| = 1.43, Core vs Periphery at h=3). The Periphery pattern is
consistent and economically interpretable but not precisely estimated (N=4).

**⚠️ NOTE FOR PAPER:** The Periphery result is interesting — larger
pass-through in the countries with the most troubled fiscal and inflation
histories (Italy, Spain, Portugal, Greece) — but the N=4 sample means you
cannot make strong statistical claims. Flag this as suggestive.

**Figures:** `g_lp_heterogeneity.pdf`
**Table:** `tab_heterogeneity.tex`

---

## Phase 6: Statistical Robustness

### Lag sensitivity (L = 4, 8, 12, 16)

Stable. Peak estimate at h=6 ranges from 0.88 (L=16) to 1.04 (L=4). The
shape of the IRF is unchanged. KP F increases with lags (169 to 255) — more
controls tighten the first stage. **No concern here.**

### State dependence (extended dataset, EMU20)

All three regimes confirmed on the extended dataset. Patterns are sharper
than in the original analysis:

| Regime | Low-stress peak h=6 | High-stress peak h=6 |
|--------|--------------------|--------------------|
| GSCPI  | 0.39 (F=98.7) | 1.54 (F=108.2) |
| NEER   | 0.72 apprec (F=70.8) | 1.50 deprec (F=126.8) |
| Volatility | 0.63 (F=146.9) | 1.42 (F=62.3) |

State dependence is large, consistent, and all instruments strong. This
strengthens the paper's narrative that second-round pass-through is
conditional on the monetary and supply environment.

### Pre vs post COVID sample split

| Sample | KP F | Peak θ | Horizon |
|--------|------|--------|---------|
| Full (1998–2025) | 208.6 | 0.964 | h=6 |
| Pre-COVID (1998–2019) | 132.9 | 0.483 | h=6 |
| Post-COVID (2020–2025) | 74.3 | 1.43 | h=7–10 |

**⚠️ MAJOR FLAG — ACTION REQUIRED:** The baseline estimate is not stable
across sub-periods. Pre-COVID pass-through peaks at 0.48 (roughly half the
full-sample estimate); post-COVID peaks at 1.43 (50% larger). The full-sample
0.964 is a weighted average driven by the 2021–2022 inflation surge.

This finding is substantive and needs to be addressed in the paper, not buried
in a robustness section. There are two valid framings:

1. **State-dependence framing:** The pass-through is significantly larger in
   high-inflation episodes. This is consistent with the GSCPI and volatility
   regime results above and with the literature on non-linear price-setting.
   Present the full-sample as the average structural estimate and the split as
   state-dependence evidence.

2. **Stability concern framing:** The pre-COVID estimate is much weaker
   (though still positive), which raises the question of whether the result
   will persist once the 2021–2023 inflation episode fades from the sample.
   This should be flagged honestly.

The post-COVID F-statistic (74.3) is well above any weak-instrument threshold,
so the stronger post-COVID estimate is not a data artifact.

**Figures:** `g_lag_sensitivity.pdf`, `g_state_dependence.pdf`,
`g_sample_splits.pdf`

---

## Summary of Output Files

### Figures (`output/figures/`)
| File | Content | Phase |
|------|---------|-------|
| `g_lp_base.pdf` | Baseline IRF (original) | Pre-work |
| `g_lp_emu20_replication.pdf` | EMU20 replication | 3 |
| `g_lp_eu27_comparison.pdf` | EMU20 vs EU27 | 3 |
| `g_lp_eu7_placebo.pdf` | EU7 placebo | 3 |
| `g_lp_emu20_vs_eu7.pdf` | EMU20 vs EU7 overlay | 3 |
| `g_lp_rob_instruments.pdf` | BH vs IMF vs GSCPI | 4 |
| `g_lp_heterogeneity.pdf` | Core/Periphery/SOE | 5 |
| `g_lag_sensitivity.pdf` | L=4,8,12,16 | 6 |
| `g_state_dependence.pdf` | 3×2 regime grid (extended) | 6 |
| `g_sample_splits.pdf` | Full/Pre/Post COVID | 6 |

### Tables (`output/tables/`)
| File | Content | Phase |
|------|---------|-------|
| `tab_phase3_comparison.tex` | EMU20 / EU27 / EU7 at h=0,3,6,9,12 | 3 |
| `tab_rob_instruments.tex` | BH vs IMF (GSCPI excluded, noted) | 4 |
| `tab_heterogeneity.tex` | Core/Periphery/SOE + t-stats | 5 |

---

## Issues Requiring Author Decision Before Writing

1. **EU7 placebo is not clean** — significant pass-through at h>6, possibly
   driven by EUR-pegged countries (DK, BG). Consider dropping them or
   reframing the test.

2. **Sample split asymmetry** — the result is largely a post-2020 phenomenon.
   The paper's framing of a "structural" EMU pass-through parameter needs to
   account for this.

3. **IMF non-fuel instrument** gives larger estimates than BH — the paper
   should explain whether BH is the preferred instrument and why, or present
   both as a range.

4. **Periphery heterogeneity** — suggestive but imprecise (N=4). Either
   present cautiously or merge Periphery into a broader "non-Core" category.
