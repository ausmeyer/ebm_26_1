# EBM 26_1

Interactive Quarto/Reveal.js lecture for pediatric residents, built from 10 previously unused AAP PREP statistics questions.

## Lecture sequence

1. Q10 - stable incidence, increasing prevalence
2. Q24 - precision versus accuracy
3. Q20 - cross-sectional design
4. Q22 - randomized controlled trial for a causal treatment question
5. Q23 - case series for a rare disorder
6. Q25 - open-label randomized trial
7. Q18 - biological gradient and causal reasoning
8. Q15 - skewed data and test assumptions
9. Q16 - correlation and the coefficient of determination
10. Q44 - run charts for quality improvement

The complete 44-item coverage audit is in `question_coverage.csv`, and the source-backed claim audit is in `audit/evidence_claim_ledger.md`. Four questions were intentionally not included: Q14 and Q27 substantially repeat prior teaching, while Q19 and Q36 have answer keys that conflict with their own rationales.

## Render locally

```bash
QUARTO_R=/Library/Frameworks/R.framework/Versions/Current/Resources/bin/R quarto render index.qmd
```

The generated `index.html` is self-contained and is the GitHub Pages entry point. The explanatory figures are reproducible with:

```bash
Rscript assets/generate_visuals.R
```

All plotted data are labeled synthetic teaching data unless they reproduce values stated in a PREP stem.

## Present

- Arrow keys: advance or return
- `F`: fullscreen
- `S`: speaker view and notes
- `O`: slide overview
- Submit buttons: show the keyed answer and explanation
