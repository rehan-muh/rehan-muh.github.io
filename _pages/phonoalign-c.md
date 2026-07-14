---
layout: page
title: PhonoAlign-C
permalink: /phonoalign-c/
description: Multilingual consistency alignment for historical phonology
nav: true
nav_order: 6
---

**PhonoAlign-C** aligns cognate forms by combining the phonological strengths of complete SCA with evidence from the rest of the multilingual cognate set. A correspondence that is uncertain in one language pair can be recovered when the same homology is supported through several other languages.

<div style="display:flex;gap:.75rem;flex-wrap:wrap;margin:1.25rem 0 1.75rem 0">
  <a class="btn btn-primary" href="https://colab.research.google.com/github/rehan-muh/rehan-muh.github.io/blob/main/assets/notebooks/phonoalign_c_colab.ipynb" target="_blank" rel="noopener">Open the alignment notebook</a>
  <a class="btn btn-outline-primary" href="{{ '/assets/data/phonoalign_c_example.csv' | relative_url }}">Download example data</a>
</div>

## What the method does

Most alignment systems score each language pair independently. PhonoAlign-C first runs the complete Sound-Class-Based Alignment system on every pair in a cognate set. It then asks whether each proposed segment link is also recovered through third languages. Direct SCA evidence and this multilingual consistency evidence are combined in a normalized conditional random field, which returns both a conventional alignment and posterior probabilities for individual homology links.

The full evaluation compares identity, Covington, ALINE, feature distance, Dolgopolsky classes, controlled SCA scoring, IWSA, Global PMI, information-weighted PMI, complete SCA, and PhonoAlign-C. Complete SCA is markedly stronger than the other pairwise systems on expert-edited data. Adding multilingual consistency improves it further, especially for multiple-alignment columns.

| Evaluation | SCA | PhonoAlign-C |
|---|---:|---:|
| Source held out | 0.964 | **0.969** |
| Family held out | 0.961 | **0.969** |
| Multiple-alignment column F1 | 0.912 | **0.934** |
| Tone | 0.939 | **0.946** |
| Multilingual simulation | 0.903 | **0.915** |
| External Romance diagnostic | 0.943 | **0.947** |

## Generate alignments from your own data

The Colab notebook accepts **CSV, TSV, or XLSX** files in long format. Each row should contain:

- a cognate-set identifier;
- a language or variety name;
- a tokenized phonological form, with segments separated by spaces.

The default column names are `cognate_set`, `language`, and `tokens`, but the notebook lets you choose different columns. For each cognate set it creates a complete SCA pair library, applies the multilingual consistency CRF, and writes a new `phonoalign_c_alignment` column. It also exports pairwise links and posterior link probabilities for closer inspection.

A minimal input looks like this:

```text
cognate_set,language,tokens
mother,English,m ʌ ð ə r
mother,Dutch,m uː d ə r
mother,German,m ʊ t ɐ
```

The notebook runs entirely in the user's Colab session. Uploaded files are not sent to this website.

## Evaluation resources

The reported experiments deliberately use several datasets because they test different claims:

- **BDPA segmental pairs:** 7,198 manually edited pair projections from 750 multilingual cognate sets, evaluated with source- and family-held-out splits.
- **BDPA multiple alignments:** all 750 expert-edited rectangular alignments, evaluated with column, sum-of-pairs, and exact-recovery metrics.
- **BDPA tone:** 1,089 tone-sequence alignments.
- **Simulation:** five seeds under fourteen historical and annotation processes, where the true ancestry links are known.
- **Goldstein et al. Romance data:** an independently constructed external diagnostic from manually aligned Nytril matrices.

These resources should not be treated as interchangeable. Segmental pairs test ordinary link recovery; multiple alignments test global column structure; tone tests suprasegmental transfer; simulation tests recovery of known ancestry; and Romance provides an external annotation tradition.

## Recommended use

PhonoAlign-C assumes that cognate sets and tokenized forms are already available. It is designed to support expert analysis, not to replace it. Posterior link probabilities should be inspected when the model is uncertain, and automatic alignments should be reviewed before they are used for reconstruction or phylogenetic coding.

## Citation

A citation entry will be added after archival publication. Until then, please cite the paper PDF and this project page, and retain the original dataset citations listed in the paper.
