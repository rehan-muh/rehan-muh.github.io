---
layout: page
permalink: /presentations/
title: presentations
description: presentations and posters listed from the CV section, with optional downloads and BibTeX copy.
nav: true
nav_order: 3
---

{% assign presentations_section = site.data.cv | where_exp: "section", "section.title == 'Presentations & Posters'" | first %}

<div class="publications">
  {% if presentations_section and presentations_section.contents %}
    {% for presentation in presentations_section.contents %}
      <div class="row mb-4">
        <div class="col-sm-2 text-sm-center mb-2 mb-sm-0">
          {% if presentation.year %}
            <span class="badge font-weight-bold danger-color-dark text-uppercase align-middle" style="min-width: 75px">{{ presentation.year }}</span>
          {% endif %}
        </div>
        <div class="col-sm-10">
          {% if presentation.title %}
            <div class="title">{{ presentation.title }}</div>
          {% endif %}
          {% if presentation.institution %}
            <div class="periodical"><em>{{ presentation.institution }}</em></div>
          {% endif %}

          <div class="links mt-2">
            {% if presentation.downloads %}
              {% for file in presentation.downloads %}
                {% if file.url %}
                  {% if file.url contains '://' %}
                    <a href="{{ file.url }}" class="btn btn-sm z-depth-0" role="button" download>
                      <i class="fa-solid fa-download"></i> {{ file.label | default: 'Download file' }}
                    </a>
                  {% else %}
                    <a href="{{ file.url | relative_url }}" class="btn btn-sm z-depth-0" role="button" download>
                      <i class="fa-solid fa-download"></i> {{ file.label | default: 'Download file' }}
                    </a>
                  {% endif %}
                {% endif %}
              {% endfor %}
            {% endif %}

            {% if presentation.bibtex %}
              <button
                class="btn btn-sm z-depth-0"
                type="button"
                data-copy-citation="{{ presentation.bibtex | strip | escape }}"
                aria-label="Copy presentation BibTeX citation"
              >
                <i class="fa-solid fa-clipboard"></i> Copy BibTeX
              </button>
            {% endif %}
          </div>
        </div>
      </div>
    {% endfor %}

  {% else %}
    <p>No presentations found in the CV data.</p>
  {% endif %}
</div>

<hr>

<p>
  To add downloadable files and BibTeX for a presentation, add optional <code>downloads</code> and <code>bibtex</code> fields in
  <code>_data/cv.yml</code> under <code>Presentations &amp; Posters</code> entries.
</p>

{% raw %}
<pre><code>- title: "Example presentation title"
  institution: "Conference name"
  year: 2026
  downloads:
    - label: "Slides"
      url: "/assets/files/example-slides.pdf"
    - label: "Poster"
      url: "/assets/files/example-poster.pdf"
  bibtex: |
    @inproceedings{example2026,
      title = {Example presentation title},
      author = {Doe, Jane},
      year = {2026}
    }</code></pre>
{% endraw %}
