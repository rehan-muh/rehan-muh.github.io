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
      {% assign citation_id = 'presentation-citation-' | append: forloop.index %}
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
                  {% assign is_absolute_url = file.url contains '://' %}
                  {% assign file_url_first_char = file.url | slice: 0, 1 %}
                  {% assign is_root_relative = file_url_first_char == '/' %}

                  {% if is_absolute_url %}
                    <a href="{{ file.url }}" class="btn btn-sm z-depth-0" role="button" target="_blank" rel="noopener noreferrer">
                      <i class="fa-solid fa-download"></i> {{ file.label | default: 'Download file' }}
                    </a>
                  {% elsif is_root_relative %}
                    <a href="{{ file.url | relative_url }}" class="btn btn-sm z-depth-0" role="button" download>
                      <i class="fa-solid fa-download"></i> {{ file.label | default: 'Download file' }}
                    </a>
                  {% else %}
                    <a href="{{ file.url | prepend: '/assets/pdf/' | relative_url }}" class="btn btn-sm z-depth-0" role="button" download>
                      <i class="fa-solid fa-download"></i> {{ file.label | default: 'Download file' }}
                    </a>
                  {% endif %}
                {% endif %}
              {% endfor %}
            {% endif %}

            {% if presentation.bibtex %}
              <button class="btn btn-sm z-depth-0 citation-modal-trigger" type="button" data-citation-target="{{ citation_id }}">BibTeX</button>
              <textarea id="{{ citation_id }}" class="d-none" aria-hidden="true">{{ presentation.bibtex | strip }}</textarea>
            {% endif %}

            {% if presentation.download %}
              {% assign is_absolute_download = presentation.download contains '://' %}
              {% assign download_first_char = presentation.download | slice: 0, 1 %}
              {% assign is_root_download = download_first_char == '/' %}
              {% if is_absolute_download %}
                <a href="{{ presentation.download }}" class="btn btn-sm z-depth-0" role="button" target="_blank" rel="noopener noreferrer">Download</a>
              {% elsif is_root_download %}
                <a href="{{ presentation.download | relative_url }}" class="btn btn-sm z-depth-0" role="button" download>Download</a>
              {% else %}
                <a href="{{ presentation.download | prepend: '/assets/files/presentations/' | relative_url }}" class="btn btn-sm z-depth-0" role="button" download>Download</a>
              {% endif %}
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
  To add a right-side <code>BibTeX</code> button and <code>Download</code> link, add optional <code>bibtex</code> and <code>download</code> fields in
  <code>_data/cv.yml</code> under <code>Presentations &amp; Posters</code> entries.
</p>

{% raw %}
<pre><code>- title: "Example presentation title"
  institution: "Conference name"
  year: 2026
  download: "example-slides.pdf" # resolves to /assets/files/presentations/example-slides.pdf
  bibtex: |
    @inproceedings{example2026,
      title = {Example presentation title},
      author = {Doe, Jane},
      year = {2026}
    }</code></pre>
{% endraw %}
