---
layout: page
permalink: /presentations/
title: presentations
description: presentations and posters listed from the CV section.
nav: true
nav_order: 3
---

{% assign presentations_section = site.data.cv | where_exp: "section", "section.title == 'Presentations & Posters'" | first %}

<div class="publications">
  {% if presentations_section and presentations_section.contents %}
    {% for presentation in presentations_section.contents %}
      {% assign citation_id = 'presentation-citation-' | append: forloop.index %}
      {% assign bibtex_panel_id = 'presentation-bibtex-panel-' | append: forloop.index %}
      {% assign generated_key = presentation.title | default: 'presentation' | slugify | append: presentation.year %}

      {% if presentation.bibtex %}
        {% assign presentation_bibtex = presentation.bibtex | strip %}
      {% else %}
        {% capture presentation_bibtex %}@inproceedings{ {{ generated_key }},

title = { {{ presentation.title | default: 'Presentation' }} },
author = { Muhammad Rehan },
booktitle = { {{ presentation.institution | default: 'Conference presentation' }} },
year = { {{ presentation.year | default: '' }} }
}{% endcapture %}
{% endif %}

      {% assign presentation_download = presentation.download %}
      {% if presentation_download == blank and presentation.downloads and presentation.downloads.first and presentation.downloads.first.url %}
        {% assign presentation_download = presentation.downloads.first.url %}
      {% endif %}

      {% assign show_download_button = false %}
      {% if presentation_download != blank %}
        {% if presentation_download contains '://' %}
          {% assign show_download_button = true %}
        {% elsif presentation_download | slice: 0, 1 == '/' %}
          {% assign root_relative_path = presentation_download | remove_first: '/' %}
          {% capture root_file_exists %}{% file_exists {{ root_relative_path }} %}{% endcapture %}
          {% if root_file_exists == 'true' %}
            {% assign show_download_button = true %}
          {% endif %}
        {% else %}
          {% assign local_presentation_path = presentation_download | prepend: 'assets/files/presentations/' %}
          {% capture local_file_exists %}{% file_exists {{ local_presentation_path }} %}{% endcapture %}
          {% if local_file_exists == 'true' %}
            {% assign show_download_button = true %}
          {% endif %}
        {% endif %}
      {% endif %}

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
            <button class="btn btn-sm z-depth-0 bibtex-toggle" type="button" data-bibtex-target="{{ bibtex_panel_id }}">BibTeX</button>
            <textarea id="{{ citation_id }}" class="d-none" aria-hidden="true">{{ presentation_bibtex }}</textarea>

            {% if show_download_button %}
              {% assign is_absolute_download = presentation_download contains '://' %}
              {% assign download_first_char = presentation_download | slice: 0, 1 %}
              {% assign is_root_download = download_first_char == '/' %}
              {% if is_absolute_download %}
                <a href="{{ presentation_download }}" class="btn btn-sm z-depth-0" role="button" target="_blank" rel="noopener noreferrer">Download</a>
              {% elsif is_root_download %}
                <a href="{{ presentation_download | relative_url }}" class="btn btn-sm z-depth-0" role="button" download>Download</a>
              {% else %}
                <a href="{{ presentation_download | prepend: '/assets/files/presentations/' | relative_url }}" class="btn btn-sm z-depth-0" role="button" download>Download</a>
              {% endif %}
            {% endif %}
          </div>

          <div id="{{ bibtex_panel_id }}" class="d-none mt-2">
            <pre class="mb-2"><code>{{ presentation_bibtex }}</code></pre>
            <button class="btn btn-sm z-depth-0 bibtex-copy-btn" type="button" data-copy-target="{{ citation_id }}">
              <i class="fa-solid fa-clipboard"></i> Copy
            </button>
          </div>
        </div>
      </div>
    {% endfor %}

{% else %}

<p>No presentations found in the CV data.</p>
{% endif %}

</div>
