---
layout: page
permalink: /teaching/
title: teaching
nav: true
nav_order: 6
---

{% assign teaching_section = site.data.cv | where_exp: "section", "section.title == 'Teaching Experience'" | first %}

<div class="teaching-list">
  {% if teaching_section and teaching_section.contents %}
    {% assign current_group = "" %}
    {% for teaching in teaching_section.contents %}
      {% assign institution_text = teaching.institution | default: "" %}
      {% if institution_text contains "Grinnell College" %}
        {% assign group = "Grinnell College" %}
      {% else %}
        {% assign group = "UCLA" %}
      {% endif %}

      {% if group != current_group %}
        {% assign current_group = group %}
        <h2 class="teaching-group">{{ group }}</h2>
      {% endif %}

      <div class="teaching-item">
        <div class="teaching-title">
          {{ teaching.title }}
          {% if teaching.year %}<span class="teaching-year">{{ teaching.year }}</span>{% endif %}
        </div>
        {% if institution_text != blank %}<div class="teaching-institution">{{ institution_text }}</div>{% endif %}
      </div>
    {% endfor %}

{% else %}

<p>No teaching experience is currently listed in the CV data.</p>
{% endif %}

</div>
