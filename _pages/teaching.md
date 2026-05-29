---
layout: page
permalink: /teaching/
title: teaching
nav: true
nav_order: 6
---

{% assign teaching_section = site.data.cv | where_exp: "section", "section.title == 'Teaching Experience'" | first %}

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
## {{ group }}
    {% endif %}

- **{{ teaching.title }}{% if teaching.year %} ({{ teaching.year }}){% endif %}**{% if institution_text != blank %} — {{ institution_text }}{% endif %}
  {% endfor %}
{% else %}
No teaching experience is currently listed in the CV data.
{% endif %}
