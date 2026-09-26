// Filters publications and presentations on the about page by research-area tag.
(function () {
  const panel = document.getElementById("topic-results");
  if (!panel) return;

  const buttons = document.querySelectorAll(".topic-tag");
  const summary = panel.querySelector(".topic-summary");
  const items = Array.from(panel.querySelectorAll(".topic-item")).map((el) => ({
    el: el.closest("li") || el,
    group: el.closest(".topic-group").dataset.group,
    tags: (el.dataset.tags || "")
      .split("|")
      .map((t) => t.trim())
      .filter(Boolean),
  }));

  function plural(n, word) {
    return n + " " + word + (n === 1 ? "" : "s");
  }

  function show(topic) {
    buttons.forEach((b) => b.setAttribute("aria-pressed", String(b.dataset.topic === topic)));
    if (!topic) {
      panel.hidden = true;
      return;
    }

    const counts = { publications: 0, presentations: 0 };
    items.forEach((item) => {
      const match = item.tags.includes(topic);
      item.el.hidden = !match;
      if (match) counts[item.group]++;
    });
    panel.querySelectorAll(".topic-group").forEach((g) => {
      g.hidden = counts[g.dataset.group] === 0;
    });

    const label = document.querySelector('.topic-tag[data-topic="' + CSS.escape(topic) + '"]').textContent.trim();
    const total = counts.publications + counts.presentations;
    summary.textContent = total
      ? plural(counts.publications, "publication") + " and " + plural(counts.presentations, "presentation") + " in " + label
      : "Nothing listed under " + label + " yet.";
    panel.hidden = false;
  }

  buttons.forEach((b) =>
    b.addEventListener("click", () => {
      show(b.getAttribute("aria-pressed") === "true" ? null : b.dataset.topic);
    })
  );
  panel.querySelector(".topic-clear").addEventListener("click", () => show(null));
})();
