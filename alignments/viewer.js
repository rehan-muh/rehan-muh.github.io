// Read-only viewer for the alignments in alignments/data (written by
// causal/publish_alignments.py in the word-position project).
(function () {
  const LIMIT = 20;
  const BASE = document.currentScript.src.replace(/viewer\.js.*$/, "data/");
  const $ = (id) => document.getElementById(id);
  const cache = {};
  let index = [], sets = [], shown = [], offset = 0, timer = null;

  const esc = (s) => String(s).replace(/[&<>"]/g, (c) =>
    ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[c]);

  async function json(name) {
    if (!cache[name]) {
      const r = await fetch(BASE + name);
      if (!r.ok) throw new Error(r.status + " " + r.statusText);
      cache[name] = await r.json();
    }
    return cache[name];
  }

  function current() { return index[Number($("al-analysis").value)]; }

  function state() {
    const a = current();
    const p = new URLSearchParams();
    if (a) p.set("a", a.slug);
    if ($("al-q").value.trim()) p.set("q", $("al-q").value.trim());
    if ($("al-lang").value) p.set("lang", $("al-lang").value);
    if ($("al-changed").checked) p.set("changed", "1");
    if (offset) p.set("page", String(offset / LIMIT + 1));
    history.replaceState(null, "", "#" + p.toString());
  }

  async function start() {
    index = await json("index.json");
    const families = [...new Set(index.map((a) => a.family))];
    $("al-analysis").innerHTML = families.map((f) =>
      `<optgroup label="${esc(f)}">` + index.map((a, i) => a.family !== f ? "" :
        `<option value="${i}">${esc(a.analysis)}</option>`).join("") + "</optgroup>").join("");
    const p = new URLSearchParams(location.hash.slice(1));
    const at = index.findIndex((a) => a.slug === p.get("a"));
    $("al-analysis").value = String(at >= 0 ? at : 0);
    $("al-q").value = p.get("q") || "";
    $("al-changed").checked = p.get("changed") === "1";
    await open(p.get("lang") || "", Math.max(0, (Number(p.get("page")) || 1) - 1) * LIMIT);
  }

  async function open(lang, page) {
    const a = current();
    $("al-sets").innerHTML = '<div class="empty">Loading…</div>';
    sets = (await json(a.slug + ".json")).sets;
    const langs = [...new Set(sets.flatMap((s) => s.r.map((r) => r[0])))].sort();
    $("al-lang").innerHTML = '<option value="">All descendants</option>' +
      langs.map((l) => `<option>${esc(l)}</option>`).join("");
    $("al-lang").value = langs.includes(lang) ? lang : "";
    $("al-about").innerHTML =
      `${a.sets.toLocaleString()} cognate sets, ${a.languages} descendants; ${esc(a.kind)}. ` +
      (a.machine ? "Aligned by the neural aligner and reviewed. " : "Aligned by hand. ") +
      `<a href="${BASE + a.slug}.tsv" download>Download the table (TSV)</a>.`;
    $("al-sources").innerHTML = a.sources.map((s) => "<li>" + (s.url
      ? `<a href="${esc(s.url)}" rel="noopener">${esc(s.text)}</a>` : esc(s.text)) + "</li>").join("");
    filter(page);
  }

  // search ignores diacritics, length, stress, and IPA letter shapes:
  // "agni" finds ɐɡni, "sosa" finds ʂoːʂɐ
  const PLAIN = { "ɡ": "g", "ɐ": "a", "ɑ": "a", "æ": "a", "ɛ": "e", "ə": "e", "ɪ": "i", "ɨ": "i",
                  "ɔ": "o", "ʊ": "u", "ʋ": "v", "ɾ": "r", "ɽ": "r", "ʂ": "s", "ɕ": "s", "ʃ": "s",
                  "ʈ": "t", "ɖ": "d", "ɳ": "n", "ɲ": "n", "ŋ": "n", "ɭ": "l", "ʎ": "l", "β": "b",
                  "ð": "d", "θ": "t", "ɣ": "g", "χ": "x", "ʔ": "", "ː": "", "ˑ": "", "ˈ": "", "ˌ": "" };
  const fold = (t) => t.toLowerCase().normalize("NFD").replace(/[̀-ͯ]/g, "")
    .replace(/./gu, (c) => (c in PLAIN ? PLAIN[c] : c));

  function filter(page) {
    const q = fold($("al-q").value.trim());
    const lang = $("al-lang").value;
    const changed = $("al-changed").checked;
    shown = [];
    for (const s of sets) {
      if (q) {
        if (!s.k) s.k = fold(s.c + " " + s.a.join("") + " " + s.id);
        if (!s.k.includes(q)) continue;
      }
      let rows = lang ? s.r.filter((r) => r[0] === lang) : s.r;
      if (changed) rows = rows.filter((r) => r[2].includes("c"));
      if (rows.length) shown.push({ s, rows });
    }
    offset = Math.min(page || 0, Math.max(0, (Math.ceil(shown.length / LIMIT) - 1) * LIMIT));
    render();
  }

  function cell(value, mark, original) {
    const cls = ["seg", mark === "c" ? "c" : "", mark === "m" ? "m" : "", mark === "u" ? "u" : "",
                 value === "-" && mark !== "m" && mark !== "u" ? "lost" : "",
                 original !== undefined ? "fix" : ""].join(" ");
    const tip = (mark === "m" ? "morphology a descendant replaced: kept for positions, not counted. " : "") +
                (mark === "u" ? "not observable in the source's transcription. " : "") +
                (original !== undefined ? "corrected by hand; the source alignment had " + original : "");
    return `<td class="${cls}"${tip ? ` title="${esc(tip.trim())}"` : ""}>${esc(value)}</td>`;
  }

  function render() {
    if (!shown.length) {
      $("al-sets").innerHTML = '<div class="empty">No cognate sets match.</div>';
    } else {
      $("al-sets").innerHTML = shown.slice(offset, offset + LIMIT).map(({ s, rows }) => {
        const head = s.a.map((seg, i) => {
          const f = s.f[i];
          return `<th class="${f === "m" ? "m" : f === "u" ? "u" : ""}">${i === s.s ? "ˈ" : ""}${esc(seg)}</th>`;
        }).join("");
        const body = rows.map((r) => `<tr><td class="lang">${esc(r[0])}</td>` +
          r[1].map((v, i) => cell(v, r[2][i], r[3] ? r[3][String(i)] : undefined)).join("") + "</tr>").join("");
        return `<section class="set"><h3><span>${esc(s.c)}</span><span class="id">${esc(s.id)}</span></h3>` +
          `<table><tr class="anc"><th class="lang">ancestor</th>${head}</tr>${body}</table></section>`;
      }).join("");
    }
    const pages = Math.max(1, Math.ceil(shown.length / LIMIT));
    $("al-page").textContent = `${shown.length.toLocaleString()} set${shown.length === 1 ? "" : "s"} · page ${offset / LIMIT + 1} of ${pages}`;
    $("al-prev").disabled = offset === 0;
    $("al-next").disabled = offset + LIMIT >= shown.length;
    state();
  }

  $("al-analysis").addEventListener("change", () => { $("al-q").value = ""; open("", 0).catch(fail); });
  $("al-lang").addEventListener("change", () => filter(0));
  $("al-changed").addEventListener("change", () => filter(0));
  $("al-q").addEventListener("input", () => { clearTimeout(timer); timer = setTimeout(() => filter(0), 250); });
  $("al-prev").addEventListener("click", () => { offset = Math.max(0, offset - LIMIT); render(); $("al-sets").scrollIntoView(); });
  $("al-next").addEventListener("click", () => { offset += LIMIT; render(); $("al-sets").scrollIntoView(); });

  function fail(e) { $("al-sets").innerHTML = `<div class="empty">Could not load the data (${esc(e.message)}).</div>`; }
  start().catch(fail);
})();
