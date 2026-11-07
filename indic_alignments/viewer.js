// Viewer and editor for the Indic alignments (data/*.json, written by
// causal/publish_indic.py in the word-position project).
//
// One sheet is loaded at a time. Edits go straight to Google Sheets through the web app
// named in config.js, which also returns every saved edit, so all readers see them. With
// no web app set, or no passphrase given, the page is read-only.
//
// A row holds two strings, cell for cell: "k a r [a t i] · ·" over "k a r [- - -] [n aː]".
// · under a reflex segment: the reflex added it. -: lost. Square brackets: morphology.
(function () {
  const GAP = "-", ADDED = "·";

  function parseCells(text) {
    const out = [];
    let inside = false;
    String(text).split(" ").forEach((token) => {
      if (!token) return;
      const opens = token[0] === "[", shuts = token[token.length - 1] === "]";
      const bare = token.replace(/^\[/, "").replace(/\]$/, "");
      out.push({ seg: bare, morph: inside || opens });
      inside = (inside || opens) && !shuts;
    });
    return out;
  }
  function renderCells(cells) {
    return cells.map((c, n) => {
      const opens = c.morph && (n === 0 || !cells[n - 1].morph);
      const shuts = c.morph && (n === cells.length - 1 || !cells[n + 1].morph);
      return `${opens ? "[" : ""}${c.seg}${shuts ? "]" : ""}`;
    }).join(" ");
  }
  // [{top, bottom, morph}] for a pair, from its two strings
  function parsePair(anc, ref) {
    const top = parseCells(anc), bottom = parseCells(ref);
    return top.map((c, i) => ({ top: c.seg, bottom: bottom[i] ? bottom[i].seg : GAP, morph: c.morph }));
  }
  function renderPair(cells) {
    return [renderCells(cells.map((c) => ({ seg: c.top, morph: c.morph }))),
            renderCells(cells.map((c) => ({ seg: c.bottom, morph: c.morph })))];
  }
  const clean = (v) => {
    const s = String(v).normalize("NFC").trim();
    return s === "" || s === "–" || s === "—" ? GAP : s.replace(/\s+/g, "");
  };

  if (typeof document === "undefined") {
    module.exports = { parseCells, renderCells, parsePair, renderPair, clean };
    return;
  }

  const BASE = document.currentScript.src.replace(/viewer\.js.*$/, "data/");
  const ENDPOINT = (window.INDIC_CONFIG && window.INDIC_CONFIG.endpoint) || "";
  const STORE = "indic-alignments-v1";
  const PAGE = 25, ROWS = 300;
  const $ = (id) => document.getElementById(id);
  const esc = (s) => String(s).replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[c]);
  const fold = (s) => String(s).normalize("NFD").replace(/[̀-ͯ]/g, "").replace(/ʰ/g, "h").toLowerCase();
  const num = (n) => Number(n).toLocaleString("en-US");

  let INDEX = [], current = null, SETS = [], hits = [], shown = 0, wait = null;
  let editing = false, brush = false, changes = {}, links = {};
  let identity = { editor: "", pass: "" }, queue = [], sending = false;
  const pairs = new Map();

  function remember() {
    try { localStorage.setItem(STORE, JSON.stringify({ editor: identity.editor, pass: identity.pass, queue })); } catch (e) { /* page still works */ }
  }
  function recall() {
    try {
      const v = JSON.parse(localStorage.getItem(STORE)) || {};
      identity = { editor: v.editor || "", pass: v.pass || "" };
      queue = v.queue || [];
    } catch (e) { queue = []; }
  }

  async function post(body) {
    const r = await fetch(ENDPOINT, { method: "POST", body: JSON.stringify(body) });
    if (!r.ok) throw new Error(`${r.status}`);
    return r.json();
  }
  function say(text, bad) {
    const el = $("ia-saved");
    el.textContent = text;
    el.classList.toggle("bad", !!bad);
  }
  async function flush() {
    if (sending || !queue.length) return;
    sending = true;
    while (queue.length) {
      say(queue.length === 1 ? "Saving…" : `Saving ${queue.length} pairs…`);
      try {
        const res = await post({ ...queue[0], action: "save", pass: identity.pass, editor: identity.editor });
        if (!res.ok) throw new Error(res.error || "refused");
        const item = queue.shift();
        const key = `${item.sheet}\t${item.key}`;
        if (item.clear) delete changes[key];
        else changes[key] = { ...item, saved: res.saved, editor: identity.editor };
        remember();
        say(`Saved to Google Sheets at ${new Date().toTimeString().slice(0, 5)}`);
      } catch (e) {
        sending = false;
        remember();
        say(`Not saved (${e.message}). ${queue.length} waiting; they will be sent with your next edit.`, true);
        return;
      }
    }
    sending = false;
  }

  function overlay(pair) {
    const c = changes[`${current.slug}\t${pair.key}`];
    pair.cells = parsePair(c ? c.anc : pair.base[0], c ? c.ref : pair.base[1]);
    pair.morph = c ? c.morph : pair.base[2];
    pair.status = c ? c.status : pair.base[3];
    pair.note = c ? c.note : pair.base[4];
    pair.edited = c ? `${c.editor || ""} ${String(c.saved || "").slice(0, 16)}`.trim() : "";
    pair.ranks = [];
    pair.cells.forEach((cell, i) => { if (cell.top !== ADDED) pair.ranks.push(i); });
  }
  function save(pair) {
    const [anc, ref] = renderPair(pair.cells);
    const same = anc === pair.base[0] && ref === pair.base[1] && pair.status === pair.base[3] && pair.note === pair.base[4];
    const item = { sheet: current.slug, key: pair.key, row: pair.n + 2, anc, ref, morph: pair.morph, status: pair.status, note: pair.note };
    if (same) { item.clear = true; item.morph = pair.base[2]; pair.morph = pair.base[2]; }
    pair.edited = same ? "" : `${identity.editor} ${new Date().toISOString().slice(0, 16)}`.trim();
    delete changes[`${current.slug}\t${pair.key}`];
    if (!same) changes[`${current.slug}\t${pair.key}`] = { ...item, editor: identity.editor, saved: new Date().toISOString() };
    queue = queue.filter((q) => !(q.sheet === item.sheet && q.key === item.key));
    queue.push(item);
    remember();
    flush();
  }

  // ------------------------------------------------------------- drawing
  function segCell(pair, i) {
    const cell = pair.cells[i];
    const lost = cell.bottom === GAP;
    const was = parsePair(pair.base[0], pair.base[1])[i];
    const fixed = !was || was.bottom !== cell.bottom || was.morph !== cell.morph;
    const cls = ["seg", lost ? "lost" : cell.bottom !== cell.top ? "c" : "", cell.morph ? "m" : "", fixed ? "fix" : ""].join(" ");
    const tip = fixed && was ? ` title="first alignment: ${esc(was.bottom)}${was.morph ? " (morphology)" : ""}"` : "";
    let body = esc(lost ? GAP : cell.bottom);
    if (editing && !brush) body = `<input class="cell" data-i="${i}" value="${esc(lost ? "" : cell.bottom)}" aria-label="Reflex of ${esc(cell.top)}">`;
    if (editing && brush) body = `<button class="mk" type="button" data-i="${i}" aria-pressed="${cell.morph}">${esc(lost ? GAP : cell.bottom)}</button>`;
    return `<td class="${cls}" data-i="${i}"${tip}>${body}</td>`;
  }
  // cells of a pair by slot: added[p] are the segments the reflex has where the ancestor has
  // a gap, before ancestor segment p (the last entry is the end of the word), and rank[p] is
  // that segment's cell. Each gap in the ancestor is a green column of its own.
  function layout(pair) {
    const added = [[]], rank = [];
    pair.cells.forEach((cell, i) => {
      if (cell.top === ADDED) added[added.length - 1].push(i);
      else { rank.push(i); added.push([]); }
    });
    return { added, rank };
  }
  function addedCell(pair, i) {
    return i === undefined ? `<td class="seg add none">${GAP}</td>` : `<td class="seg add">${esc(pair.cells[i].bottom)}</td>`;
  }
  // `slots[p]` is how many columns the added segments before p need in this table
  function rowHTML(pair, slots) {
    const excluded = /excluded/.test(pair.status);
    let h = `<tr class="${excluded ? "ex" : ""}" data-key="${esc(pair.key)}">`;
    h += `<td class="lang">${esc(pair.language)}</td><td class="form">${esc(pair.form)}</td>`;
    const { added, rank } = layout(pair);
    slots.forEach((width, p) => {
      for (let k = 0; k < width; k++) h += addedCell(pair, added[p] ? added[p][k] : undefined);
      if (p < rank.length) h += segCell(pair, rank[p]);
      else if (p < slots.length - 1) h += "<td></td>";
    });
    const status = pair.status && pair.status !== "machine" ? `<span class="tag ${esc(pair.status.split(" ")[0])}">${esc(pair.status)}</span>` : "";
    h += `<td class="where">${esc(pair.rule())}${status}${pair.details ? ` <span class="det">${esc(pair.details)}</span>` : ""}${pair.edited ? ` <span class="det">edited ${esc(pair.edited)}</span>` : ""}</td>`;
    if (editing) {
      h += `<td class="tools"><label><input class="exclude" type="checkbox"${excluded ? " checked" : ""}> exclude</label>` +
           `<input class="note" type="text" placeholder="note" aria-label="Note" value="${esc(pair.note)}"></td>`;
    } else if (pair.note) {
      h += `<td class="where">${esc(pair.note)}</td>`;
    }
    return h + "</tr>";
  }
  function box(hit) {
    const { set, rows } = hit;
    const first = rows[0];
    const segs = first.ranks.map((i) => first.cells[i]);
    const everyone = segs.map((_, k) => rows.every((p) => p.cells[p.ranks[k]] && p.cells[p.ranks[k]].morph));
    const slots = segs.map(() => 0).concat([0]);
    rows.forEach((p) => {
      layout(p).added.forEach((list, k) => { if (k < slots.length) slots[k] = Math.max(slots[k], list.length); });
      p.slots = slots;
    });
    let h = `<section class="set"><h3>${esc(set.head || segs.map((s) => s.top).join(""))}`;
    if (set.gloss && set.gloss !== set.head) h += ` <span class="gl">‘${esc(set.gloss)}’</span>`;
    h += ` <span class="id">${esc(set.id)}, ${rows.length} of ${set.rows.length} ${set.rows.length === 1 ? "form" : "forms"}</span></h3>`;
    h += '<table><thead><tr class="anc"><th class="lang" scope="col">Language</th><th class="form" scope="col">As cited</th>';
    slots.forEach((width, k) => {
      h += `<th class="add" scope="col">${GAP}</th>`.repeat(width);
      if (k < segs.length) h += `<th scope="col"${everyone[k] ? ' class="m"' : ""}>${esc(segs[k].top)}</th>`;
    });
    h += '<th class="where" scope="col">Morphology, status, source</th>' + (editing ? '<th class="where" scope="col">Edit</th>' : "") + "</tr></thead><tbody>";
    rows.forEach((p) => { h += rowHTML(p, slots); });
    return h + "</tbody></table></section>";
  }
  function draw() {
    // the largest sets run to 200 forms, so a page is counted in forms as well as sets
    let html = "", forms = 0, i = shown;
    for (; i < hits.length && i < shown + PAGE && forms < ROWS; i++) {
      html += box(hits[i]);
      forms += hits[i].rows.length;
    }
    $("ia-sets").insertAdjacentHTML("beforeend", html);
    shown = i;
    $("ia-more").hidden = shown >= hits.length;
  }
  function apply() {
    if (!current) return;
    const q = fold($("ia-q").value.trim()), lang = $("ia-lang").value, status = $("ia-status").value, morph = $("ia-morph").value;
    hits = [];
    let forms = 0;
    SETS.forEach((set) => {
      const whole = q && set.f.includes(q);
      const rows = set.rows.filter((p) => {
        if (lang && p.base_language !== lang) return false;
        if (status === "edited" && !p.edited) return false;
        if (status && status !== "edited" && !p.status.startsWith(status)) return false;
        const marked = p.cells.some((c) => c.morph);
        if (morph === "yes" && !marked) return false;
        if (morph === "no" && marked) return false;
        if (morph && !["yes", "no"].includes(morph) && !p.morph.includes(morph)) return false;
        return !q || whole || p.f.includes(q);
      });
      if (rows.length) { hits.push({ set, rows }); forms += rows.length; }
    });
    const how = $("ia-sort").value;
    hits.sort((x, y) => how === "a" ? (x.set.f < y.set.f ? -1 : x.set.f > y.set.f ? 1 : 0) : (y.rows.length - x.rows.length));
    $("ia-count").textContent = `${num(forms)} forms in ${num(hits.length)} sets`;
    $("ia-sets").innerHTML = hits.length ? "" : '<div class="empty">No forms match these filters.</div>';
    shown = 0;
    draw();
  }

  // ------------------------------------------------------------- loading
  async function open(slug) {
    const entry = INDEX.find((s) => s.slug === slug) || INDEX[0];
    current = entry;
    $("ia-sheet").value = entry.slug;
    try { history.replaceState(null, "", `#${entry.slug}`); } catch (e) { /* no matter */ }
    $("ia-sets").innerHTML = `<div class="empty">Loading ${num(entry.rows)} rows…</div>`;
    $("ia-more").hidden = true;
    const r = await fetch(`${BASE}${entry.slug}.json`);
    if (!r.ok) throw new Error(`${r.status} ${r.statusText}`);
    const data = await r.json();
    const byId = new Map();
    pairs.clear();
    const languages = {}, rules = {};
    data.rows.forEach((row, n) => {
      const key = row[0], cut = key.lastIndexOf("|");
      const id = key.slice(0, cut);
      let set = byId.get(id);
      if (!set) { set = { id, head: row[3], gloss: row[1], rows: [] }; byId.set(id, set); }
      const language = row[2].split("#")[0];
      const pair = { key, n, language: row[2], base_language: language, form: row[4], base: [row[5], row[6], row[7], row[8], row[10]],
                     details: row[9], f: fold(`${row[2]} ${row[4]}`) };
      pair.rule = function () { return this.morph; };
      overlay(pair);
      languages[language] = (languages[language] || 0) + 1;
      row[7].split("; ").forEach((x) => { if (x) rules[x] = (rules[x] || 0) + 1; });
      set.rows.push(pair);
      pairs.set(key, pair);
    });
    SETS = [...byId.values()];
    SETS.forEach((s) => { s.f = fold(`${s.head} ${s.gloss} ${s.id}`); });
    $("ia-lang").innerHTML = '<option value="">all languages</option>' + Object.keys(languages).sort()
      .map((k) => `<option value="${esc(k)}">${esc(k)} (${num(languages[k])})</option>`).join("");
    $("ia-morph").innerHTML = '<option value="">all forms</option><option value="yes">with morphology marked</option><option value="no">without</option>' +
      Object.keys(rules).sort((a, b) => rules[b] - rules[a]).slice(0, 30).map((k) => `<option value="${esc(k)}">${esc(k)} (${num(rules[k])})</option>`).join("");
    showLink();
    $("ia-about").textContent = `${num(entry.rows)} pairs in ${num(entry.sets)} sets, ${num(entry.languages)} language labels; ` +
      `${num(entry.marked)} pairs have morphology marked, ${num(entry.reviewed)} were reviewed.`;
    apply();
  }
  function showLink() {
    const link = current && links[current.slug];
    $("ia-open").hidden = !link;
    if (link) $("ia-open").href = link;
  }
  async function readChanges() {
    if (!ENDPOINT) return;
    try {
      const r = await fetch(ENDPOINT);
      const data = await r.json();
      changes = {};
      (data.changes || []).forEach((c) => { changes[`${c.sheet}\t${c.key}`] = c; });
    } catch (e) {
      say("Could not read the saved edits from Google Sheets. Reload to try again.", true);
    }
    // edits that have not gone out yet are newer than anything the sheet holds
    queue.forEach((item) => {
      if (item.clear) delete changes[`${item.sheet}\t${item.key}`];
      else changes[`${item.sheet}\t${item.key}`] = { ...item, editor: identity.editor, saved: "not yet saved" };
    });
  }
  async function start() {
    recall();
    const r = await fetch(`${BASE}index.json`);
    if (!r.ok) throw new Error(`${r.status} ${r.statusText}`);
    INDEX = await r.json();
    let group = null, html = "";
    INDEX.forEach((s) => {
      if (s.book !== group) { html += `${group ? "</optgroup>" : ""}<optgroup label="${esc(s.book)}">`; group = s.book; }
      html += `<option value="${esc(s.slug)}">${esc(s.sheet)} (${num(s.rows)})</option>`;
    });
    $("ia-sheet").innerHTML = html + "</optgroup>";
    $("ia-edit").disabled = !ENDPOINT;
    if (!ENDPOINT) $("ia-edit").title = "Saving to Google Sheets is not switched on yet";
    await readChanges();
    await open(decodeURIComponent(location.hash.slice(1)));
    if (queue.length) flush();
  }

  // ------------------------------------------------------------- events
  function setEditing(on) {
    editing = on;
    brush = false;
    $("ia-edit").setAttribute("aria-pressed", on ? "true" : "false");
    $("ia-editbar").hidden = !on;
    $("ia-brush").setAttribute("aria-pressed", "false");
    apply();
  }
  $("ia-sheet").addEventListener("change", (e) => {
    open(e.target.value).catch((err) => { $("ia-sets").innerHTML = `<div class="empty">The sheet could not be loaded (${esc(err.message)}).</div>`; });
  });
  $("ia-q").addEventListener("input", () => { clearTimeout(wait); wait = setTimeout(apply, 180); });
  ["ia-lang", "ia-status", "ia-morph", "ia-sort"].forEach((id) => $(id).addEventListener("change", apply));
  $("ia-more").addEventListener("click", draw);
  $("ia-edit").addEventListener("click", () => {
    if (editing) { setEditing(false); return; }
    $("ia-editor").value = identity.editor;
    $("ia-pass").value = identity.pass;
    $("ia-login").hidden = false;
    $("ia-editor").focus();
  });
  $("ia-login").addEventListener("submit", async (e) => {
    e.preventDefault();
    const editor = $("ia-editor").value.trim(), pass = $("ia-pass").value;
    $("ia-loginmsg").textContent = "Checking…";
    try {
      const res = await post({ action: "check", pass });
      if (!res.ok) { $("ia-loginmsg").textContent = "That passphrase was not accepted."; return; }
      identity = { editor, pass };
      links = res.links || {};
      showLink();
      remember();
      $("ia-loginmsg").textContent = "";
      $("ia-login").hidden = true;
      setEditing(true);
      flush();
    } catch (err) {
      $("ia-loginmsg").textContent = `Google Sheets did not answer (${err.message}).`;
    }
  });
  $("ia-brush").addEventListener("click", () => {
    brush = !brush;
    $("ia-brush").setAttribute("aria-pressed", brush ? "true" : "false");
    apply();
  });
  $("ia-sets").addEventListener("click", (e) => {
    const button = e.target.closest("button.mk");
    if (!button) return;
    const pair = pairs.get(button.closest("tr").dataset.key);
    const cell = pair.cells[+button.dataset.i];
    cell.morph = !cell.morph;
    const was = parsePair(pair.base[0], pair.base[1]);
    const unchanged = pair.cells.every((c, i) => was[i].morph === c.morph);
    pair.morph = unchanged ? pair.base[2] : pair.cells.some((c) => c.morph) ? `${pair.base[2] ? pair.base[2] + "; " : ""}edited by hand` : "";
    save(pair);
    const tr = button.closest("tr");
    tr.outerHTML = rowHTML(pair, pair.slots);
  });
  $("ia-sets").addEventListener("change", (e) => {
    const tr = e.target.closest("tr[data-key]");
    if (!tr) return;
    const pair = pairs.get(tr.dataset.key);
    if (e.target.classList.contains("cell")) {
      pair.cells[+e.target.dataset.i].bottom = clean(e.target.value);
      const td = e.target.closest("td");
      const cell = pair.cells[+e.target.dataset.i];
      const was = parsePair(pair.base[0], pair.base[1])[+e.target.dataset.i];
      td.classList.toggle("lost", cell.bottom === GAP);
      td.classList.toggle("c", cell.bottom !== GAP && cell.bottom !== cell.top);
      td.classList.toggle("fix", was.bottom !== cell.bottom || was.morph !== cell.morph);
    } else if (e.target.classList.contains("exclude")) {
      pair.status = e.target.checked ? "excluded" : (/excluded/.test(pair.base[3]) ? "machine" : pair.base[3]);
      tr.classList.toggle("ex", e.target.checked);
    } else if (e.target.classList.contains("note")) {
      pair.note = e.target.value.trim();
    } else return;
    save(pair);
  });
  $("ia-sets").addEventListener("keydown", (e) => {
    if (e.key !== "Enter" || !e.target.classList.contains("cell")) return;
    e.preventDefault();
    const all = [...$("ia-sets").querySelectorAll("input.cell")];
    const next = all[all.indexOf(e.target) + 1];
    e.target.blur();
    if (next) next.focus();
  });
  window.addEventListener("online", flush);
  start().catch((e) => {
    $("ia-sets").innerHTML = `<div class="empty">The alignments could not be loaded (${esc(e.message)}). Reload the page to try again.</div>`;
  });
})();
