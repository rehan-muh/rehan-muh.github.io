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
  const GAP = "-", ADDED = "·", CHECK = "|#checked:", PROBLEM = "|#problem";

  function parseCells(text) {
    const out = [];
    let inside = false;
    String(text).split(" ").forEach((token) => {
      if (!token) return;
      const opens = token[0] === "[", shuts = token[token.length - 1] === "]";
      let bare = token.replace(/^\[/, "").replace(/\]$/, "");
      const seam = bare.length > 1 && bare[0] === "+";
      if (seam) bare = bare.slice(1);
      out.push({ seg: bare, morph: inside || opens, seam });
      inside = (inside || opens) && !shuts;
    });
    return out;
  }
  function renderCells(cells) {
    return cells.map((c, n) => {
      const opens = c.morph && (n === 0 || !cells[n - 1].morph);
      const shuts = c.morph && (n === cells.length - 1 || !cells[n + 1].morph);
      return `${opens ? "[" : ""}${c.seam ? "+" : ""}${c.seg}${shuts ? "]" : ""}`;
    }).join(" ");
  }
  // [{top, bottom, morph}] for a pair, from its two strings
  function parsePair(anc, ref) {
    const top = parseCells(anc), bottom = parseCells(ref);
    return top.map((c, i) => ({ top: c.seg, bottom: bottom[i] ? bottom[i].seg : GAP, morph: c.morph, seam: c.seam }));
  }
  function renderPair(cells) {
    return [renderCells(cells.map((c) => ({ seg: c.top, morph: c.morph, seam: c.seam }))),
            renderCells(cells.map((c) => ({ seg: c.bottom, morph: c.morph })))];
  }
  const clean = (v) => {
    const s = String(v).normalize("NFC").trim();
    return s === "" || s === "–" || s === "—" ? GAP : s.replace(/\s+/g, "");
  };
  // indices of the cells that hold an ancestor segment
  const rankList = (cells) => cells.reduce((a, c, i) => (c.top !== ADDED ? a.concat(i) : a), []);
  // key G: a gap before the reflex segment in cell i. That segment and the ones after it move
  // one ancestor segment to the right, up to the first gap, which they fill; past the end of
  // the word the last one goes to a column of its own. Returns a message if nothing moved.
  function insertGap(cells, i) {
    const ranks = rankList(cells), k = ranks.indexOf(i);
    if (k < 0) return "A gap goes in one of the ancestor's columns.";
    if (cells[i].bottom === GAP) return "This is a gap already.";
    let carry = cells[i].bottom;
    cells[i].bottom = GAP;
    for (let j = k + 1; j < ranks.length; j++) {
      const next = cells[ranks[j]].bottom;
      cells[ranks[j]].bottom = carry;
      if (next === GAP) return "";
      carry = next;
    }
    cells.splice(ranks[ranks.length - 1] + 1, 0, { top: ADDED, bottom: carry, morph: false, seam: false });
    return "";
  }
  // key D: the gap in cell i taken out. The segments after it move one to the left, up to the
  // next gap; at the end of the word, a segment in a column of its own just after the word
  // (not a suffix) comes back in.
  function removeGap(cells, i) {
    const ranks = rankList(cells), k = ranks.indexOf(i);
    if (k < 0 || cells[i].bottom !== GAP) return "Only a gap in one of the ancestor's columns can be taken out.";
    let j = k;
    while (j + 1 < ranks.length && cells[ranks[j + 1]].bottom !== GAP) {
      cells[ranks[j]].bottom = cells[ranks[j + 1]].bottom;
      j++;
    }
    // cell j's segment has moved to j - 1; j takes the spilled segment, or becomes the gap
    const after = ranks[ranks.length - 1] + 1;
    const spill = j === ranks.length - 1 && cells[after] && cells[after].top === ADDED && !cells[after].morph;
    if (j === k && !spill) return "There is nothing after this gap to move in.";
    if (spill) {
      cells[ranks[j]].bottom = cells[after].bottom;
      cells.splice(after, 1);
    } else {
      cells[ranks[j]].bottom = GAP;
    }
    return "";
  }

  if (typeof document === "undefined") {
    module.exports = { parseCells, renderCells, parsePair, renderPair, clean, insertGap, removeGap };
    return;
  }

  const BASE = document.currentScript.src.replace(/viewer\.js.*$/, "data/");
  const ENDPOINT = (window.INDIC_CONFIG && window.INDIC_CONFIG.endpoint) || "";
  const STORE = "indic-alignments-v1";
  const PAGE = 25, ROWS = 300;
  const KEEPS_CHECKED = ["edited", "uncertain", "learned"];
  const LEARNED = "probable learned form";
  const $ = (id) => document.getElementById(id);
  const esc = (s) => String(s).replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[c]);
  const fold = (s) => String(s).normalize("NFD").replace(/[̀-ͯ]/g, "").replace(/ʰ/g, "h").toLowerCase();
  const num = (n) => Number(n).toLocaleString("en-US");

  let INDEX = [], current = null, SETS = [], hits = [], shown = 0, wait = null;
  let editing = false, brush = false, gapping = false, changes = {}, links = {};
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
      // the item sent is taken off the queue by identity: an edit to the same pair made while
      // it was on its way has replaced it in the queue, and must still be sent after it
      const item = queue[0];
      try {
        const res = await post({ ...item, action: "save", pass: identity.pass, editor: identity.editor });
        if (!res.ok) throw new Error(res.error || "refused");
        queue = queue.filter((q) => q !== item);
        const key = `${item.sheet}\t${item.key}`;
        const newer = queue.some((q) => q.sheet === item.sheet && q.key === item.key);
        if (!newer && item.clear) delete changes[key];
        else if (!newer) changes[key] = { ...item, saved: res.saved, editor: identity.editor };
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

  // a check is one row on the Changes tab, keyed by the set and the person
  function checksOf(id) {
    const head = `${current.slug}\t${id}${CHECK}`;
    return Object.keys(changes).filter((k) => k.startsWith(head)).map((k) => k.slice(head.length));
  }
  function setChecks(set) {
    set.checks = checksOf(set.id);
    set.problem = changes[`${current.slug}\t${set.id}${PROBLEM}`] || null;
  }
  function checkLabel(set) {
    return set.checks.length ? `<span class="ok">✓ checked by ${esc(set.checks.join(", "))}</span>` : "";
  }
  // the menu of sheets, smallest first, each with how far the checking has got
  function fillSheets() {
    $("ia-sheet").innerHTML = INDEX.map((s) => {
      const head = `${s.slug}\t`;
      const done = new Set(Object.keys(changes).filter((k) => k.startsWith(head) && k.includes(CHECK))
        .map((k) => k.slice(head.length).split(CHECK)[0])).size;
      return `<option value="${esc(s.slug)}">${esc(s.sheet)} (${num(done)} of ${num(s.sets)} sets checked)</option>`;
    }).join("");
    if (current) $("ia-sheet").value = current.slug;
  }
  function showChecked() {
    const done = SETS.filter((s) => s.checks.length).length;
    const aside = SETS.filter((s) => s.problem).length;
    let uncertain = 0;
    SETS.forEach((s) => { uncertain += s.rows.filter((p) => p.status === "uncertain").length; });
    $("ia-done").textContent = `${num(done)} of ${num(SETS.length)} sets checked · ${num(aside)} ${aside === 1 ? "set" : "sets"} set aside · ${num(uncertain)} ${uncertain === 1 ? "row" : "rows"} uncertain`;
  }
  function toggleCheck(set, on) {
    const name = identity.editor.replace(/\|/g, " ");
    const key = `${set.id}${CHECK}${name}`;
    const item = { sheet: current.slug, key, row: 0, anc: "", ref: "", morph: "", status: "checked", note: "" };
    if (on) changes[`${current.slug}\t${key}`] = { ...item, editor: identity.editor, saved: new Date().toISOString() };
    else { item.clear = true; delete changes[`${current.slug}\t${key}`]; }
    setChecks(set);
    queue = queue.filter((q) => !(q.sheet === item.sheet && q.key === item.key));
    queue.push(item);
    remember();
    flush();
  }

  // take a table off the page without redrawing, so the reader keeps their place
  function leave(set, section) {
    const at = hits.findIndex((h) => h.set === set);
    if (at >= 0) {
      hits.splice(at, 1);
      if (at < shown) shown--;
    }
    section.remove();
    const forms = hits.reduce((n, h) => n + h.rows.length, 0);
    $("ia-count").textContent = `${num(forms)} forms in ${num(hits.length)} sets`;
    if (!$("ia-sets").querySelector("section.set") && shown < hits.length) draw();
  }

  function toggleProblem(set, on) {
    const key = `${set.id}${PROBLEM}`;
    const item = { sheet: current.slug, key, row: 0, anc: "", ref: "", morph: "", status: "problem set", note: "" };
    if (on) changes[`${current.slug}\t${key}`] = { ...item, editor: identity.editor, saved: new Date().toISOString() };
    else { item.clear = true; delete changes[`${current.slug}\t${key}`]; }
    setChecks(set);
    queue = queue.filter((q) => !(q.sheet === item.sheet && q.key === item.key));
    queue.push(item);
    remember();
    flush();
  }

  function overlay(pair) {
    const c = changes[`${current.slug}\t${pair.key}`];
    pair.cells = parsePair(c ? c.anc : pair.base[0], c ? c.ref : pair.base[1]);
    pair.morph = c ? c.morph : pair.base[2];
    pair.status = c ? c.status : pair.base[3];
    pair.note = c ? c.note : pair.base[4];
    pair.edited = c ? `${c.editor || ""} ${String(c.saved || "").slice(0, 16)}`.trim() : "";
    pair.baseRanks = parsePair(pair.base[0], pair.base[1]).filter((c) => c.top !== ADDED);
    setRanks(pair);
  }
  function setRanks(pair) {
    pair.ranks = [];
    pair.cells.forEach((cell, i) => { if (cell.top !== ADDED) pair.ranks.push(i); });
  }
  // a segment with no counterpart goes to a gap column of its own, beside the column it was in
  function toGap(pair, i, side) {
    const cell = pair.cells[i];
    pair.cells.splice(side === "left" ? i : i + 1, 0, { top: ADDED, bottom: cell.bottom, morph: false, seam: false });
    cell.bottom = GAP;
    setRanks(pair);
  }
  // back into the next empty column on either side, if there is one
  function putBack(pair, i) {
    const near = (step) => {
      let j = i + step;
      while (j >= 0 && j < pair.cells.length && pair.cells[j].top === ADDED) j += step;
      return j >= 0 && j < pair.cells.length && pair.cells[j].bottom === GAP ? j : -1;
    };
    const j = near(1) >= 0 ? near(1) : near(-1);
    if (j < 0) return false;
    pair.cells[j].bottom = pair.cells[i].bottom;
    pair.cells.splice(i, 1);
    setRanks(pair);
    return true;
  }
  function redraw(pair, inside) {
    const hit = hits.find((h) => h.set === pair.set);
    if (hit) inside.closest("section.set").outerHTML = box(hit);
  }
  // the cell to type in once a table has been drawn again
  function focusCell(pair, i) {
    const tr = [...$("ia-sets").querySelectorAll("tr[data-key]")].find((t) => t.dataset.key === pair.key);
    if (!tr) return;
    const inputs = [...tr.querySelectorAll("input.cell")];
    const input = inputs.find((x) => +x.dataset.i === i) ||
                  inputs.filter((x) => +x.dataset.i < i).pop() || inputs[0];
    if (input) { input.focus(); input.select(); }
  }
  // every change to a pair can be taken back with U, as far as the state the page loaded
  function snapshot(pair) {
    pair.history = pair.history || [];
    pair.history.push(JSON.stringify({ cells: pair.cells, morph: pair.morph, status: pair.status, note: pair.note }));
    if (pair.history.length > 100) pair.history.shift();
  }
  function undo(pair) {
    if (!pair.history || !pair.history.length) return false;
    Object.assign(pair, JSON.parse(pair.history.pop()));
    setRanks(pair);
    return true;
  }
  function toggleMorph(pair, i) {
    const cell = pair.cells[i];
    cell.morph = !cell.morph;
    const was = parsePair(pair.base[0], pair.base[1]);
    const unchanged = was.length === pair.cells.length && pair.cells.every((c, k) => was[k].morph === c.morph);
    pair.morph = unchanged ? pair.base[2] : pair.cells.some((c) => c.morph) ? `${pair.base[2] ? pair.base[2] + "; " : ""}edited by hand` : "";
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
    const was = pair.baseRanks[pair.ranks.indexOf(i)];
    const fixed = !was || was.bottom !== cell.bottom || was.morph !== cell.morph;
    const cls = ["seg", lost ? "lost" : cell.bottom !== cell.top ? "c" : "", cell.morph ? "m" : "", cell.seam ? "seam" : "", fixed ? "fix" : ""].join(" ");
    const tip = fixed && was ? ` title="first alignment: ${esc(was.bottom)}${was.morph ? " (morphology)" : ""}"` : "";
    let body = esc(lost ? GAP : cell.bottom);
    if (editing && !brush) body = `<input class="cell" data-i="${i}" value="${esc(lost ? "" : cell.bottom)}" aria-label="Reflex of ${esc(cell.top)}">`;
    if (editing && brush) body = `<button class="mk" type="button" data-i="${i}" aria-pressed="${cell.morph}">${esc(lost ? GAP : cell.bottom)}</button>`;
    if (editing && gapping && !lost) {
      body = `<button class="gp" type="button" data-i="${i}" data-side="left" aria-label="Move ${esc(cell.bottom)} to a gap column on the left">←</button>${esc(cell.bottom)}` +
             `<button class="gp" type="button" data-i="${i}" data-side="right" aria-label="Move ${esc(cell.bottom)} to a gap column on the right">→</button>`;
    } else if (editing && gapping) {
      body = esc(GAP);
    }
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
  function addedCell(pair, i, green) {
    const kind = green ? "add" : "add plain";
    if (i === undefined) return `<td class="seg ${kind} none">${GAP}</td>`;
    const seg = esc(pair.cells[i].bottom);
    if (editing && gapping) return `<td class="seg ${kind}"><button class="gb" type="button" data-i="${i}" title="Put back in the empty column beside it">${seg}</button></td>`;
    if (editing && brush) return `<td class="seg ${kind}"><button class="mk" type="button" data-i="${i}" aria-pressed="${pair.cells[i].morph}">${seg}</button></td>`;
    // a suffix or an unplaced segment can be changed too; emptied, it is taken out
    if (editing) return `<td class="seg ${kind}"><input class="cell" data-i="${i}" value="${seg}" aria-label="Segment the ancestor lacks"></td>`;
    return `<td class="seg ${kind}">${seg}</td>`;
  }
  // `slots[p]` is how many columns the added segments before p need in this table
  function rowHTML(pair, slots, greens) {
    const excluded = /excluded/.test(pair.status);
    let h = `<tr class="${excluded ? "ex" : ""}" data-key="${esc(pair.key)}">`;
    h += `<td class="lang">${esc(pair.language)}</td><td class="form">${esc(pair.form)}</td>`;
    const { added, rank } = layout(pair);
    slots.forEach((width, p) => {
      for (let k = 0; k < width; k++) h += addedCell(pair, added[p] ? added[p][k] : undefined, greens[p][k]);
      if (p < rank.length) h += segCell(pair, rank[p]);
      else if (p < slots.length - 1) h += "<td></td>";
    });
    const status = pair.status && pair.status !== "machine" ? `<span class="tag ${esc(pair.status.split(" ")[0])}">${esc(pair.status)}</span>` : "";
    h += `<td class="where">${esc(pair.rule())}${status}${pair.details ? ` <span class="det">${esc(pair.details)}</span>` : ""}${pair.edited ? ` <span class="det">edited ${esc(pair.edited)}</span>` : ""}</td>`;
    if (editing) {
      h += `<td class="tools"><label><input class="exclude" type="checkbox"${excluded ? " checked" : ""}> exclude</label> ` +
           `<label><input class="uncertain" type="checkbox"${pair.status === "uncertain" ? " checked" : ""}> uncertain</label>` +
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
    // a column is green if it holds a suffix in any row, and grey if it holds only segments
    // the aligner could not place
    const greens = slots.map((width) => Array(width).fill(false));
    rows.forEach((p) => {
      p.greens = greens;
      layout(p).added.forEach((list, k) => list.forEach((i, n) => {
        if (k < greens.length && p.cells[i].morph) greens[k][n] = true;
      }));
    });
    let h = `<section class="set"><h3>${esc(set.head || segs.map((s) => s.top).join(""))}`;
    if (set.gloss && set.gloss !== set.head) h += ` <span class="gl">‘${esc(set.gloss)}’</span>`;
    const aside = set.rows.filter((p) => p.status === "uncertain").length;
    h += ` <span class="id">${esc(set.id)}, ${rows.length} of ${set.rows.length} ${set.rows.length === 1 ? "form" : "forms"}${aside ? `, ${aside} uncertain` : ""}</span>`;
    if (editing) {
      const mine = set.checks.includes(identity.editor.replace(/\|/g, " "));
      h += ` <label class="chk"><input class="setcheck" type="checkbox" data-id="${esc(set.id)}"${mine ? " checked" : ""}> I checked this set</label>`;
      h += ` <label class="chk2"><input class="setproblem" type="checkbox" data-id="${esc(set.id)}"${set.problem ? " checked" : ""}> problem set, set aside</label>`;
    }
    if (set.learned) h += '<span class="tag learned">probable learned form</span>';
    h += `<span class="oks">${checkLabel(set)}</span>`;
    if (set.problem) h += `<span class="tag excluded">set aside${set.problem.editor ? ` by ${esc(set.problem.editor)}` : ""}</span>`;
    h += "</h3>";
    h += '<table><thead><tr class="anc"><th class="lang" scope="col">Language</th><th class="form" scope="col">As cited</th>';
    slots.forEach((width, k) => {
      for (let n = 0; n < width; n++) h += `<th class="add${greens[k][n] ? "" : " plain"}" scope="col">${GAP}</th>`;
      if (k < segs.length) {
        const cls = [everyone[k] ? "m" : "", segs[k].seam ? "seam" : ""].join(" ").trim();
        h += `<th scope="col"${cls ? ` class="${cls}"` : ""}>${esc(segs[k].top)}</th>`;
      }
    });
    h += '<th class="where" scope="col">Morphology, status, source</th>' + (editing ? '<th class="where" scope="col">Edit</th>' : "") + "</tr></thead><tbody>";
    rows.forEach((p) => { h += rowHTML(p, slots, greens); });
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
      // a problem set leaves every view but its own
      if (status === "problem" ? !set.problem : set.problem) return;
      // a checked set is done, so it leaves the page too, except where a view asks for it
      if (status === "checked" ? !set.checks.length : set.checks.length && !KEEPS_CHECKED.includes(status)) return;
      if (status === "learned" && !set.learned) return;
      const whole = q && set.f.includes(q);
      const rows = set.rows.filter((p) => {
        if (lang && p.base_language !== lang) return false;
        // an uncertain row leaves its table and is seen only in the views that ask for it
        if (p.status === "uncertain" && status !== "uncertain" && status !== "edited") return false;
        if (status === "edited" && !p.edited) return false;
        if (status && !["edited", "checked", "problem", "learned"].includes(status) && !p.status.startsWith(status)) return false;
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
      if (row[9].includes(LEARNED)) set.learned = true;
      const details = row[9].replace(new RegExp(`(^|; )${LEARNED}(?=;|$)`), "");
      const pair = { key, n, set, language: row[2], base_language: language, form: row[4], base: [row[5], row[6], row[7], row[8], row[10]],
                     details, f: fold(`${row[2]} ${row[4]}`) };
      pair.rule = function () { return this.morph; };
      overlay(pair);
      languages[language] = (languages[language] || 0) + 1;
      row[7].split("; ").forEach((x) => { if (x) rules[x] = (rules[x] || 0) + 1; });
      set.rows.push(pair);
      pairs.set(key, pair);
    });
    SETS = [...byId.values()];
    SETS.forEach((s) => { s.f = fold(`${s.head} ${s.gloss} ${s.id}`); setChecks(s); });
    showChecked();
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
    INDEX = (await r.json()).sort((x, y) => x.sets - y.sets);
    $("ia-edit").disabled = !ENDPOINT;
    if (!ENDPOINT) $("ia-edit").title = "Saving to Google Sheets is not switched on yet";
    await readChanges();
    fillSheets();
    await open(decodeURIComponent(location.hash.slice(1)));
    if (queue.length) flush();
  }

  // ------------------------------------------------------------- events
  function setEditing(on) {
    editing = on;
    brush = false;
    gapping = false;
    $("ia-gap").setAttribute("aria-pressed", "false");
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
  function setTool(useBrush, useGap) {
    brush = useBrush;
    gapping = useGap;
    $("ia-brush").setAttribute("aria-pressed", brush ? "true" : "false");
    $("ia-gap").setAttribute("aria-pressed", gapping ? "true" : "false");
    apply();
  }
  $("ia-brush").addEventListener("click", () => setTool(!brush, false));
  $("ia-gap").addEventListener("click", () => setTool(false, !gapping));
  $("ia-sets").addEventListener("click", (e) => {
    const button = e.target.closest("button.mk, button.gp, button.gb");
    if (!button) return;
    const pair = pairs.get(button.closest("tr").dataset.key);
    const i = +button.dataset.i;
    snapshot(pair);
    if (button.classList.contains("gp")) {
      toGap(pair, i, button.dataset.side);
    } else if (button.classList.contains("gb")) {
      if (!putBack(pair, i)) { pair.history.pop(); say("There is no empty column beside it. Empty one first.", true); return; }
    } else {
      toggleMorph(pair, i);
    }
    save(pair);
    redraw(pair, button);
  });
  $("ia-sets").addEventListener("change", (e) => {
    if (e.target.classList.contains("setcheck")) {
      const set = SETS.find((s) => s.id === e.target.dataset.id);
      toggleCheck(set, e.target.checked);
      e.target.closest("h3").querySelector(".oks").innerHTML = checkLabel(set);
      showChecked();
      fillSheets();
      const view = $("ia-status").value;
      const gone = view === "checked" ? !set.checks.length : set.checks.length && !KEEPS_CHECKED.includes(view);
      if (gone) leave(set, e.target.closest("section.set"));
      return;
    }
    if (e.target.classList.contains("setproblem")) {
      const set = SETS.find((s) => s.id === e.target.dataset.id);
      toggleProblem(set, e.target.checked);
      showChecked();
      leave(set, e.target.closest("section.set"));
      return;
    }
    const tr = e.target.closest("tr[data-key]");
    if (!tr) return;
    const pair = pairs.get(tr.dataset.key);
    if (e.target.classList.contains("cell")) {
      const i = +e.target.dataset.i, cell = pair.cells[i], value = clean(e.target.value);
      if (!cell || value === cell.bottom) return;
      snapshot(pair);
      if (cell.top === ADDED) {
        // a suffix or unplaced segment emptied is taken out of the row
        if (value === GAP) {
          pair.cells.splice(i, 1);
          setRanks(pair);
          save(pair);
          redraw(pair, e.target);
          return;
        }
        cell.bottom = value;
        save(pair);
        return;
      }
      cell.bottom = value;
      const td = e.target.closest("td");
      const was = pair.baseRanks[pair.ranks.indexOf(i)];
      td.classList.toggle("lost", cell.bottom === GAP);
      td.classList.toggle("c", cell.bottom !== GAP && cell.bottom !== cell.top);
      td.classList.toggle("fix", !was || was.bottom !== cell.bottom || was.morph !== cell.morph);
    } else if (e.target.classList.contains("exclude")) {
      snapshot(pair);
      pair.status = e.target.checked ? "excluded" : (/excluded/.test(pair.base[3]) ? "machine" : pair.base[3]);
      tr.classList.toggle("ex", e.target.checked);
    } else if (e.target.classList.contains("uncertain")) {
      snapshot(pair);
      pair.status = e.target.checked ? "uncertain" : (pair.base[3] === "uncertain" ? "machine" : pair.base[3]);
      save(pair);
      showChecked();
      if ($("ia-status").value !== "edited") {
        const section = tr.closest("section.set");
        tr.remove();
        if (!section.querySelector("tbody tr")) section.remove();
      }
      return;
    } else if (e.target.classList.contains("note")) {
      snapshot(pair);
      pair.note = e.target.value.trim();
    } else return;
    save(pair);
  });
  // keys in a segment cell: Enter, the next cell. Capitals, which no transcription here uses:
  // M marks or unmarks morphology, G puts a gap before the segment, D takes a gap out, and
  // U takes back the last change to the row.
  const KEYS = { M: "morph", G: "gap", D: "ungap", U: "undo" };
  $("ia-sets").addEventListener("keydown", (e) => {
    const input = e.target;
    if (!input.classList || !input.classList.contains("cell")) return;
    const tr = input.closest("tr[data-key]");
    const pair = tr && pairs.get(tr.dataset.key);
    if (!pair) return;
    if (e.key === "Enter") {
      e.preventDefault();
      const all = [...$("ia-sets").querySelectorAll("input.cell")];
      const next = all[all.indexOf(input) + 1];
      const nextRow = next && next.closest("tr[data-key]"), nextI = next ? +next.dataset.i : 0;
      input.blur();
      if (next && next.isConnected) next.focus();
      // the row was drawn again (an emptied suffix was taken out): find the cell anew
      else if (nextRow) focusCell(pairs.get(nextRow.dataset.key), nextRow === tr ? nextI - 1 : nextI);
      return;
    }
    // a capital letter (Shift, or Caps Lock), whatever the keyboard layout reports for it
    const letter = e.key.length === 1 && e.key === e.key.toUpperCase() ? e.key
      : e.shiftKey && /^Key[MGDU]$/.test(e.code || "") ? e.code.slice(3) : "";
    const op = !e.ctrlKey && !e.metaKey && !e.altKey && KEYS[letter];
    if (!op) return;
    e.preventDefault();
    const i = +input.dataset.i;
    snapshot(pair);
    // what was typed in the cell counts first
    const typed = clean(input.value), cell = pair.cells[i];
    if (cell && typed !== cell.bottom && !(cell.top === ADDED && typed === GAP)) cell.bottom = typed;
    let target = i, msg = "";
    if (op === "morph") {
      toggleMorph(pair, i);
    } else if (op === "gap") {
      const ranks = pair.ranks.slice(), k = ranks.indexOf(i);
      msg = insertGap(pair.cells, i);
      // the segment moved one column on, and the cursor follows it, so G again moves it again
      if (!msg) target = k + 1 < ranks.length ? ranks[k + 1] : ranks[ranks.length - 1] + 1;
    } else if (op === "ungap") {
      msg = removeGap(pair.cells, i);
    } else {
      pair.history.pop();
      if (!undo(pair)) msg = "Nothing left to take back in this row.";
      target = Math.min(i, pair.cells.length - 1);
    }
    if (msg) {
      if (op !== "undo") undo(pair);
      say(msg, true);
      return;
    }
    setRanks(pair);
    save(pair);
    redraw(pair, input);
    focusCell(pair, target);
  });
  window.addEventListener("online", flush);
  start().catch((e) => {
    $("ia-sets").innerHTML = `<div class="empty">The alignments could not be loaded (${esc(e.message)}). Reload the page to try again.</div>`;
  });
})();
