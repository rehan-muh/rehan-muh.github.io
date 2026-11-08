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
  // the ancestor's k-th segment taken out of one row: a reflex segment that stood under it
  // stays, in a column of its own (the ancestor has a gap there); a gap goes with it
  function deleteAncestor(cells, k) {
    const ranks = rankList(cells);
    if (k < 0 || k >= ranks.length) return "That segment is not in this row.";
    if (ranks.length < 2) return "The ancestor would have no segment left.";
    const i = ranks[k], cell = cells[i];
    if (cell.seam && k + 1 < ranks.length) cells[ranks[k + 1]].seam = true;
    if (cell.bottom === GAP) cells.splice(i, 1);
    else cells[i] = { top: ADDED, bottom: cell.bottom, morph: false, seam: false };
    return "";
  }
  // a segment added to the ancestor of one row, before its segment `pos` (after the last one
  // when pos is the number of segments), with a gap under it: the reflex is typed in, or an
  // added segment beside it is moved in with the gap tool
  function insertAncestor(cells, pos, seg) {
    const ranks = rankList(cells);
    if (pos < 0 || pos > ranks.length) return "That place is not in this row.";
    const token = clean(seg);
    if (token === GAP || token === ADDED || /[[\]+]/.test(token)) return "Type the segment to add, in IPA.";
    const at = pos < ranks.length ? ranks[pos] : ranks[ranks.length - 1] + 1;
    cells.splice(at, 0, { top: token, bottom: GAP, morph: false, seam: false });
    return "";
  }
  // cells by slot: added[p] are the cells the reflex has where the ancestor has a gap, before
  // ancestor segment p (the last entry is after the word); rank[p] is that segment's cell
  function layoutOf(cells) {
    const added = [[]], rank = [];
    cells.forEach((cell, i) => {
      if (cell.top === ADDED) added[added.length - 1].push(i);
      else { rank.push(i); added.push([]); }
    });
    return { added, rank };
  }
  // where a new cell of slot p goes: after the slot's added cells
  const slotEnd = (cells, p) => { const r = rankList(cells); return p < r.length ? r[p] : cells.length; };
  // a reflex segment typed into column n of slot p, where this row has none: it goes after
  // the row's other added cells of that slot
  function insertReflex(cells, p, n, seg, morph) {
    const { added } = layoutOf(cells);
    if (p < 0 || p >= added.length) return "That place is not in this row.";
    if (n < added[p].length) return "This row has a segment there already; change it instead.";
    const token = clean(seg);
    if (token === GAP || token === ADDED || /[[\]+]/.test(token)) return "Type the segment, in IPA.";
    cells.splice(slotEnd(cells, p), 0, { top: ADDED, bottom: token, morph: !!morph, seam: false });
    return "";
  }
  // an ancestor segment put into gap column n of slot p: a reflex segment this row has in
  // that column stands under it; a row with none there gets a gap
  function ancestorIntoColumn(cells, p, n, seg) {
    const { added } = layoutOf(cells);
    if (p < 0 || p >= added.length) return "That place is not in this row.";
    const token = clean(seg);
    if (token === GAP || token === ADDED || /[[\]+]/.test(token)) return "Type the segment to add, in IPA.";
    if (n < added[p].length) {
      const i = added[p][n];
      cells[i] = { top: token, bottom: cells[i].bottom, morph: false, seam: false };
    } else {
      cells.splice(slotEnd(cells, p), 0, { top: token, bottom: GAP, morph: false, seam: false });
    }
    return "";
  }
  // a whole column deleted from one row: an ancestor segment with whatever stands under it
  // ("rank", p), or the row's added segment in column n of slot p ("added")
  function deleteColumn(cells, kind, p, n) {
    if (kind === "rank") {
      const ranks = rankList(cells);
      if (p < 0 || p >= ranks.length) return "That segment is not in this row.";
      if (ranks.length < 2) return "The ancestor would have no segment left.";
      if (cells[ranks[p]].seam && p + 1 < ranks.length) cells[ranks[p + 1]].seam = true;
      cells.splice(ranks[p], 1);
      return "";
    }
    const { added } = layoutOf(cells);
    if (p < 0 || p >= added.length) return "That place is not in this row.";
    if (n < added[p].length) cells.splice(added[p][n], 1);
    return "";
  }
  // the ancestor's k-th segment marked as morphology, or not, in one row
  function markAncestor(cells, k, on) {
    const ranks = rankList(cells);
    if (k < 0 || k >= ranks.length) return "That segment is not in this row.";
    cells[ranks[k]].morph = on;
    return "";
  }
  // the ancestor's k-th segment changed in one row; emptied, it is taken out (deleteAncestor)
  function renameAncestor(cells, k, seg) {
    const ranks = rankList(cells);
    if (k < 0 || k >= ranks.length) return "That segment is not in this row.";
    const token = clean(seg);
    if (token === GAP) return deleteAncestor(cells, k);
    if (token === ADDED || /[[\]+]/.test(token)) return "Type the segment in IPA.";
    cells[ranks[k]].top = token;
    return "";
  }
  // a reflex segment moved within its row, from cell i to a target: {kind: "rank", p}, the
  // gap under ancestor segment p, or {kind: "added", p, n}, column n of the gap columns before
  // ancestor segment p (the last p is after the word). In a gap column it goes before the
  // segment the row has in that column, or after the row's others in that place.
  function moveReflex(cells, i, target) {
    const src = cells[i];
    if (!src || src.bottom === GAP) return "There is no segment there to move.";
    const seg = src.bottom, wasAdded = src.top === ADDED;
    const take = () => { if (wasAdded) cells.splice(cells.indexOf(src), 1); else src.bottom = GAP; };
    if (target.kind === "rank") {
      const ranks = rankList(cells);
      if (target.p < 0 || target.p >= ranks.length) return "That place is not in this row.";
      const dest = cells[ranks[target.p]];
      if (dest === src) return "";
      if (dest.bottom !== GAP) return `${dest.top} has ${dest.bottom} under it already. Drop the segment on a gap, or add a column with +.`;
      take();
      dest.bottom = seg;
      return "";
    }
    const { added, rank } = layoutOf(cells);
    const { p, n } = target;
    if (p < 0 || p >= added.length) return "That place is not in this row.";
    const over = n < added[p].length ? cells[added[p][n]] : null;
    if (over === src) return "";
    // without a segment in that column, the new cell goes at the end of the place: before
    // ancestor segment p, or at the end of the word
    const before = over ? null : (p < rank.length ? cells[rank[p]] : null);
    take();
    const at = over ? cells.indexOf(over) : before ? cells.indexOf(before) : cells.length;
    cells.splice(at, 0, { top: ADDED, bottom: seg, morph: wasAdded ? src.morph : false, seam: false });
    return "";
  }
  // the ancestor's k-th segment moved into column n of the gap columns before ancestor segment
  // p (as the table was before the move): a reflex segment the row has in that column stands
  // under it, a row with none there gets a gap; the reflex segment that stood under it stays,
  // in a column of its own
  function moveAncestor(cells, k, p, n) {
    const ranks = rankList(cells);
    if (k < 0 || k >= ranks.length) return "That segment is not in this row.";
    const { added, rank } = layoutOf(cells);
    if (p < 0 || p >= added.length) return "That place is not in this row.";
    const at = ranks[k], src = cells[at];
    const over = n < added[p].length ? cells[added[p][n]] : null;
    const before = over ? null : (p < rank.length ? cells[rank[p]] : null);
    if (src.seam && k + 1 < ranks.length) cells[ranks[k + 1]].seam = true;
    if (src.bottom === GAP) cells.splice(at, 1);
    else cells[at] = { top: ADDED, bottom: src.bottom, morph: false, seam: false };
    if (over) {
      over.top = src.top;
      over.morph = src.morph;
      return "";
    }
    // into the place just before itself: the new cell goes where it was, before what stood under it
    const where = before === src ? at : before ? cells.indexOf(before) : cells.length;
    cells.splice(where, 0, { top: src.top, bottom: GAP, morph: src.morph, seam: false });
    return "";
  }

  if (typeof document === "undefined") {
    module.exports = { parseCells, renderCells, parsePair, renderPair, clean, rankList, deleteAncestor, markAncestor,
                       insertAncestor, layoutOf, insertReflex, ancestorIntoColumn, deleteColumn,
                       renameAncestor, moveReflex, moveAncestor };
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
  // U takes back the last change on the page, whether to one pair or to a whole set (an
  // ancestor segment deleted or marked), as far back as the state the page loaded
  const UNDO = [];
  function snapshot(list) {
    UNDO.push((Array.isArray(list) ? list : [list]).map((p) =>
      [p, JSON.stringify({ cells: p.cells, morph: p.morph, status: p.status, note: p.note })]));
    if (UNDO.length > 200) UNDO.shift();
  }
  function undoLast() {
    const entry = UNDO.pop();
    if (!entry) { say("Nothing left to take back.", true); return; }
    const sets = new Set();
    entry.forEach(([p, state]) => {
      Object.assign(p, JSON.parse(state));
      setRanks(p);
      save(p);
      sets.add(p.set);
    });
    sets.forEach(redrawSet);
  }
  function redrawSet(set) {
    const hit = hits.find((h) => h.set === set);
    const section = [...$("ia-sets").querySelectorAll("section.set")].find((s) => s.dataset.set === set.id);
    if (hit && section) section.outerHTML = box(hit);
  }
  // after the set's ancestor changed shape: the empty columns added with + are kept by place,
  // and the places have moved, so they go
  function reshaped(set) {
    delete set.want;
    redrawSet(set);
  }
  // the morphology label of a pair after its marks changed
  function relabel(pair) {
    const was = parsePair(pair.base[0], pair.base[1]);
    const unchanged = was.length === pair.cells.length && pair.cells.every((c, k) => was[k].morph === c.morph && was[k].top === c.top);
    pair.morph = unchanged ? pair.base[2] : pair.cells.some((c) => c.morph) ? `${pair.base[2] ? pair.base[2] + "; " : ""}edited by hand` : "";
  }
  function toggleMorph(pair, i) {
    pair.cells[i].morph = !pair.cells[i].morph;
    relabel(pair);
  }
  // the rows of a set whose ancestor has segment `seg` at rank k
  function ancestorRows(set, k, seg) {
    return set.rows.filter((p) => p.ranks.length > k && p.cells[p.ranks[k]].top === seg);
  }
  // morphology mode, a click on an ancestor letter: marked in every row of the set, or, if
  // every row has it marked already, unmarked in every row
  function markAncestorInSet(set, k, seg) {
    const rows = ancestorRows(set, k, seg);
    if (!rows.length) return;
    const on = !rows.every((p) => p.cells[p.ranks[k]].morph);
    snapshot(rows);
    rows.forEach((p) => { markAncestor(p.cells, k, on); relabel(p); save(p); });
    say(`${seg} ${on ? "marked" : "unmarked"} as morphology in ${rows.length} ${rows.length === 1 ? "row" : "rows"}`);
    redrawSet(set);
  }
  // gap mode, the + beside an ancestor letter: a segment added to the set's ancestor, in every
  // row whose ancestor is the one the table shows
  function insertAncestorInSet(set, pos, seg, tops) {
    const rows = set.rows.filter((p) => p.ranks.length === tops.length &&
                                        p.ranks.every((i, k) => p.cells[i].top === tops[k]));
    if (!rows.length) return;
    const test = rows[0].cells.map((c) => ({ ...c }));
    const msg = insertAncestor(test, pos, seg);
    if (msg) { say(msg, true); return; }
    snapshot(rows);
    rows.forEach((p) => { insertAncestor(p.cells, pos, seg); setRanks(p); relabel(p); save(p); });
    const where = pos < tops.length ? `before ${tops[pos]}` : `after ${tops[tops.length - 1]}`;
    say(`${clean(seg)} added to the ancestor ${where} in ${rows.length} ${rows.length === 1 ? "row" : "rows"}, ` +
        "with a gap under it; type the reflexes in, or click an added segment to move it in. U takes it back");
    reshaped(set);
  }
  // the rows of a set whose ancestor is the one the table shows
  function rowsWithAncestor(set, tops) {
    return set.rows.filter((p) => p.ranks.length === tops.length &&
                                  p.ranks.every((i, k) => p.cells[i].top === tops[k]));
  }
  // gap mode, the + of a gap column: an ancestor segment put into that column in every row
  function ancestorIntoColumnInSet(set, slot, n, seg, tops) {
    const rows = rowsWithAncestor(set, tops);
    if (!rows.length) return;
    const msg = ancestorIntoColumn(rows[0].cells.map((c) => ({ ...c })), slot, n, seg);
    if (msg) { say(msg, true); return; }
    snapshot(rows);
    rows.forEach((p) => { ancestorIntoColumn(p.cells, slot, n, seg); setRanks(p); relabel(p); save(p); });
    say(`${clean(seg)} put in the ancestor in ${rows.length} ${rows.length === 1 ? "row" : "rows"}; ` +
        "segments in that column stand under it, the other rows have a gap. U takes it back");
    reshaped(set);
  }
  // gap mode, ⌫: a whole column deleted in every row, reflex segments included
  function deleteColumnInSet(set, kind, slot, n, tops) {
    const rows = rowsWithAncestor(set, tops);
    if (!rows.length) return;
    if (kind === "rank" && tops.length < 2) { say("The ancestor would have no segment left.", true); return; }
    const touched = rows.filter((p) => kind === "rank" || n < layoutOf(p.cells).added[slot].length);
    if (!touched.length) return;
    snapshot(touched);
    touched.forEach((p) => { deleteColumn(p.cells, kind, slot, n); setRanks(p); relabel(p); save(p); });
    say(`Column deleted in ${touched.length} ${touched.length === 1 ? "row" : "rows"}; the segments in it are gone, ` +
        "so those rows no longer spell their cited form. U takes it back");
    reshaped(set);
  }
  // typing mode, an ancestor letter changed: changed in every row whose ancestor has it there
  function renameAncestorInSet(set, k, seg, value) {
    const rows = ancestorRows(set, k, seg);
    if (!rows.length) return;
    const token = clean(value);
    if (token === seg) return;
    if (token === GAP) { deleteAncestorInSet(set, k, seg); return; }
    const msg = renameAncestor(rows[0].cells.map((c) => ({ ...c })), k, value);
    if (msg) { say(msg, true); redrawSet(set); return; }
    snapshot(rows);
    rows.forEach((p) => { renameAncestor(p.cells, k, value); relabel(p); save(p); });
    say(`${seg} changed to ${token} in the ancestor in ${rows.length} ${rows.length === 1 ? "row" : "rows"}; U takes it back`);
    reshaped(set);
  }
  // an ancestor letter dragged onto a gap column: moved there in every row with the ancestor
  // the table shows
  function moveAncestorInSet(set, k, slot, n, tops, seg) {
    const rows = rowsWithAncestor(set, tops);
    if (!rows.length) return;
    const msg = moveAncestor(rows[0].cells.map((c) => ({ ...c })), k, slot, n);
    if (msg) { say(msg, true); return; }
    snapshot(rows);
    rows.forEach((p) => { moveAncestor(p.cells, k, slot, n); setRanks(p); relabel(p); save(p); });
    say(`${seg} moved in the ancestor in ${rows.length} ${rows.length === 1 ? "row" : "rows"}: segments in that column stand ` +
        `under it, and what stood under it stays in a column of its own. U takes it back`);
    reshaped(set);
  }
  // gap mode, the × under an ancestor letter: the segment taken out of the set's ancestor
  function deleteAncestorInSet(set, k, seg) {
    const rows = ancestorRows(set, k, seg);
    if (!rows.length) return;
    if (rows.some((p) => p.ranks.length < 2)) { say("The ancestor would have no segment left.", true); return; }
    snapshot(rows);
    rows.forEach((p) => { deleteAncestor(p.cells, k); setRanks(p); relabel(p); save(p); });
    say(`${seg} taken out of the ancestor in ${rows.length} ${rows.length === 1 ? "row" : "rows"}; press U to take it back`);
    reshaped(set);
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
    return layoutOf(pair.cells);
  }
  function addedCell(pair, i, green, p, n) {
    const kind = green ? "add" : "add plain";
    // where the cell is, for a segment dragged onto it
    const at = `data-slot="${p}" data-n="${n}"${i === undefined ? "" : ` data-i="${i}"`}`;
    if (i === undefined) {
      // typing mode: a segment can be typed into this row's gap in the column
      if (editing && !brush && !gapping) {
        return `<td class="seg ${kind} none" ${at}><input class="cell slotin" data-slot="${p}" data-n="${n}" data-green="${green ? 1 : 0}" value="" aria-label="Add a segment to this row here"></td>`;
      }
      return `<td class="seg ${kind} none" ${at}>${GAP}</td>`;
    }
    const seg = esc(pair.cells[i].bottom);
    if (editing && gapping) return `<td class="seg ${kind}" ${at}><button class="gb" type="button" data-i="${i}" title="Put back in the empty column beside it">${seg}</button></td>`;
    if (editing && brush) return `<td class="seg ${kind}" ${at}><button class="mk" type="button" data-i="${i}" aria-pressed="${pair.cells[i].morph}">${seg}</button></td>`;
    // a suffix or an unplaced segment can be changed too; emptied, it is taken out
    if (editing) return `<td class="seg ${kind}" ${at}><input class="cell" data-i="${i}" value="${seg}" aria-label="Segment the ancestor lacks"></td>`;
    return `<td class="seg ${kind}" ${at}>${seg}</td>`;
  }
  // `slots[p]` is how many columns the added segments before p need in this table
  function rowHTML(pair, slots, greens) {
    const excluded = /excluded/.test(pair.status);
    let h = `<tr class="${excluded ? "ex" : ""}" data-key="${esc(pair.key)}">`;
    h += `<td class="lang">${esc(pair.language)}</td><td class="form">${esc(pair.form)}</td>`;
    const { added, rank } = layout(pair);
    slots.forEach((width, p) => {
      for (let k = 0; k < width; k++) h += addedCell(pair, added[p] ? added[p][k] : undefined, greens[p][k], p, k);
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
    // columns the editor added with + and has not filled yet (set.want: place -> width)
    const slots = segs.map((_, k) => (set.want && set.want[k]) || 0).concat([(set.want && set.want[segs.length]) || 0]);
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
    let h = `<section class="set" data-set="${esc(set.id)}"><h3>${esc(set.head || segs.map((s) => s.top).join(""))}`;
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
    // gap mode: ⌫ deletes a whole column in every row of the set
    const drop = (attrs, what) => `<button class="cdel" type="button" ${attrs} aria-label="Delete ${what} in every row of this set" title="Delete ${what} in every row of this set">⌫</button>`;
    const typing = editing && !brush && !gapping;
    // typing mode: + inserts an empty column, to drag a segment into
    const column = (slot, where) => `<button class="cadd${slot === segs.length ? " after" : ""}" type="button" data-slot="${slot}" aria-label="Insert an empty column ${where}" title="Insert an empty column ${where}">+</button>`;
    slots.forEach((width, k) => {
      for (let n = 0; n < width; n++) {
        // gap mode: + puts an ancestor segment into this gap column; typing mode: the
        // segment is typed in at the top of the column
        const body = editing && gapping
          ? `<button class="aput" type="button" data-slot="${k}" data-n="${n}" aria-label="Put an ancestor segment in this column" title="Put an ancestor segment in this column">+</button>` +
            drop(`data-kind="added" data-slot="${k}" data-n="${n}"`, "this column and its segments")
          : typing
            ? `<input class="aslot" data-slot="${k}" data-n="${n}" value="" placeholder="${GAP}" aria-label="Put an ancestor segment over this column, in every row of this set" title="Type an ancestor segment to put over this column">`
            : GAP;
        h += `<th class="add${greens[k][n] ? "" : " plain"}" scope="col" data-slot="${k}" data-n="${n}">${body}</th>`;
      }
      if (k < segs.length) {
        const cls = [everyone[k] ? "m" : "", segs[k].seam ? "seam" : ""].join(" ").trim();
        const seg = esc(segs[k].top);
        let body = seg;
        if (typing) {
          // the letter can be changed in every row of the set; emptied, it is taken out of
          // the ancestor (what stood under it stays)
          body = column(k, `before ${seg}`) +
                 `<input class="acell" data-k="${k}" data-seg="${seg}" value="${seg}" aria-label="Ancestor segment ${seg}, changed in every row of this set" title="Change ${seg} in every row of this set; empty it to take it out">` +
                 (k === segs.length - 1 ? column(k + 1, `after ${seg}`) : "");
        }
        // morphology mode: the letter marks or unmarks the segment in every row of the set;
        // gap mode: × takes the segment out of the set's ancestor
        if (editing && brush) {
          body = `<button class="amk" type="button" data-k="${k}" data-seg="${seg}" aria-pressed="${everyone[k]}" title="Mark or unmark ${seg} as morphology in every row of this set">${seg}</button>`;
        } else if (editing && gapping) {
          // + adds a segment to the ancestor before this one (and, on the last, after it too)
          const add = (pos, where) => `<button class="aadd" type="button" data-pos="${pos}" aria-label="Add a segment to the ancestor ${where}" title="Add a segment to the ancestor ${where}">+</button>`;
          body = add(k, `before ${seg}`) + seg +
                 (segs.length > 1 ? `<button class="adel" type="button" data-k="${k}" data-seg="${seg}" aria-label="Take ${seg} out of the ancestor in every row of this set" title="Take ${seg} out of the ancestor; what stands under it stays">×</button>` +
                                    drop(`data-kind="rank" data-slot="${k}" data-n="0"`, `the column of ${seg}, with what stands under it,`) : "") +
                 (k === segs.length - 1 ? add(k + 1, `after ${seg}`) : "");
        }
        h += `<th scope="col"${cls ? ` class="${cls}"` : ""} data-k="${k}" data-seg="${seg}">${body}</th>`;
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
    // the sets on the page are drawn again where they are, so the reader keeps their place
    [...$("ia-sets").querySelectorAll("section.set")].forEach((section) => {
      const hit = hits.find((h) => h.set.id === section.dataset.set);
      if (hit) section.outerHTML = box(hit);
    });
    say(brush ? "Morphology mode: click a segment, or an ancestor letter for the whole set (M or Esc to leave)"
        : gapping ? "Gap mode: arrows send a segment to a gap column, × takes an ancestor segment out (G or Esc to leave)"
        : "Typing mode: drag a segment to move it, + at the top adds an empty column");
  }
  $("ia-brush").addEventListener("click", () => setTool(!brush, false));
  $("ia-gap").addEventListener("click", () => setTool(false, !gapping));
  $("ia-sets").addEventListener("click", (e) => {
    const col = e.target.closest("button.cadd");
    if (col) {
      // an empty column in that place, one wider than the widest row there
      const section = col.closest("section.set");
      const hit = hits.find((h) => h.set.id === section.dataset.set);
      if (!hit) return;
      const slot = +col.dataset.slot, set = hit.set;
      set.want = set.want || {};
      set.want[slot] = 1 + Math.max(set.want[slot] || 0,
                                    ...hit.rows.map((p) => (layoutOf(p.cells).added[slot] || []).length));
      redrawSet(set);
      say("Empty column added: drag a segment into it, or type an ancestor segment at its top");
      return;
    }
    const head = e.target.closest("button.amk, button.adel, button.aadd, button.aput, button.cdel");
    if (head) {
      const section = head.closest("section.set");
      const set = SETS.find((s) => s.id === section.dataset.set);
      const hit = hits.find((h) => h.set === set);
      if (!set || !hit) return;
      const first = hit.rows[0], tops = first.ranks.map((i) => first.cells[i].top);
      if (head.classList.contains("amk")) markAncestorInSet(set, +head.dataset.k, head.dataset.seg);
      else if (head.classList.contains("adel")) deleteAncestorInSet(set, +head.dataset.k, head.dataset.seg);
      else if (head.classList.contains("cdel")) deleteColumnInSet(set, head.dataset.kind, +head.dataset.slot, +head.dataset.n, tops);
      else {
        // + becomes a small box for the new segment: Enter adds it, Esc leaves it
        const field = document.createElement("input");
        field.className = "anew";
        if (head.classList.contains("aput")) {
          field.dataset.slot = head.dataset.slot;
          field.dataset.n = head.dataset.n;
        } else {
          field.dataset.pos = head.dataset.pos;
        }
        field.setAttribute("aria-label", head.getAttribute("aria-label"));
        field.placeholder = "+";
        head.replaceWith(field);
        field.focus();
      }
      return;
    }
    const button = e.target.closest("button.mk, button.gp, button.gb");
    if (!button) return;
    const pair = pairs.get(button.closest("tr").dataset.key);
    const i = +button.dataset.i;
    snapshot(pair);
    if (button.classList.contains("gp")) {
      toGap(pair, i, button.dataset.side);
    } else if (button.classList.contains("gb")) {
      if (!putBack(pair, i)) { UNDO.pop(); say("There is no empty column beside it. Empty one first.", true); return; }
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
    if (e.target.classList.contains("acell") || e.target.classList.contains("aslot")) {
      // the ancestor at the top of a table, typed in: changed in every row of the set
      const section = e.target.closest("section.set");
      const hit = hits.find((h) => h.set.id === section.dataset.set);
      if (!hit) return;
      const first = hit.rows[0], tops = first.ranks.map((i) => first.cells[i].top);
      if (e.target.classList.contains("aslot")) {
        if (!e.target.value.trim()) return;
        ancestorIntoColumnInSet(hit.set, +e.target.dataset.slot, +e.target.dataset.n, e.target.value, tops);
      } else {
        renameAncestorInSet(hit.set, +e.target.dataset.k, e.target.dataset.seg, e.target.value);
      }
      return;
    }
    const tr = e.target.closest("tr[data-key]");
    if (!tr) return;
    const pair = pairs.get(tr.dataset.key);
    if (e.target.classList.contains("slotin")) {
      // a segment typed into this row's gap in a green or outlined column
      if (!e.target.value.trim()) return;
      snapshot(pair);
      const msg = insertReflex(pair.cells, +e.target.dataset.slot, +e.target.dataset.n, e.target.value, e.target.dataset.green === "1");
      if (msg) { UNDO.pop(); say(msg, true); return; }
      setRanks(pair);
      save(pair);
      redraw(pair, e.target);
      return;
    }
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
  // in a segment cell: Enter goes to the next cell, Esc leaves the cell (so that the keys
  // below work again)
  $("ia-sets").addEventListener("keydown", (e) => {
    const input = e.target;
    if (input.classList && (input.classList.contains("acell") || input.classList.contains("aslot"))) {
      // the ancestor's boxes: Enter makes the change, Esc leaves the box as it was
      if (e.key !== "Enter" && e.key !== "Escape") return;
      e.preventDefault();
      if (e.key === "Escape") input.value = input.defaultValue;
      input.blur();
      return;
    }
    if (input.classList && input.classList.contains("anew")) {
      // the box for a segment to add to the ancestor
      if (e.key !== "Enter" && e.key !== "Escape") return;
      e.preventDefault();
      const section = input.closest("section.set");
      const hit = hits.find((h) => h.set.id === section.dataset.set);
      if (!hit) return;
      if (e.key === "Escape" || !input.value.trim()) { redrawSet(hit.set); return; }
      const first = hit.rows[0], tops = first.ranks.map((i) => first.cells[i].top);
      if (input.dataset.slot !== undefined) ancestorIntoColumnInSet(hit.set, +input.dataset.slot, +input.dataset.n, input.value, tops);
      else insertAncestorInSet(hit.set, +input.dataset.pos, input.value, tops);
      return;
    }
    if (!input.classList || !input.classList.contains("cell")) return;
    if (e.key === "Escape") {
      e.preventDefault();
      input.blur();
      return;
    }
    if (e.key !== "Enter") return;
    const tr = input.closest("tr[data-key]");
    e.preventDefault();
    const all = [...$("ia-sets").querySelectorAll("input.cell")];
    const next = all[all.indexOf(input) + 1];
    const nextRow = next && next.closest("tr[data-key]"), nextI = next ? +next.dataset.i : 0;
    input.blur();
    if (next && next.isConnected) next.focus();
    // the row was drawn again (an emptied suffix was taken out): find the cell anew
    else if (nextRow) focusCell(pairs.get(nextRow.dataset.key), nextRow === tr ? nextI - 1 : nextI);
  });
  // keys while editing, when no text field has the cursor: M switches morphology mode on or
  // off, G gap mode, Esc goes back to typing, U takes back the last change
  window.addEventListener("keydown", (e) => {
    if (!editing || e.ctrlKey || e.metaKey || e.altKey) return;
    const t = e.target;
    if (t && (t.isContentEditable || /^(INPUT|TEXTAREA|SELECT)$/.test(t.tagName) && t.type !== "checkbox")) return;
    const key = e.key.toLowerCase();
    if (key === "m") setTool(!brush, false);
    else if (key === "g") setTool(false, !gapping);
    else if (key === "escape" && (brush || gapping)) setTool(false, false);
    else if (key === "u") undoLast();
    else return;
    e.preventDefault();
  });
  // ------------------------------------------------------------- dragging
  // While editing, in any mode, a reflex segment dragged within its row goes onto a gap under
  // an ancestor segment, or into a green or outlined column (before the segment the row has
  // there, or after its others); an ancestor letter dragged onto a green or outlined column
  // goes there in every row of the set. A drag starts once the pointer leaves the cell, so a
  // click still types, marks, or moves as before. + in typing mode adds an empty column.
  let drag = null, swallowClick = false;
  function dragSource(el) {
    if (!editing || !el.closest) return null;
    if (el.closest("button.cadd, button.aadd, button.adel, button.aput, button.cdel, input.anew, input.slotin, input.aslot, td.tools")) return null;
    const td = el.closest("td.seg");
    if (td) {
      const tr = td.closest("tr[data-key]");
      const pair = tr && pairs.get(tr.dataset.key);
      const i = td.dataset.i === undefined ? NaN : +td.dataset.i;
      if (!pair || Number.isNaN(i) || !pair.cells[i] || pair.cells[i].bottom === GAP) return null;
      return { kind: "reflex", el: td, pair, i, label: pair.cells[i].bottom };
    }
    const th = el.closest("tr.anc th[data-k]");
    if (th) {
      const hit = hits.find((h) => h.set.id === th.closest("section.set").dataset.set);
      if (!hit) return null;
      return { kind: "ancestor", el: th, hit, k: +th.dataset.k, label: th.dataset.seg };
    }
    return null;
  }
  // the cell under the pointer that the dragged segment can go to, if any
  function dropTarget(source, x, y) {
    const el = document.elementFromPoint(x, y);
    if (!el || !el.closest) return null;
    if (source.kind === "reflex") {
      const td = el.closest("td.seg");
      if (!td || td.closest("tr") !== source.el.closest("tr")) return null;
      if (td.dataset.slot !== undefined) return { el: td, target: { kind: "added", p: +td.dataset.slot, n: +td.dataset.n } };
      if (td.dataset.i === undefined) return null;
      const i = +td.dataset.i, p = source.pair.ranks.indexOf(i);
      if (p < 0) return null;
      return { el: td, target: { kind: "rank", p }, full: i !== source.i && source.pair.cells[i].bottom !== GAP };
    }
    const th = el.closest("tr.anc th.add[data-slot]");
    if (!th || th.closest("section.set") !== source.el.closest("section.set")) return null;
    return { el: th, target: { p: +th.dataset.slot, n: +th.dataset.n } };
  }
  function endDrag(d) {
    if (d.ghost) d.ghost.remove();
    if (d.over) d.over.classList.remove("dropok", "dropno");
    d.source.el.classList.remove("dragfrom");
    document.body.classList.remove("ia-dragging");
  }
  $("ia-sets").addEventListener("pointerdown", (e) => {
    if (e.button !== 0 || e.pointerType === "touch") return;
    const source = dragSource(e.target);
    drag = source ? { source, on: false, ghost: null, over: null } : null;
  });
  window.addEventListener("pointermove", (e) => {
    if (!drag) return;
    if (!drag.on) {
      const r = drag.source.el.getBoundingClientRect();
      if (e.clientX >= r.left - 2 && e.clientX <= r.right + 2 && e.clientY >= r.top - 2 && e.clientY <= r.bottom + 2) return;
      if (!drag.source.el.isConnected) { drag = null; return; }
      drag.on = true;
      const active = document.activeElement;
      if (active && active !== document.body && active.blur) active.blur();
      drag.ghost = document.createElement("div");
      drag.ghost.className = "ia-ghost";
      drag.ghost.textContent = drag.source.label;
      document.body.appendChild(drag.ghost);
      document.body.classList.add("ia-dragging");
      drag.source.el.classList.add("dragfrom");
    }
    const selection = window.getSelection && window.getSelection();
    if (selection) selection.removeAllRanges();
    drag.ghost.style.left = `${e.clientX + 12}px`;
    drag.ghost.style.top = `${e.clientY + 8}px`;
    const hit = dropTarget(drag.source, e.clientX, e.clientY);
    if (drag.over && (!hit || drag.over !== hit.el)) drag.over.classList.remove("dropok", "dropno");
    drag.over = hit ? hit.el : null;
    if (hit) hit.el.classList.add(hit.full ? "dropno" : "dropok");
  });
  window.addEventListener("pointerup", (e) => {
    const d = drag;
    drag = null;
    if (!d || !d.on) return;
    endDrag(d);
    // the click that ends a drag does nothing else
    swallowClick = true;
    setTimeout(() => { swallowClick = false; }, 0);
    if (!d.source.el.isConnected) return;
    const hit = dropTarget(d.source, e.clientX, e.clientY);
    if (!hit) {
      say(d.source.kind === "reflex"
        ? "Drop the segment on a gap in its own row, or on a green or outlined column; + adds an empty column"
        : "Drop the ancestor letter on a green or outlined column at the top of its table; + adds an empty column", true);
      return;
    }
    if (d.source.kind === "reflex") {
      const pair = d.source.pair;
      snapshot(pair);
      const msg = moveReflex(pair.cells, d.source.i, hit.target);
      if (msg) { UNDO.pop(); say(msg, true); return; }
      setRanks(pair);
      save(pair);
      redraw(pair, d.source.el);
      say(`${d.source.label} moved; U takes it back`);
      return;
    }
    const { hit: table, k, label } = d.source;
    const first = table.rows[0], tops = first.ranks.map((i) => first.cells[i].top);
    moveAncestorInSet(table.set, k, hit.target.p, hit.target.n, tops, label);
  });
  window.addEventListener("pointercancel", () => { if (drag && drag.on) endDrag(drag); drag = null; });
  $("ia-sets").addEventListener("click", (e) => {
    if (!swallowClick) return;
    swallowClick = false;
    e.stopPropagation();
    e.preventDefault();
  }, true);
  // no native dragging of the text in a box while editing
  $("ia-sets").addEventListener("dragstart", (e) => { if (editing) e.preventDefault(); });

  window.addEventListener("online", flush);
  start().catch((e) => {
    $("ia-sets").innerHTML = `<div class="empty">The alignments could not be loaded (${esc(e.message)}). Reload the page to try again.</div>`;
  });
})();
