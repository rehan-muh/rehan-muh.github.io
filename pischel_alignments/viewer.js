// Viewer and editor for Pischel's forms aligned to their Sanskrit equivalents
// (data/sets.json, written by causal/publish_pischel.py in the word-position project).
//
// The site is static, so edits are kept in the browser's localStorage. "Export"
// writes them as a CSV in the format of data/local/corrections/*.csv, which
// run_v3.apply_corrections reads. Published corrections are read from
// data/corrections.csv, so replacing that file with an export shows them to everyone.
(function () {
  const FIELDS = ["Analysis", "Set", "Language", "Rank", "Original", "Corrected", "Note", "Saved"];
  const ANALYSES = ["Old Indo-Aryan > Prakrits", "Reconstructed OIA > Prakrits"];
  const GAP = "-";

  const cellCSV = (v) => {
    const s = String(v === undefined || v === null ? "" : v);
    return /[",\r\n]/.test(s) ? '"' + s.replace(/"/g, '""') + '"' : s;
  };
  function toCSV(rows) {
    return [FIELDS.join(",")].concat(rows.map((r) => FIELDS.map((f) => cellCSV(r[f])).join(","))).join("\n") + "\n";
  }
  function parseCSV(text) {
    const rows = [];
    let row = [], cell = "", quoted = false;
    text = text.replace(/^﻿/, "");
    for (let i = 0; i < text.length; i++) {
      const c = text[i];
      if (quoted) {
        if (c === '"' && text[i + 1] === '"') { cell += '"'; i++; }
        else if (c === '"') quoted = false;
        else cell += c;
      } else if (c === '"') quoted = true;
      else if (c === ",") { row.push(cell); cell = ""; }
      else if (c === "\n" || c === "\r") {
        if (c === "\r" && text[i + 1] === "\n") i++;
        row.push(cell); cell = "";
        if (row.some((x) => x !== "")) rows.push(row);
        row = [];
      } else cell += c;
    }
    if (cell !== "" || row.length) { row.push(cell); if (row.some((x) => x !== "")) rows.push(row); }
    if (!rows.length) return [];
    const head = rows[0];
    return rows.slice(1).map((r) => Object.fromEntries(head.map((h, i) => [h, r[i] === undefined ? "" : r[i]])));
  }
  const setId = (a, k) => `pischel_full:${ANALYSES[a]}|${k}`;
  const pairKey = (a, k, language) => `${setId(a, k)}\t${language}`;
  const clean = (v) => {
    const s = String(v).normalize("NFC").trim();
    return s === "" || s === "–" || s === "—" ? GAP : s;
  };

  // {pair key: {cells: {rank: value}, exclude: bool, note: text, saved: time}}
  function rowsToEdits(rows) {
    const out = {};
    rows.forEach((r) => {
      if (!r.Set || !r.Language) return;
      const key = `${r.Set}\t${r.Language}`;
      const e = out[key] || (out[key] = { cells: {}, exclude: false, note: "", saved: "" });
      const value = String(r.Corrected || "").trim();
      if (value === "<exclude>") e.exclude = true;
      else if (r.Rank !== "*" && r.Rank !== "") e.cells[r.Rank] = value === "" ? GAP : value;
      if (r.Note) e.note = r.Note;
      if (r.Saved) e.saved = r.Saved;
    });
    return out;
  }
  // `original(key, rank)` gives the machine alignment of a segment.
  function editsToRows(edits, original) {
    const rows = [];
    Object.keys(edits).sort().forEach((key) => {
      const e = edits[key];
      const [set, language] = key.split("\t");
      const analysis = set.slice(set.indexOf(":") + 1, set.indexOf("|"));
      const base = { Analysis: analysis, Set: set, Language: language, Saved: e.saved || "" };
      let wrote = false;
      if (e.exclude) {
        rows.push({ ...base, Rank: "*", Original: "", Corrected: "<exclude>", Note: e.note || "" });
        wrote = true;
      }
      Object.keys(e.cells).sort((a, b) => a - b).forEach((rank) => {
        const was = original(key, rank), now = e.cells[rank];
        if (now === was) return;
        rows.push({ ...base, Rank: rank, Original: was === GAP ? "" : was, Corrected: now === GAP ? "" : now,
                    Note: wrote ? "" : e.note || "" });
        wrote = true;
      });
      if (!wrote && e.note) {
        const was = original(key, 0);
        rows.push({ ...base, Rank: "0", Original: was === GAP ? "" : was, Corrected: was === GAP ? "" : was, Note: e.note });
      }
    });
    return rows;
  }

  if (typeof document === "undefined") {
    module.exports = { toCSV, parseCSV, rowsToEdits, editsToRows, clean, pairKey };
    return;
  }

  const BASE = document.currentScript.src.replace(/viewer\.js.*$/, "data/");
  const STORE = "pischel-alignments-edits-v1";
  const PAGE = 25;
  const $ = (id) => document.getElementById(id);
  const NAMES = { M: "Māhārāṣṭrī", AMg: "Ardhamāgadhī", JM: "Jaina Māhārāṣṭrī", "Ś": "Śaurasenī", "JŚ": "Jaina Śaurasenī",
    Mg: "Māgadhī", A: "Apabhraṃśa", P: "Paiśācī", CP: "Cūlikāpaiśācika", "Ḍh": "Ḍhakkī", D: "Dākṣiṇātyā",
    "D or Ḍh": "Dākṣiṇātyā or Ḍhakkī, unresolved", "Āv": "Āvantī", PG: "Pallava grants", "Pāli": "Pāli", Pkt: "No variety named" };
  const ORDER = ["M", "AMg", "JM", "Ś", "JŚ", "Mg", "A", "P", "CP", "Ḍh", "D", "D or Ḍh", "Āv", "PG", "Pāli", "Pkt"];
  const TAGS = { false: "rejected", read: "corrected", text: "bad reading", unclear: "unclear", phrase: "phrase", addendum: "addendum", cont: "cut off" };
  const esc = (s) => String(s).replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[c]);
  const fold = (s) => s.normalize("NFD").replace(/[̀-ͯ]/g, "").replace(/ʰ/g, "h").toLowerCase().replace(/[^a-z]/g, "");

  let SETS = [], hits = [], shown = 0, editing = false, wait = null;
  let published = {}, local = {};
  const picked = new Set();
  const rowsByKey = new Map();

  function loadLocal() {
    try { return JSON.parse(localStorage.getItem(STORE)) || {}; } catch (e) { return {}; }
  }
  function saveLocal() {
    try { localStorage.setItem(STORE, JSON.stringify(local)); }
    catch (e) { $("pa-count").textContent = "This browser refused to save. Export your corrections now."; return; }
    showCount();
  }
  const original = (key, rank) => {
    const r = rowsByKey.get(key);
    return r && r.al[rank] !== undefined ? r.al[rank] : GAP;
  };
  function merged(key) {
    const p = published[key], l = local[key];
    if (!p && !l) return null;
    return { cells: { ...(p ? p.cells : {}), ...(l ? l.cells : {}) },
             exclude: l && "exclude" in l ? l.exclude : !!(p && p.exclude),
             note: l && "note" in l ? l.note : (p ? p.note : ""),
             saved: (l && l.saved) || (p && p.saved) || "" };
  }
  function allEdits() {
    const out = {};
    new Set([...Object.keys(published), ...Object.keys(local)]).forEach((k) => { out[k] = merged(k); });
    return out;
  }
  const hasChange = (key) => {
    const e = merged(key);
    return !!e && (e.exclude || !!e.note || Object.keys(e.cells).some((r) => e.cells[r] !== original(key, r)));
  };
  function touch(key) {
    const l = local[key] || (local[key] = { cells: {} });
    l.saved = new Date().toISOString().slice(0, 19);
    return l;
  }
  function prune(key) {
    const l = local[key];
    if (l && !Object.keys(l.cells).length && !("exclude" in l) && !("note" in l)) delete local[key];
  }
  function setCell(key, rank, value) {
    const p = published[key];
    const base = p && p.cells[rank] !== undefined ? p.cells[rank] : original(key, rank);
    const l = touch(key);
    if (value === base) delete l.cells[rank]; else l.cells[rank] = value;
    prune(key);
    saveLocal();
  }
  function setExclude(key, on) {
    const base = !!(published[key] && published[key].exclude);
    const l = touch(key);
    if (on === base) delete l.exclude; else l.exclude = on;
    prune(key);
    saveLocal();
  }
  function setNote(key, text) {
    const base = published[key] ? published[key].note : "";
    const l = touch(key);
    if (text === base) delete l.note; else l.note = text;
    prune(key);
    saveLocal();
  }
  function showCount() {
    const n = Object.keys(local).length;
    $("pa-count").textContent = n === 1 ? "1 pair edited in this browser" : `${n.toLocaleString("en-US")} pairs edited in this browser`;
    $("pa-discard").disabled = !n;
  }

  const secs = (r) => {
    if (!r.nums.length) return "";
    const v = r.nums.slice(0, 3).join(", ") + (r.nums.length > 3 ? ` +${r.nums.length - 3}` : "");
    return `§ ${v}${r.pg ? `, p. ${r.pg}` : ""}${r.n > 1 ? ` · ${r.n}×` : ""}`;
  };
  function cells(r, set, edit) {
    const by = {};
    r.ins.forEach((x) => { by[x[0]] = (by[x[0]] || "") + `<span class="in">${esc(x[1].replace(/ /g, ""))}</span>`; });
    return set.segs.map((seg, i) => {
      const was = r.al[i] === undefined ? GAP : r.al[i];
      const now = edit && edit.cells[i] !== undefined ? edit.cells[i] : was;
      return cellHTML(seg, was, now, i, by[i] || "", i === set.segs.length - 1 ? by[set.segs.length] || "" : "");
    }).join("");
  }
  function cellHTML(seg, was, now, i, before, after) {
    const lost = now === GAP;
    const cls = ["seg", lost ? "lost" : now !== seg ? "c" : "", now !== was ? "fix" : ""].join(" ");
    const tip = now !== was ? ` title="machine alignment: ${esc(was)}"` : "";
    const body = editing
      ? `<input class="cell" data-i="${i}" value="${esc(now === GAP ? "" : now)}" aria-label="Reflex of ${esc(seg)}">`
      : esc(lost ? GAP : now);
    return `<td class="${cls}" data-i="${i}" data-seg="${esc(seg)}"${tip}>${before}${body}${after}</td>`;
  }
  function box(hit) {
    const { set, rows } = hit;
    let h = `<section class="set"><h3>${esc(set.s)}`;
    if (set.st) h += ' <span class="star">reconstructed</span>';
    h += ` <span class="id">${set.vars} ${set.vars === 1 ? "variety" : "varieties"}, ${set.r.length} ${set.r.length === 1 ? "form" : "forms"}</span></h3>`;
    h += '<table><thead><tr class="anc"><th class="lang" scope="col">Variety</th><th class="form" scope="col">As printed</th>';
    set.segs.forEach((s) => { h += `<th scope="col">${esc(s)}</th>`; });
    h += '<th class="where" scope="col">Where</th>' + (editing ? '<th class="where" scope="col">Edit</th>' : "") + "</tr></thead><tbody>";
    rows.forEach((r) => {
      const edit = merged(r.key);
      const tags = r.fl ? r.fl.split(",").filter((x) => TAGS[x]).map((x) => `<span class="tag ${x}">${TAGS[x]}</span>`).join("") : "";
      const rej = /(^|,)(false|text)(,|$)/.test(r.fl);
      const excluded = !!(edit && edit.exclude);
      h += `<tr class="${rej ? "rej" : ""} ${excluded ? "ex" : ""}" data-key="${esc(r.key)}">`;
      h += `<td class="lang" title="${esc(NAMES[r.lab] || r.lab)}">${esc(r.lab)}</td><td class="form">${esc(r.form)}</td>`;
      h += cells(r, set, edit);
      h += `<td class="where">${secs(r)}${tags}${r.src === "G" ? '<span class="tag">grammarians</span>' : ""}</td>`;
      if (editing) {
        h += `<td class="tools"><label><input class="exclude" type="checkbox"${excluded ? " checked" : ""}> exclude</label>` +
             `<input class="note" type="text" placeholder="note" aria-label="Note" value="${esc(edit && edit.note ? edit.note : "")}"></td>`;
      } else if (edit && edit.note) {
        h += `<td class="where">${esc(edit.note)}</td>`;
      }
      h += "</tr>";
    });
    return h + "</tbody></table></section>";
  }
  function draw() {
    const stop = Math.min(shown + PAGE, hits.length);
    let html = "";
    for (let i = shown; i < stop; i++) html += box(hits[i]);
    $("pa-sets").insertAdjacentHTML("beforeend", html);
    shown = stop;
    $("pa-more").hidden = shown >= hits.length;
  }
  function keep(r, st) {
    if (picked.size && !picked.has(r.lab)) return false;
    if (st.view === "ana" && !r.ana) return false;
    if (st.view === "fix" && !hasChange(r.key)) return false;
    if (!["all", "ana", "fix"].includes(st.view) && !r.fl.split(",").includes(st.view)) return false;
    if (st.src === "T" && !r.src.includes("T")) return false;
    if (st.src === "G" && r.src !== "G") return false;
    if ((st.a || st.b) && !r.nums.some((x) => x >= (st.a || 1) && x <= (st.b || 999))) return false;
    return true;
  }
  function apply() {
    const st = { q: fold($("pa-q").value), view: $("pa-view").value, src: $("pa-src").value,
                 a: +$("pa-s1").value || 0, b: +$("pa-s2").value || 0 };
    hits = [];
    let forms = 0;
    SETS.forEach((set) => {
      const whole = st.q && set.f.includes(st.q);
      const rows = set.r.filter((r) => keep(r, st) && (!st.q || whole || r.f.includes(st.q)));
      if (rows.length) { hits.push({ set, rows }); forms += rows.length; }
    });
    const how = $("pa-sort").value;
    hits.sort((x, y) => {
      if (how === "a") return x.set.f < y.set.f ? -1 : x.set.f > y.set.f ? 1 : 0;
      if (how === "s") return x.set.first - y.set.first;
      return (y.set.vars - x.set.vars) || (y.rows.length - x.rows.length);
    });
    $("pa-status").textContent = `${forms.toLocaleString("en-US")} forms in ${hits.length.toLocaleString("en-US")} sets`;
    $("pa-sets").innerHTML = hits.length ? "" : '<div class="empty">No forms match these filters.</div>';
    shown = 0;
    draw();
  }
  function refreshCell(td, key) {
    const i = +td.dataset.i, r = rowsByKey.get(key), e = merged(key);
    const was = r.al[i] === undefined ? GAP : r.al[i];
    const now = e && e.cells[i] !== undefined ? e.cells[i] : was;
    td.classList.toggle("lost", now === GAP);
    td.classList.toggle("c", now !== GAP && now !== td.dataset.seg);
    td.classList.toggle("fix", now !== was);
    if (now !== was) td.title = `machine alignment: ${was}`; else td.removeAttribute("title");
  }
  function chips() {
    const counts = {};
    SETS.forEach((s) => s.r.forEach((r) => { counts[r.lab] = (counts[r.lab] || 0) + 1; }));
    const labels = ORDER.filter((k) => counts[k]).concat(Object.keys(counts).filter((k) => !ORDER.includes(k)).sort());
    labels.forEach((k) => {
      const b = document.createElement("button");
      b.type = "button"; b.title = NAMES[k] || k;
      b.setAttribute("aria-pressed", "false");
      b.innerHTML = `${esc(k)}<small>${counts[k].toLocaleString("en-US")}</small>`;
      b.addEventListener("click", () => {
        if (picked.has(k)) picked.delete(k); else picked.add(k);
        b.setAttribute("aria-pressed", picked.has(k) ? "true" : "false");
        apply();
      });
      $("pa-chips").appendChild(b);
    });
  }
  function download() {
    const rows = editsToRows(allEdits(), original);
    const blob = new Blob(["﻿" + toCSV(rows)], { type: "text/csv;charset=utf-8" });
    const a = document.createElement("a");
    a.href = URL.createObjectURL(blob);
    a.download = `pischel_corrections_${new Date().toISOString().slice(0, 10)}.csv`;
    document.body.appendChild(a);
    a.click();
    a.remove();
    setTimeout(() => URL.revokeObjectURL(a.href), 2000);
  }
  async function bringIn(file) {
    const incoming = rowsToEdits(parseCSV(await file.text()));
    let n = 0;
    Object.keys(incoming).forEach((key) => {
      if (!rowsByKey.has(key)) return;
      const e = incoming[key];
      local[key] = { cells: { ...(local[key] ? local[key].cells : {}), ...e.cells }, saved: e.saved };
      if (e.exclude) local[key].exclude = true;
      if (e.note) local[key].note = e.note;
      n++;
    });
    saveLocal();
    $("pa-count").textContent = `Imported ${n.toLocaleString("en-US")} ${n === 1 ? "pair" : "pairs"}. ` + $("pa-count").textContent;
    apply();
  }

  async function start() {
    const r = await fetch(BASE + "sets.json");
    if (!r.ok) throw new Error(`${r.status} ${r.statusText}`);
    SETS = await r.json();
    SETS.forEach((set) => {
      set.f = fold(set.s);
      set.segs = set.t.split(" ");
      set.first = 9999;
      const names = new Set();
      set.r = set.r.map((a) => {
        const row = { lab: a[0], language: a[1] || a[0], form: a[2], al: a[3].split(" "), ins: a[4], sec: a[5], n: a[6], src: a[7], fl: a[8], pg: a[9] };
        row.key = pairKey(set.a, set.k, row.language);
        row.f = fold(row.form);
        row.nums = row.sec ? row.sec.split(",").map(Number).filter((x) => x > 0) : [];
        row.nums.forEach((x) => { if (x < set.first) set.first = x; });
        row.ana = row.ins.some((x) => x[0] > 0 && x[0] < row.al.length && /^[aeiouãẽĩõũ]/.test(x[1]));
        names.add(row.lab);
        rowsByKey.set(row.key, row);
        return row;
      });
      set.vars = names.size;
    });
    try {
      const c = await fetch(BASE + "corrections.csv", { cache: "no-cache" });
      if (c.ok) published = rowsToEdits(parseCSV(await c.text()));
    } catch (e) { published = {}; }
    local = loadLocal();
    chips();
    showCount();
    apply();
  }

  $("pa-q").addEventListener("input", () => { clearTimeout(wait); wait = setTimeout(apply, 140); });
  ["pa-view", "pa-src", "pa-sort"].forEach((id) => $(id).addEventListener("change", apply));
  ["pa-s1", "pa-s2"].forEach((id) => $(id).addEventListener("input", () => { clearTimeout(wait); wait = setTimeout(apply, 200); }));
  $("pa-more").addEventListener("click", draw);
  $("pa-sva").addEventListener("click", () => { $("pa-s1").value = 131; $("pa-s2").value = 140; $("pa-sort").value = "s"; apply(); });
  $("pa-clear").addEventListener("click", () => {
    ["pa-q", "pa-s1", "pa-s2", "pa-src"].forEach((id) => { $(id).value = ""; });
    $("pa-view").value = "all"; $("pa-sort").value = "v"; picked.clear();
    document.querySelectorAll("#pa-chips button").forEach((b) => b.setAttribute("aria-pressed", "false"));
    apply();
  });
  $("pa-edit").addEventListener("click", () => {
    editing = !editing;
    $("pa-edit").setAttribute("aria-pressed", editing ? "true" : "false");
    $("pa-editbar").hidden = !editing;
    apply();
  });
  $("pa-export").addEventListener("click", download);
  $("pa-import").addEventListener("click", () => $("pa-file").click());
  $("pa-file").addEventListener("change", (e) => { if (e.target.files[0]) bringIn(e.target.files[0]); e.target.value = ""; });
  $("pa-discard").addEventListener("click", () => {
    if (!window.confirm("Discard all edits saved in this browser? Export them first if you want to keep them.")) return;
    local = {};
    saveLocal();
    apply();
  });
  $("pa-sets").addEventListener("change", (e) => {
    const tr = e.target.closest("tr[data-key]");
    if (!tr) return;
    const key = tr.dataset.key;
    if (e.target.classList.contains("cell")) {
      setCell(key, e.target.dataset.i, clean(e.target.value));
      refreshCell(e.target.closest("td"), key);
    } else if (e.target.classList.contains("exclude")) {
      setExclude(key, e.target.checked);
      tr.classList.toggle("ex", e.target.checked);
    } else if (e.target.classList.contains("note")) {
      setNote(key, e.target.value.trim());
    }
  });
  $("pa-sets").addEventListener("keydown", (e) => {
    if (e.key !== "Enter" || !e.target.classList.contains("cell")) return;
    e.preventDefault();
    const all = [...$("pa-sets").querySelectorAll("input.cell")];
    const next = all[all.indexOf(e.target) + 1];
    e.target.blur();
    if (next) next.focus();
  });
  start().catch((e) => {
    $("pa-sets").innerHTML = `<div class="empty">The alignments could not be loaded (${esc(e.message)}). Reload the page to try again.</div>`;
  });
})();
