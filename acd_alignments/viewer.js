// Viewer for the ACD alignments. Data: data/index.json (levels, languages, one
// entry per reconstruction) and data/<LEVEL>_<n>.json (the rows), written by
// causal/publish_acd.py in the analysis repository.
(function () {
  "use strict";
  var base = (document.currentScript && document.currentScript.src.replace(/viewer\.js.*$/, "")) || "./";
  var PER_PAGE = 20;
  var index = null, cache = {}, shown = [], page = 0;
  var $ = function (id) { return document.getElementById(id); };

  function el(tag, cls, text) {
    var node = document.createElement(tag);
    if (cls) node.className = cls;
    if (text !== undefined && text !== null) node.textContent = text;
    return node;
  }

  function fetchJson(name) {
    if (cache[name]) return Promise.resolve(cache[name]);
    return fetch(base + "data/" + name).then(function (r) {
      if (!r.ok) throw new Error(name + ": " + r.status);
      return r.json();
    }).then(function (d) { cache[name] = d; return d; });
  }

  // combining marks U+0300 to U+036F, built from code points so that no tool rewrites the range
  var MARKS = new RegExp("[" + String.fromCharCode(0x300) + "-" + String.fromCharCode(0x36f) + "]", "g");

  function plain(s) {
    return String(s).normalize("NFD").replace(MARKS, "").replace(/[*₀-₉]/g, "").toLowerCase();
  }

  function filterSets() {
    var level = $("acd-level").value, find = plain($("acd-find").value.trim());
    var langName = $("acd-lang").value.trim(), lang = -1;
    if (langName) {
      lang = index.languages.indexOf(langName);
      if (lang < 0) {
        var want = plain(langName);
        for (var i = 0; i < index.languages.length; i++) {
          if (plain(index.languages[i]).indexOf(want) === 0) { lang = i; break; }
        }
      }
    }
    shown = [];
    for (var k = 0; k < index.sets.length; k++) {
      var s = index.sets[k];
      if (level !== "" && String(s[0]) !== level) continue;
      if (find && plain(s[2]).indexOf(find) < 0 && plain(s[3]).indexOf(find) < 0) continue;
      if (langName && (lang < 0 || s[5].indexOf(lang) < 0)) continue;
      shown.push(k);
    }
    shown.lang = langName ? lang : null;
    page = 0;
    render();
  }

  function alignment(anc, ref) {
    var a = anc.split(" "), r = ref.split(" ");
    var table = el("table", "acd-al"), top = el("tr", "acd-anc"), bottom = el("tr");
    var inAffix = false;
    for (var i = 0; i < a.length; i++) {
      var cell = a[i], opens = cell.indexOf("[") >= 0, shuts = cell.indexOf("]") >= 0;
      var affix = inAffix || opens;
      inAffix = affix && !shuts;
      var up = el("td", null, cell === "-" ? "" : cell), down = el("td", null, (r[i] === undefined || r[i] === "-") ? "–" : r[i]);
      if (cell === "-") { down.className = "acd-added"; up.className = "acd-nil"; up.textContent = "·"; }
      else {
        if (affix || cell.indexOf("(") >= 0) up.className = "acd-affix";
        if (r[i] === "-" || r[i] === undefined) down.className = "acd-gap";
      }
      top.appendChild(up); bottom.appendChild(down);
    }
    table.appendChild(top); table.appendChild(bottom);
    return table;
  }

  function renderSet(entry, rows) {
    var box = el("div", "acd-set"), head = el("h3");
    head.appendChild(el("span", null, entry[2]));
    head.appendChild(el("span", "acd-gloss", "‘" + entry[3] + "’"));
    head.appendChild(el("span", "acd-id", index.levels[entry[0]][0] + " · set " + entry[1] + " · " + entry[7] + " reflexes"));
    box.appendChild(head);
    var table = el("table", "acd-rows"), body = el("tbody"), withProblems = $("acd-problems").checked, hidden = 0, n = 0;
    rows.forEach(function (row) {
      if (shown.lang !== null && shown.lang >= 0 && row[0] !== shown.lang) return;
      if (row[5] === 3 && !withProblems) { hidden++; return; }
      var tr = el("tr");
      tr.appendChild(el("td", "acd-lang", index.languages[row[0]] + " (" + index.groups[row[0]] + ")"));
      var form = el("td");
      form.appendChild(el("div", "acd-form", row[1]));
      // the dictionary's source links, [Nihira 1983](Source#cldf:nihira1983), are shown as the name alone
      form.appendChild(el("div", "acd-mean", row[2].replace(/\[([^\]]+)\]\(Source#[^)]*\)/g, "$1")));
      tr.appendChild(form);
      var al = el("td");
      if (row[3]) al.appendChild(alignment(row[3], row[4]));
      tr.appendChild(al);
      var note = el("td", "acd-note");
      if (row[5] === 1) note.appendChild(el("span", "acd-tag acd-fixed", "corrected"));
      if (row[5] === 2) note.appendChild(el("span", "acd-tag acd-fixed", "second reading"));
      if (row[5] === 3) note.appendChild(el("span", "acd-tag acd-prob", "problem" + (row[6] ? ": " + row[6] : "")));
      var text = [row[7], row[8]].filter(Boolean).join(" · ");
      if (text) note.appendChild(document.createTextNode(text));
      tr.appendChild(note);
      body.appendChild(tr);
      n++;
    });
    table.appendChild(body);
    box.appendChild(table);
    if (hidden) box.appendChild(el("p", "acd-status", hidden + " row" + (hidden > 1 ? "s" : "") + " marked as a problem not shown"));
    return n || hidden ? box : null;
  }

  function render() {
    var pages = Math.max(1, Math.ceil(shown.length / PER_PAGE));
    page = Math.min(Math.max(page, 0), pages - 1);
    var part = shown.slice(page * PER_PAGE, (page + 1) * PER_PAGE).map(function (k) { return index.sets[k]; });
    var label = "Page " + (page + 1) + " of " + pages;
    ["acd-page", "acd-page2"].forEach(function (id) { $(id).textContent = label; });
    ["acd-prev", "acd-prev2"].forEach(function (id) { $(id).disabled = page === 0; });
    ["acd-next", "acd-next2"].forEach(function (id) { $(id).disabled = page >= pages - 1; });
    $("acd-status").textContent = shown.length.toLocaleString() + " reconstruction" + (shown.length === 1 ? "" : "s") +
      ($("acd-lang").value.trim() && shown.lang < 0 ? " (no language of that name)" : "");
    var needed = {};
    part.forEach(function (s) { needed[index.files[s[4]]] = true; });
    var token = render.token = {};
    Promise.all(Object.keys(needed).map(fetchJson)).then(function () {
      if (render.token !== token) return;
      var host = $("acd-sets");
      host.textContent = "";
      part.forEach(function (s) {
        var rows = cache[index.files[s[4]]][s[1]];
        var box = rows && renderSet(s, rows);
        if (box) host.appendChild(box);
      });
    }).catch(function (e) { $("acd-status").textContent = "Could not load the data: " + e.message; });
  }

  function start(data) {
    index = data;
    var c = index.counts, n = function (x) { return (x || 0).toLocaleString(); };
    $("acd-counts").textContent = n(c.rows) + " reflexes of " + n(c.sets) + " reconstructions in " + n(c.languages) +
      " lects (" + n(c.glottocodes) + " Glottolog languages). The reviewer confirmed " + n(c.confirmed) + ", realigned " +
      n((c.corrected || 0) + (c["resolved on second reading"] || 0)) + ", and marked " + n(c.problem) + " as a problem.";
    var level = $("acd-level");
    level.appendChild(new Option("all levels", ""));
    index.levels.forEach(function (lv, i) { level.appendChild(new Option(lv[0] + " · " + lv[1], String(i))); });
    var list = $("acd-langs");
    index.languages.forEach(function (name) { var o = document.createElement("option"); o.value = name; list.appendChild(o); });
    var files = $("acd-files"), d = index.downloads;
    [["reviewed", "The reviewed table (CSV), without the rows marked as a problem"],
     ["reviewed_short", "The same rows with the final alignments alone (no review columns)"],
     ["problems", "The rows marked as a problem, with the reviewer's alignment where one was given"],
     ["not_aligned", "The forms that could not be read, with the reason"],
     ["readme", "README: columns, conventions and how the tables were made"]].forEach(function (pair) {
      if (!d[pair[0]]) return;
      var li = el("li"), a = el("a", null, pair[1]);
      a.href = base + "data/" + d[pair[0]];
      li.appendChild(a); files.appendChild(li);
    });
    ["acd-level", "acd-problems"].forEach(function (id) { $(id).addEventListener("change", id === "acd-problems" ? render : filterSets); });
    var timer = null;
    ["acd-find", "acd-lang"].forEach(function (id) {
      $(id).addEventListener("input", function () { clearTimeout(timer); timer = setTimeout(filterSets, 250); });
    });
    [["acd-prev", -1], ["acd-prev2", -1], ["acd-next", 1], ["acd-next2", 1]].forEach(function (pair) {
      $(pair[0]).addEventListener("click", function () { page += pair[1]; render(); window.scrollTo(0, $("acd-status").offsetTop - 80); });
    });
    filterSets();
  }

  fetchJson("index.json").then(start).catch(function (e) {
    $("acd-status").textContent = "Could not load the data: " + e.message;
  });
})();
