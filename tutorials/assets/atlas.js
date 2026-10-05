(function () {
  "use strict";

  var store = {
    get: function (k, fallback) {
      try { var v = localStorage.getItem(k); return v === null ? fallback : JSON.parse(v); }
      catch (e) { return fallback; }
    },
    set: function (k, v) { try { localStorage.setItem(k, JSON.stringify(v)); } catch (e) { /* private mode */ } }
  };

  /* ---- copy buttons ---- */
  document.querySelectorAll("div.sourceCode").forEach(function (block) {
    var btn = document.createElement("button");
    btn.type = "button";
    btn.className = "copy";
    btn.textContent = "Copy";
    btn.setAttribute("aria-label", "Copy this code");
    btn.addEventListener("click", function () {
      var text = block.querySelector("code").innerText.replace(/\n$/, "");
      var done = function () {
        btn.textContent = "Copied"; btn.classList.add("done");
        setTimeout(function () { btn.textContent = "Copy"; btn.classList.remove("done"); }, 1600);
      };
      if (navigator.clipboard && navigator.clipboard.writeText) {
        navigator.clipboard.writeText(text).then(done, function () { btn.textContent = "Select and copy by hand"; });
      } else {
        var range = document.createRange(); range.selectNodeContents(block.querySelector("code"));
        var sel = getSelection(); sel.removeAllRanges(); sel.addRange(range);
        try { document.execCommand("copy"); done(); } catch (e) { btn.textContent = "Select and copy by hand"; }
      }
    });
    block.appendChild(btn);
  });

  /* ---- wide tables scroll inside the sheet ---- */
  document.querySelectorAll(".sheet table").forEach(function (t) {
    var w = document.createElement("div"); w.className = "tablewrap";
    t.parentNode.insertBefore(w, t); w.appendChild(t);
  });

  /* ---- sheet index: which plates have been opened ---- */
  var visited = store.get("atlas-visited", {});
  var here = document.body.dataset.plate;
  if (here) { visited[here] = true; store.set("atlas-visited", visited); }
  document.querySelectorAll("[data-plate]").forEach(function (a) {
    if (a !== document.body && visited[a.dataset.plate] && a.getAttribute("aria-current") !== "page") a.classList.add("visited");
  });

  /* ---- legend: sections fill in as they are read ---- */
  var legend = document.querySelector(".legend nav");
  if (legend && here) {
    var links = Array.prototype.slice.call(legend.querySelectorAll(":scope > ul > li > a"));
    var sections = links.map(function (a) { return document.getElementById(decodeURIComponent(a.hash.slice(1))); });
    var read = store.get("atlas-read-" + here, {});
    var paint = function () {
      var line = window.innerHeight * 0.3, current = 0;
      sections.forEach(function (s, i) {
        if (!s) return;
        var r = s.getBoundingClientRect();
        if (r.top <= line) current = i;
        if (r.bottom <= window.innerHeight * 0.6 && !read[s.id]) { read[s.id] = true; store.set("atlas-read-" + here, read); }
      });
      links.forEach(function (a, i) {
        a.classList.toggle("here", i === current);
        a.classList.toggle("read", !!(sections[i] && read[sections[i].id]));
        if (i === current) a.setAttribute("aria-current", "location"); else a.removeAttribute("aria-current");
      });
    };
    var ticking = false;
    addEventListener("scroll", function () {
      if (ticking) return; ticking = true;
      requestAnimationFrame(function () { paint(); ticking = false; });
    }, { passive: true });
    addEventListener("resize", paint);
    paint();
  }

  /* ---- index map: the legend drives the plate ---- */
  var world = document.querySelector("svg.world");
  if (world) {
    var data = JSON.parse(document.getElementById("langs").textContent);
    var dots = Array.prototype.slice.call(world.querySelectorAll(".lang"));
    var frame = world.parentNode;
    var card = frame.querySelector(".cartouche");
    var readout = document.querySelector(".maplegend .readout");
    var buttons = Array.prototype.slice.call(document.querySelectorAll(".maplegend button"));
    var tests = {
      present: function (d) { return d.e === 1; },
      absent: function (d) { return d.e === 0; },
      high: function (d) { return d.el >= 1500; }
    };
    var active = null;
    var fmt = function (n) { return n.toLocaleString("en-GB"); };
    var apply = function (key) {
      active = key;
      var shown = 0, withEj = 0;
      dots.forEach(function (c, i) {
        var keep = !key || tests[key](data[i]);
        c.classList.toggle("dim", !keep);
        if (keep) { shown++; withEj += data[i].e; }
      });
      buttons.forEach(function (b) { b.setAttribute("aria-pressed", String(b.dataset.key === key)); });
      readout.textContent = key
        ? fmt(shown) + " languages shown, " + withEj + " with ejectives (" + Math.round(100 * withEj / shown) + "%)."
        : "Press a row to isolate those languages.";
    };
    buttons.forEach(function (b) {
      b.addEventListener("click", function () { apply(active === b.dataset.key ? null : b.dataset.key); });
    });
    apply(null);

    var hot = null;
    var place = function (i, evt) {
      var d = data[i], box = frame.getBoundingClientRect();
      card.innerHTML = "";
      var b = document.createElement("b"); b.textContent = d.n; card.appendChild(b);
      [d.f + " · " + d.m, fmt(d.el) + " m above sea level",
       (d.e ? "Ejectives" : "No ejectives") + " · " + d.c + " consonants"].forEach(function (t) {
        var s = document.createElement("span"); s.textContent = t; card.appendChild(s);
      });
      card.hidden = false;
      var x = evt.clientX - box.left + 14, y = evt.clientY - box.top + 14;
      if (x + card.offsetWidth > box.width - 8) x = evt.clientX - box.left - card.offsetWidth - 14;
      if (y + card.offsetHeight > box.height - 8) y = evt.clientY - box.top - card.offsetHeight - 14;
      card.style.left = Math.max(8, x) + "px"; card.style.top = Math.max(8, y) + "px";
    };
    var clear = function () { if (hot !== null) dots[hot].classList.remove("hot"); hot = null; card.hidden = true; };
    world.addEventListener("pointermove", function (evt) {
      var pt = world.createSVGPoint(); pt.x = evt.clientX; pt.y = evt.clientY;
      var p = pt.matrixTransform(world.getScreenCTM().inverse());
      var scale = world.viewBox.baseVal.width / world.getBoundingClientRect().width;
      var reach = Math.pow(16 * scale, 2), best = null;
      for (var i = 0; i < data.length; i++) {
        if (active && !tests[active](data[i])) continue;
        var dx = data[i].x - p.x, dy = data[i].y - p.y, q = dx * dx + dy * dy;
        if (q < reach) { reach = q; best = i; }
      }
      if (best === null) { clear(); return; }
      if (hot !== best) { if (hot !== null) dots[hot].classList.remove("hot"); hot = best; dots[best].classList.add("hot"); }
      place(best, evt);
    });
    world.addEventListener("pointerleave", clear);
  }
})();
