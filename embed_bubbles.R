# embed_bubbles.R -- packed bubble chart with search and a per-name detail panel.

bubbles_css <- function() r"---(
  .gv .controls{display:flex;flex-wrap:wrap;gap:12px 20px;align-items:flex-end;
    margin:14px 0 12px;padding:12px 14px;background:var(--surface-2);
    border:1px solid var(--line);border-radius:12px}
  .gv select,.gv input[type=search]{font:inherit;font-size:15px;padding:9px 11px;
    border-radius:8px;border:1px solid var(--line);background:var(--surface-1);
    color:var(--text-primary);min-height:40px;width:100%}
  .gv .searchwrap{position:relative;flex:1 1 230px;min-width:0}
  .gv .colorwrap{flex:0 1 210px;min-width:0}
  @media (max-width:560px){
    .gv .controls{gap:10px 12px;padding:10px}
    .gv .controls .ctl{flex:1 1 100%}
    .gv .seg{width:100%} .gv .seg button{flex:1 1 0;padding:7px 6px}
  }
  .gv #results{position:absolute;z-index:40;top:calc(100% + 4px);left:0;width:100%;
    max-height:270px;overflow:auto;background:var(--surface-1);border:1px solid var(--line);
    border-radius:8px;box-shadow:0 8px 24px rgba(0,0,0,.16);display:none;padding:4px}
  .gv #results.on{display:block}
  .gv #results button{display:flex;justify-content:space-between;gap:10px;width:100%;
    text-align:left;border:0;background:transparent;color:var(--text-primary);font:inherit;
    font-size:15px;padding:9px;border-radius:6px;cursor:pointer;min-height:40px}
  .gv #results button:hover,.gv #results button.hi{background:var(--surface-3)}
  .gv #results .m{color:var(--text-muted);font-size:12px;
    font-variant-numeric:tabular-nums;flex:none}

  .gv .layout{display:grid;grid-template-columns:minmax(0,1fr) 320px;gap:16px;align-items:start}
  @media (max-width:940px){.gv .layout{grid-template-columns:minmax(0,1fr)}}
  .gv .chartcard{padding:6px;overflow:hidden}
  .gv svg.bubbles{display:block;width:100%;height:auto;touch-action:manipulation}
  /* transform is driven per-frame by the pointer loop, so it must not also be
     transitioned here -- only opacity (the search dim) animates in CSS */
  .gv svg.bubbles circle{stroke:var(--surface-2);stroke-width:2;transition:opacity .18s}
  .gv svg.bubbles g.b{cursor:pointer;outline:none}
  .gv svg.bubbles g.b text{pointer-events:none;text-anchor:middle;font-weight:600;
    paint-order:stroke;stroke-width:2.4px}
  .gv svg.bubbles g.b .cnt{font-weight:500;opacity:.9}
  .gv svg.bubbles g.b:hover circle{stroke:var(--text-primary);stroke-width:2.5}
  .gv svg.bubbles g.b:focus-visible circle{stroke:var(--focus);stroke-width:3.5}
  .gv svg.bubbles g.b.sel circle{stroke:var(--text-primary);stroke-width:3.5}
  .gv svg.bubbles.dim g.b:not(.hit){opacity:.16}
  @media (prefers-reduced-motion: reduce){ .gv svg.bubbles circle{transition:none} }

  .gv .legend{display:flex;flex-wrap:wrap;gap:6px 16px;align-items:center;
    padding:7px 8px 3px;font-size:12.5px;color:var(--text-secondary)}
  .gv .legend .k{display:flex;align-items:center;gap:6px}
  .gv .legend .sw{width:11px;height:11px;border-radius:3px;flex:none}
  .gv .legend .ramp{display:flex;height:11px;border-radius:3px;overflow:hidden;width:110px}
  .gv .legend .ramp i{flex:1}

  .gv .panel{padding:15px 15px 17px;position:sticky;top:12px}
  @media (max-width:940px){.gv .panel{position:static}}
  .gv .panel .ph{color:var(--text-muted);font-size:13.5px;line-height:1.6;margin:0}
  .gv .panel h2{margin:0;font-size:22px;letter-spacing:-.01em}
  .gv .panel .sub{color:var(--text-secondary);font-size:13px;margin:3px 0 12px;
    font-variant-numeric:tabular-nums}
  .gv .stats{display:grid;grid-template-columns:1fr 1fr;gap:8px;margin:0 0 12px}
  .gv .stat{background:var(--surface-1);border:1px solid var(--line);border-radius:8px;
    padding:8px 10px}
  .gv .stat b{display:block;font-size:17px;font-variant-numeric:tabular-nums}
  .gv .stat span{font-size:11px;color:var(--text-muted);letter-spacing:.04em;
    text-transform:uppercase}
  .gv .panel h3{font-size:11px;letter-spacing:.07em;text-transform:uppercase;
    color:var(--text-muted);margin:14px 0 6px;font-weight:600}
  .gv table.mini{width:100%;border-collapse:collapse;font-size:12.5px;
    font-variant-numeric:tabular-nums}
  .gv table.mini th{text-align:right;font-weight:600;color:var(--text-muted);
    padding:3px 0;font-size:11px}
  .gv table.mini th:first-child{text-align:left}
  .gv table.mini td{text-align:right;padding:3px 0;border-top:1px solid var(--line)}
  .gv table.mini td:first-child{text-align:left;color:var(--text-secondary)}
  .gv .na{color:var(--text-muted)}
  .gv .variants{font-size:12.5px;color:var(--text-secondary);line-height:1.7;
    word-break:break-word}
)---"

bubbles_body <- function() paste0(r"---(
<h1>Austin baby names</h1>
<p class="standfirst">)---", STANDFIRST, r"---( · bubble area is the number of babies given that name</p>

<div class="controls">
  <div class="ctl"><span id="lb-sex">Show</span>
    <div class="seg" id="sexes" role="group" aria-labelledby="lb-sex">
      <button data-sex="ALL" aria-pressed="true">Everyone</button>
      <button data-sex="FEMALE" aria-pressed="false">Girls</button>
      <button data-sex="MALE" aria-pressed="false">Boys</button>
    </div></div>
  <div class="ctl colorwrap"><label for="colorby">Colour by</label>
    <select id="colorby">
      <option value="sex">Girls / boys / both</option>
      <option value="pop">Popularity</option>
      <option value="trend">Trend over time</option>
      <option value="tx">Austin vs. Texas</option>
    </select></div>
  <div class="ctl searchwrap"><label for="q">Search every name</label>
    <input type="search" id="q" placeholder="try zoe, jose, aurora…" autocomplete="off"
           role="combobox" aria-expanded="false" aria-controls="results" aria-autocomplete="list">
    <div id="results" role="listbox" aria-label="Search results"></div></div>
</div>

<div class="layout">
  <div>
    <div class="card chartcard" id="chartcard">
      <div class="legend" id="legend"></div>
      <svg class="bubbles" id="svg" role="application"
           aria-label="Packed bubble chart of Austin baby names. Use arrow keys to move between names and Enter to open details."></svg>
    </div>
    <p class="sr" id="live" aria-live="polite"></p>
    <noscript><p class="credit">This chart needs JavaScript. The full top-100 list is
      published as a separate table.</p></noscript>
  </div>
  <aside class="card panel" id="panel">
    <p class="ph">Tap or click any bubble — or search for a name — to see how it moved
      across 2017–2024, how Austin compares with Texas, and which spellings were merged into it.</p>
  </aside>
</div>
<p class="credit">)---", CREDIT, r"---(</p>
)---")

bubbles_js <- function() r"---(
const YEARS = DATA.meta.years, NY = YEARS.length;
const NAMES = DATA.names, LC = NAMES.map(s => s.toLowerCase());
const VBW = DATA.meta.vbw, VBH = DATA.meta.vbh;

// ---- index the columnar series into per-name records -------------------
const SER = new Map();
function slot(i){
  let e = SER.get(i);
  if(!e){ e = {f:new Array(NY).fill(0), m:new Array(NY).fill(0),
               rf:new Array(NY).fill(0), rm:new Array(NY).fill(0),
               tf:new Array(NY).fill(0), tm:new Array(NY).fill(0)};
          SER.set(i,e); }
  return e;
}
(function(){
  const S = DATA.series;
  for(let k=0;k<S.i.length;k++){
    const e = slot(S.i[k]);
    if(S.s[k]===0){ e.f[S.y[k]] = S.n[k]; e.rf[S.y[k]] = S.rk[k]; }
    else          { e.m[S.y[k]] = S.n[k]; e.rm[S.y[k]] = S.rk[k]; }
  }
  const T = DATA.tx;
  for(let k=0;k<T.i.length;k++){
    const e = slot(T.i[k]);
    if(T.s[k]===0) e.tf[T.y[k]] = T.rk[k]; else e.tm[T.y[k]] = T.rk[k];
  }
})();
const totalOf = i => { const e=SER.get(i); if(!e) return 0;
  let t=0; for(let y=0;y<NY;y++) t += e.f[y]+e.m[y]; return t; };

// ---- state -------------------------------------------------------------
// size: "d" (desktop) or "m" (mobile). Chosen from the CONTAINER width, not the
// viewport -- an embed can sit in a narrow column on a wide screen.
const state = { sex:"ALL", size:"d", color:"sex", sel:null, active:0, hits:null };
const svg   = document.getElementById("svg");
const panel = document.getElementById("panel");
const live  = document.getElementById("live");
const card  = document.getElementById("chartcard");
const NS    = "http://www.w3.org/2000/svg";
let MARKS = [];

const viewOf = () => DATA.views[state.size + "|" + state.sex];

function colorOf(d, k){
  switch(state.color){
    case "sex": {
      const ms = d.ms[k];
      if(ms === null) return css("--s3");
      return ms <= 0.15 ? css("--s2") : ms >= 0.85 ? css("--s1") : css("--s3");
    }
    case "pop": {
      const s = [1,2,3,4,5,6,7].map(x => css("--seq-"+x));
      const q = Math.min(6, Math.floor((d.rank[k]-1) / Math.max(1, d.rank.length/7)));
      return s[6-q];
    }
    case "trend": return d.tr[k]===null ? css("--div-mid") : diverge(d.tr[k], 0.6);
    case "tx":    return d.td[k]===null ? css("--surface-3") : diverge(d.td[k], 45);
  }
}
const SEQ = () => [1,2,3,4,5,6,7].map(k => css("--seq-"+k));
const LEGENDS = {
  sex:  () => `<span class="k"><i class="sw" style="background:${css("--s2")}"></i>Mostly girls</span>
               <span class="k"><i class="sw" style="background:${css("--s1")}"></i>Mostly boys</span>
               <span class="k"><i class="sw" style="background:${css("--s3")}"></i>Used for both</span>`,
  // seq-1 is the step nearest the surface and seq-7 the far end, in BOTH modes
  // (the dark ramp is re-stepped, not flipped), so the legend runs in natural order.
  pop:  () => `<span class="k">Less common</span>
               <span class="ramp">${SEQ().map(c=>`<i style="background:${c}"></i>`).join("")}</span>
               <span class="k">More common</span>`,
  trend:() => `<span class="k">Falling</span>
               <span class="ramp"><i style="background:${css("--div-lo")}"></i><i style="background:${mix(css("--div-lo"),css("--div-mid"),.6)}"></i><i style="background:${css("--div-mid")}"></i><i style="background:${mix(css("--div-mid"),css("--div-hi"),.6)}"></i><i style="background:${css("--div-hi")}"></i></span>
               <span class="k">Rising</span>`,
  tx:   () => `<span class="k">Texas favours it</span>
               <span class="ramp"><i style="background:${css("--div-lo")}"></i><i style="background:${mix(css("--div-lo"),css("--div-mid"),.6)}"></i><i style="background:${css("--div-mid")}"></i><i style="background:${mix(css("--div-mid"),css("--div-hi"),.6)}"></i><i style="background:${css("--div-hi")}"></i></span>
               <span class="k">Austin favours it</span>
               <span class="k" style="color:var(--text-muted)">· grey = outside the Texas top 100</span>`
};

document.getElementById("sexes").onclick = e => {
  const b = e.target.closest("button"); if(!b) return;
  state.sex = b.dataset.sex;
  [...e.currentTarget.querySelectorAll("button")].forEach(x =>
    x.setAttribute("aria-pressed", String(x.dataset.sex === state.sex)));
  draw();
};
document.getElementById("colorby").onchange = e => { state.color = e.target.value; draw(); };

// ---- draw --------------------------------------------------------------
// Tier is measured at draw time rather than only in the ResizeObserver, so the
// very first render is right for the current width even if the observer never
// fires (some embed hosts never deliver resize into a child frame).
const tierFor = w => (w < 560 ? "m" : "d");

function draw(){
  state.size = tierFor(card.clientWidth || document.documentElement.clientWidth || 1024);
  const d = viewOf();
  svg.setAttribute("viewBox", `0 0 ${VBW} ${VBH}`);
  svg.textContent = "";
  MARKS = [];
  document.getElementById("legend").innerHTML = LEGENDS[state.color]();

  // Transparent backdrop so pointermove fires over the gaps between bubbles.
  // First in document order, so every circle draws on top and keeps its clicks.
  const backdrop = document.createElementNS(NS, "rect");
  backdrop.setAttribute("width", VBW); backdrop.setAttribute("height", VBH);
  backdrop.setAttribute("fill", "transparent");
  svg.appendChild(backdrop);

  d.i.forEach((nameIdx, k) => {
    const g = document.createElementNS(NS, "g");
    g.setAttribute("class", "b");
    g.setAttribute("role", "button");
    g.setAttribute("tabindex", k === state.active ? "0" : "-1");
    g.dataset.i = nameIdx; g.dataset.k = k;
    g.setAttribute("aria-label", `${NAMES[nameIdx]}, ${d.n[k]} babies, ranked ${d.rank[k]}`);

    const fill = colorOf(d, k);
    const pale = lum(fill) > 0.42;
    const ink  = pale ? "#111111" : "#ffffff";
    const halo = pale ? "rgba(255,255,255,.55)" : "rgba(0,0,0,.28)";

    const c = document.createElementNS(NS, "circle");
    c.setAttribute("cx", d.x[k]); c.setAttribute("cy", d.y[k]); c.setAttribute("r", d.r[k]);
    c.setAttribute("fill", fill);
    g.appendChild(c);

    // small bubbles lose their direct label, so every bubble carries a tooltip
    const tip = document.createElementNS(NS, "title");
    tip.textContent = `${NAMES[nameIdx]} — ${d.n[k]} babies, ranked #${d.rank[k]}`;
    g.appendChild(tip);

    // Direct-label selectively: shrink to fit, then drop the label rather than
    // let it spill past the rim.
    const r = d.r[k], label = NAMES[nameIdx], AVG = 0.54;
    const fits = f => label.length * f * AVG <= r * 1.80;
    let fs = Math.min(19, r * 0.42);
    while(fs > 8.5 && !fits(fs)) fs -= 0.5;
    if(r > 14 && fits(fs)){
      const twoLine = r > 30;
      const t = document.createElementNS(NS, "text");
      t.setAttribute("x", d.x[k]); t.setAttribute("y", d.y[k] + (twoLine ? -1 : 4));
      t.setAttribute("font-size", fs.toFixed(1));
      t.setAttribute("fill", ink); t.setAttribute("stroke", halo);
      t.textContent = label;
      g.appendChild(t);
      if(twoLine){
        const t2 = document.createElementNS(NS, "text");
        t2.setAttribute("class", "cnt");
        t2.setAttribute("x", d.x[k]); t2.setAttribute("y", d.y[k] + 14);
        t2.setAttribute("font-size", Math.max(8, Math.min(13, r * 0.26)));
        t2.setAttribute("fill", ink); t2.setAttribute("stroke", halo);
        t2.textContent = d.n[k];
        g.appendChild(t2);
      }
    }
    g.addEventListener("click", () => { state.active = k; select(nameIdx); });
    g.addEventListener("keydown", ev => {
      if(ev.key === "Enter" || ev.key === " "){ ev.preventDefault(); select(nameIdx); }
      else if(ev.key.startsWith("Arrow")){
        ev.preventDefault();
        const step = (ev.key === "ArrowRight" || ev.key === "ArrowDown") ? 1 : -1;
        state.active = (k + step + d.i.length) % d.i.length;
        const nx = svg.querySelector(`g.b[data-k="${state.active}"]`);
        if(nx){ [...svg.querySelectorAll("g.b")].forEach(x => x.setAttribute("tabindex","-1"));
                nx.setAttribute("tabindex","0"); nx.focus(); }
      }
    });
    MARKS.push({ g, bx: d.x[k], by: d.y[k], x: d.x[k], y: d.y[k],
                 vx: 0, vy: 0, r: d.r[k], m: d.r[k] * d.r[k] });
    svg.appendChild(g);
  });
  applyHighlight();
  if(state.sel !== null) markSelected();
}
function markSelected(){
  svg.querySelectorAll("g.b").forEach(g => g.classList.toggle("sel", +g.dataset.i === state.sel));
}
function applyHighlight(){
  if(!state.hits){ svg.classList.remove("dim"); return; }
  svg.classList.add("dim");
  svg.querySelectorAll("g.b").forEach(g => g.classList.toggle("hit", state.hits.has(+g.dataset.i)));
}

// ---- responsive: pick the layout from the CONTAINER width --------------
// Container, not viewport: an embed can sit in a narrow column on a wide screen.
const reflow = () => { if(tierFor(card.clientWidth) !== state.size) draw(); };
const cardRO = new ResizeObserver(reflow);   // retained; see note in autoHeight
cardRO.observe(card);
window.__gvCardRO = cardRO;
addEventListener("resize", reflow);

// ---- pointer physics ---------------------------------------------------
// Each bubble is a spring-damped mass anchored to its packed position. The
// cursor shoves them; a separation pass then makes them bounce off each other,
// so a shove propagates through the cluster instead of overlapping it.
const REDUCED = matchMedia("(prefers-reduced-motion: reduce)");
const COARSE  = matchMedia("(hover: none)");   // touch: no hover to react to
const REACH  = 190,   // cursor influence radius, viewBox units
      PUSH   = 1.9,   // cursor impulse per frame
      SPRING = 0.055, // pull back toward the packed position
      DAMP   = 0.87,  // velocity retained per frame (<1 so it settles)
      SLOP   = 0.35,  // overlap tolerated before separating. The pack already
                      // has rims exactly touching, so demanding any clearance
                      // would make the solver fight the springs forever.
      ITER   = 3;
let ptr = { x:0, y:0, on:false }, raf = null;
const motionOff = () => REDUCED.matches || COARSE.matches;

function toViewBox(ev){
  const b = svg.getBoundingClientRect();
  return { x: (ev.clientX - b.left) / b.width * VBW,
           y: (ev.clientY - b.top)  / b.height * VBH };
}
function tick(){
  const n = MARKS.length;
  for(let k=0;k<n;k++){
    const m = MARKS[k];
    m.vx += (m.bx - m.x) * SPRING;
    m.vy += (m.by - m.y) * SPRING;
    if(ptr.on){
      const dx = m.x - ptr.x, dy = m.y - ptr.y, d = Math.hypot(dx, dy);
      if(d < REACH && d > 0.01){
        // Impulse peaks at mid-range and falls to zero at the cursor itself, so
        // the bubble being aimed at holds still and stays clickable.
        const f = Math.sin(Math.PI * (d / REACH)) * PUSH;
        m.vx += dx/d*f; m.vy += dy/d*f;
      }
    }
    m.vx *= DAMP; m.vy *= DAMP;
    m.x += m.vx;  m.y += m.vy;
  }
  // Separation, mass-weighted by area: Liam barely budges, small names ricochet.
  for(let it=0; it<ITER; it++){
    for(let i=0;i<n;i++){
      const a = MARKS[i];
      for(let j=i+1;j<n;j++){
        const b = MARKS[j];
        const dx = b.x-a.x, dy = b.y-a.y, min = a.r + b.r - SLOP;
        if(dx > min || dx < -min || dy > min || dy < -min) continue;
        const d2 = dx*dx + dy*dy;
        if(d2 >= min*min || d2 < 1e-6) continue;
        const d = Math.sqrt(d2), over = min - d;
        const nx = dx/d, ny = dy/d, tot = a.m + b.m;
        const sa = b.m/tot, sb = a.m/tot;
        a.x -= nx*over*sa; a.y -= ny*over*sa;
        b.x += nx*over*sb; b.y += ny*over*sb;
      }
    }
  }
  let moving = false;
  for(let k=0;k<n;k++){
    const m = MARKS[k];
    const ox = m.x - m.bx, oy = m.y - m.by;
    if(Math.abs(ox) > 0.06 || Math.abs(oy) > 0.06 ||
       Math.abs(m.vx) > 0.03 || Math.abs(m.vy) > 0.03){
      m.g.setAttribute("transform", `translate(${ox.toFixed(2)} ${oy.toFixed(2)})`);
      moving = true;
    } else {
      if(m.g.hasAttribute("transform")) m.g.removeAttribute("transform");
      m.x = m.bx; m.y = m.by; m.vx = 0; m.vy = 0;   // snap, so residue can't accumulate
    }
  }
  raf = (ptr.on || moving) ? requestAnimationFrame(tick) : null;
}
function kick(){ if(raf === null && !motionOff()) raf = requestAnimationFrame(tick); }
svg.addEventListener("pointermove", ev => {
  if(motionOff() || ev.pointerType === "touch") return;
  const p = toViewBox(ev); ptr.x = p.x; ptr.y = p.y; ptr.on = true; kick();
});
svg.addEventListener("pointerleave", () => { ptr.on = false; kick(); });

// ---- detail panel ------------------------------------------------------
function spark(vals){
  const w = 296, h = 46, mx = Math.max(...vals, 1);
  const pts = vals.map((v,i) => [8 + i*(w-16)/(NY-1), h - 6 - (v/mx)*(h-16)]);
  const line = pts.map((p,i) => (i?"L":"M") + p[0].toFixed(1) + " " + p[1].toFixed(1)).join(" ");
  const area = line + ` L${pts[NY-1][0].toFixed(1)} ${h-4} L${pts[0][0].toFixed(1)} ${h-4} Z`;
  const peak = vals.indexOf(mx);
  return `<svg viewBox="0 0 ${w} ${h}" width="100%" height="${h}" aria-hidden="true">
    <path d="${area}" fill="var(--s1)" opacity=".14"/>
    <path d="${line}" fill="none" stroke="var(--s1)" stroke-width="2"
          stroke-linejoin="round" stroke-linecap="round"/>
    <circle cx="${pts[peak][0].toFixed(1)}" cy="${pts[peak][1].toFixed(1)}" r="4"
            fill="var(--s1)" stroke="var(--surface-2)" stroke-width="2"/></svg>`;
}
function select(i){
  state.sel = i;
  const e = SER.get(i) || slot(i);
  const per = YEARS.map((_,y) => e.f[y] + e.m[y]);
  const tot = v => v.reduce((a,b)=>a+b,0);
  const total = tot(per), nf = tot(e.f), nm = tot(e.m);
  const peakY = YEARS[per.indexOf(Math.max(...per))];
  const domF = nf >= nm;
  const auRank = domF ? e.rf : e.rm, txRank = domF ? e.tf : e.tm;
  const lastAu = [...auRank].reverse().find(v => v > 0) || 0;

  const rows = YEARS.map((yr,y) => {
    if(!per[y]) return "";
    const tx = txRank[y] ? "#"+txRank[y]
      : `<span class="na" title="Texas publishes only its top 100">—</span>`;
    return `<tr><td>${yr}</td><td>${per[y]}</td><td>${auRank[y]?"#"+auRank[y]:"—"}</td><td>${tx}</td></tr>`;
  }).join("");
  const v = DATA.vars[String(i)];

  panel.innerHTML = `
    <h2>${esc(NAMES[i])}</h2>
    <p class="sub">${total.toLocaleString("en-US")} babies · 2017–2024${
       nf && nm ? ` · ${nf} girls, ${nm} boys` : ""}</p>
    <div class="stats">
      <div class="stat"><b>${lastAu ? "#"+lastAu : "—"}</b><span>Austin rank, ${YEARS[NY-1]}</span></div>
      <div class="stat"><b>${peakY}</b><span>Peak year</span></div>
    </div>
    <h3>Babies per year</h3>${spark(per)}
    <h3>Rank by year (${domF ? "girls" : "boys"})</h3>
    <table class="mini">
      <thead><tr><th>Year</th><th>Babies</th><th>Austin</th><th>Texas</th></tr></thead>
      <tbody>${rows}</tbody></table>
    ${v ? `<h3>Spellings merged into this name</h3><p class="variants">${esc(v)}</p>` : ""}`;
  markSelected();
  live.textContent = `${NAMES[i]}: ${total} babies between 2017 and 2024.`;
  // on a narrow screen the panel sits below the chart, so bring it into view
  if(state.size === "m") panel.scrollIntoView({block:"nearest", behavior:
    REDUCED.matches ? "auto" : "smooth"});
}

// ---- search ------------------------------------------------------------
const q = document.getElementById("q"), box = document.getElementById("results");
let hiIdx = -1, cur = [];
function runSearch(){
  const term = fold(q.value);
  if(!term){ box.classList.remove("on"); box.innerHTML = "";
    q.setAttribute("aria-expanded","false"); state.hits = null; cur = []; applyHighlight(); return; }
  const starts = [], has = [];
  for(let i=0;i<LC.length;i++){
    const p = LC[i].indexOf(term);
    if(p === 0) starts.push(i); else if(p > 0) has.push(i);
  }
  const byCount = (a,b) => totalOf(b) - totalOf(a);
  starts.sort(byCount); has.sort(byCount);
  cur = starts.concat(has);
  state.hits = new Set(cur); applyHighlight();
  box.innerHTML = cur.slice(0,20).map((i,k) =>
    `<button role="option" data-i="${i}" class="${k===0?"hi":""}">
       <span>${esc(NAMES[i])}</span><span class="m">${totalOf(i).toLocaleString("en-US")}</span></button>`
  ).join("") || `<button role="option" disabled class="m">No name matches “${esc(q.value)}”</button>`;
  hiIdx = 0; box.classList.add("on"); q.setAttribute("aria-expanded","true");
}
q.addEventListener("input", runSearch);
q.addEventListener("keydown", ev => {
  const btns = [...box.querySelectorAll("button[data-i]")];
  if(ev.key === "ArrowDown" || ev.key === "ArrowUp"){
    ev.preventDefault(); if(!btns.length) return;
    hiIdx = (hiIdx + (ev.key === "ArrowDown" ? 1 : -1) + btns.length) % btns.length;
    btns.forEach((b,k) => b.classList.toggle("hi", k === hiIdx));
    btns[hiIdx].scrollIntoView({block:"nearest"});
  } else if(ev.key === "Enter"){ ev.preventDefault(); if(btns[hiIdx]) btns[hiIdx].click(); }
  else if(ev.key === "Escape"){ q.value = ""; runSearch(); }
});
box.addEventListener("click", ev => {
  const b = ev.target.closest("button[data-i]"); if(!b) return;
  select(+b.dataset.i);
  box.classList.remove("on"); q.setAttribute("aria-expanded","false");
});
document.addEventListener("click", ev => {
  if(!ev.target.closest(".searchwrap")){
    box.classList.remove("on"); q.setAttribute("aria-expanded","false");
  }
});

// Fills are baked into each circle at draw time, so a light/dark switch has to
// force a repaint or the previous mode's colours persist.
matchMedia("(prefers-color-scheme: dark)").addEventListener("change", () => draw());
draw();

)---"
