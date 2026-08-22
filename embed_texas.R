# embed_texas.R -- Austin vs Texas: slope chart of the two top-20s plus the
# biggest rank divergences. Rank-based only: Texas publishes a top 100 per sex
# per year and no statewide birth total, so no rate or share can be computed.

texas_css <- function() r"---(
  .gv .hd{display:flex;flex-wrap:wrap;gap:10px 16px;align-items:center;
    justify-content:space-between;margin:14px 0 6px}
  .gv .lede{color:var(--text-secondary);font-size:13.5px;margin:2px 0 12px;max-width:78ch}
  .gv .lede b{color:var(--text-primary);font-variant-numeric:tabular-nums}
  .gv .vsgrid{display:grid;grid-template-columns:minmax(0,1.05fr) minmax(0,1fr);gap:20px;
    align-items:start}
  @media (max-width:820px){.gv .vsgrid{grid-template-columns:minmax(0,1fr);gap:16px}}
  .gv .slopewrap{padding:10px 8px 6px;overflow:hidden}
  .gv svg.slope{display:block;width:100%;height:auto}
  .gv svg.slope text{fill:var(--text-primary)}
  .gv svg.slope text.rk{fill:var(--text-muted);font-variant-numeric:tabular-nums}
  .gv svg.slope text.colhd{letter-spacing:.07em;fill:var(--text-muted);font-weight:600}
  .gv svg.slope line.link{stroke-width:2;fill:none;opacity:.75}
  .gv .gaps{padding:12px 14px 14px}
  .gv .gaps + .gaps{margin-top:12px}
  .gv .gaps h3{font-size:11px;letter-spacing:.07em;text-transform:uppercase;
    color:var(--text-muted);margin:0 0 6px;font-weight:600}
  .gv .gaps table{width:100%;border-collapse:collapse;font-size:13px;
    font-variant-numeric:tabular-nums}
  .gv .gaps td{padding:5px 0;border-top:1px solid var(--line);vertical-align:baseline}
  .gv .gaps td.nm{color:var(--text-primary)}
  .gv .gaps td.rk{text-align:right;color:var(--text-secondary);white-space:nowrap;
    font-size:12px;padding-left:8px}
  .gv .gaps td.d{text-align:right;font-weight:600;width:48px;padding-left:8px}
  .gv .gaps .up{color:var(--div-hi)} .gv .gaps .dn{color:var(--div-lo)}
  .gv .vsnote{color:var(--text-muted);font-size:11.5px;margin:12px 0 0;line-height:1.65;
    max-width:88ch}
  @media (max-width:560px){
    .gv .hd{gap:8px} .gv .seg{width:100%} .gv .seg button{flex:1 1 0}
    .gv .gaps td.rk{font-size:11.5px}
  }
)---"

texas_body <- function() paste0(r"---(
<h1>How Austin baby names compare with the rest of the state</h1>
<p class="standfirst">This chart compares Austin and statewide baby-name rankings for 2024. Statewide data comes from the Social Security Administration.</p>

<div class="hd">
  <div class="ctl"><span id="lb-vs" class="sr">Sex</span>
    <div class="seg" id="vssex" role="group" aria-labelledby="lb-vs">
      <button data-vssex="FEMALE" aria-pressed="true">Girls</button>
      <button data-vssex="MALE" aria-pressed="false">Boys</button>
    </div></div>
</div>
<p class="lede" id="vslede"></p>

<div class="vsgrid">
  <div class="card slopewrap">
    <svg class="slope" id="slope" role="img" aria-labelledby="slopecap"></svg>
  </div>
  <div>
    <div class="card gaps"><h3>More popular in Austin</h3>
      <table><tbody id="aufav"></tbody></table></div>
    <div class="card gaps"><h3>More popular across Texas</h3>
      <table><tbody id="txfav"></tbody></table></div>
    <div class="card gaps"><h3>Popular Austin names absent from Texas' top 100</h3>
      <table><tbody id="txmiss"></tbody></table></div>
  </div>
</div>
<p class="sr" id="slopecap"></p>
<p class="vsnote" id="vsnote"></p>
<p class="credit">Source: City of Austin new-resident-name records; Social Security Administration.</p>
)---")

texas_js <- function() r"---(
const NAMES = DATA.names;
let vsSex = "FEMALE";
const slope = document.getElementById("slope");
const wrap  = slope.parentNode;

document.getElementById("vssex").onclick = e => {
  const b = e.target.closest("button"); if(!b) return;
  vsSex = b.dataset.vssex;
  [...e.currentTarget.querySelectorAll("button")].forEach(x =>
    x.setAttribute("aria-pressed", String(x.dataset.vssex === vsSex)));
  drawVs();
};

function gapRows(id, set, dir){
  const el = document.getElementById(id);
  if(!set.i.length){
    el.innerHTML = `<tr><td class="nm" style="color:var(--text-muted)">Nothing clears the cut-off</td></tr>`;
    return;
  }
  el.innerHTML = set.i.map((idx,k) => {
    const d  = set.d ? set.d[k] : null;
    const rk = set.tx ? `#${set.au[k]} Austin · #${set.tx[k]} Texas`
                      : `#${set.au[k]} Austin · ${set.n[k]} babies`;
    return `<tr><td class="nm">${esc(NAMES[idx])}</td><td class="rk">${rk}</td>` +
      (d === null ? `<td class="d"></td>`
                  : `<td class="d ${dir}">${d > 0 ? "+" : ""}${d}</td>`) + `</tr>`;
  }).join("");
}

// The slope chart is drawn in CSS-PIXEL space -- the viewBox is set from the
// measured container width, so a 12.5px label is always 12.5px. A fixed viewBox
// scaled by width:100% would shrink the text to ~7px on a phone.
function drawVs(){
  const c = DATA.cmp[vsSex];
  if(!c) return;
  const kids = vsSex === "FEMALE" ? "girls" : "boys";

  document.getElementById("vslede").textContent =
    `The chart below shows how the most popular Austin ${kids}' names in ${DATA.year} compare to the most popular names statewide.`;

  const W = Math.max(260, Math.round(wrap.clientWidth - 18));
  const narrow = W < 460;
  const N   = Math.min(narrow ? 12 : 20, c.au.i.length);
  const FS  = narrow ? 12 : 13;
  const ROW = narrow ? 26 : 25;
  const TOP = 26, H = TOP + N*ROW + 8;
  // The centre gutter carries only the connecting lines. Each rank number sits
  // inside its own column -- putting the Texas rank in the gutter collided it
  // with the right-aligned Austin name.
  const rkW = narrow ? 22 : 26;
  const xL  = Math.round(W * 0.44);      // right edge of the Austin name
  const xR  = Math.round(W * 0.56);      // left edge of the Texas rank

  slope.setAttribute("viewBox", `0 0 ${W} ${H}`);
  slope.setAttribute("font-size", FS);
  const posR = new Map(c.tx.i.slice(0,N).map((idx,k) => [idx, k]));
  const yOf = k => TOP + k*ROW;

  let s = `<text class="colhd" font-size="${FS-2}" x="${xL}" y="13" text-anchor="end">AUSTIN</text>` +
          `<text class="colhd" font-size="${FS-2}" x="${xR}" y="13">TEXAS</text>`;
  c.au.i.slice(0,N).forEach((idx,k) => {
    if(!posR.has(idx)) return;
    const k2 = posR.get(idx), dy = k2 - k;
    const col = dy > 0 ? css("--div-hi") : dy < 0 ? css("--div-lo") : css("--text-muted");
    s += `<line class="link" x1="${xL+7}" y1="${yOf(k)-4}" x2="${xR-7}" y2="${yOf(k2)-4}" stroke="${col}"/>`;
  });
  c.au.i.slice(0,N).forEach((idx,k) => {
    s += `<text class="rk" font-size="${FS-1.5}" x="2" y="${yOf(k)}">${c.au.rank[k]}</text>` +
         `<text x="${xL}" y="${yOf(k)}" text-anchor="end">${esc(NAMES[idx])}</text>`;
  });
  c.tx.i.slice(0,N).forEach((idx,k) => {
    s += `<text class="rk" font-size="${FS-1.5}" x="${xR}" y="${yOf(k)}">${c.tx.rank[k]}</text>` +
         `<text x="${xR+rkW}" y="${yOf(k)}">${esc(NAMES[idx])}</text>`;
  });
  slope.innerHTML = s;
  document.getElementById("slopecap").textContent =
    `Austin's top ${N} ${kids}' names for ${DATA.year} beside the Texas top ${N}, joined where a name appears in both.`;

  gapRows("aufav",  c.auFav,   "up");
  gapRows("txfav",  c.txFav,   "dn");
  gapRows("txmiss", c.missing, "");

  document.getElementById("vsnote").textContent =
    `Gaps are shown only for names in both ${DATA.year} top 100s with at least 25 Austin births. Texas publishes ` +
    "no rank below its top 100, so a missing name is unranked, not 101.";
}

// drawVs() measures wrap.clientWidth itself, so the first render is already
// correct for the current width; these are only redraw triggers. The observer
// is retained because an unreferenced one can be collected in some engines.
const slopeRO = new ResizeObserver(() => drawVs());
slopeRO.observe(wrap);
window.__gvSlopeRO = slopeRO;
addEventListener("resize", () => drawVs());
matchMedia("(prefers-color-scheme: dark)").addEventListener("change", () => drawVs());
drawVs();

)---"
