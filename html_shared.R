# html_shared.R -- pieces every Grove embed reuses: design tokens, base CSS,
# JS helpers, and the iframe auto-height responder.
#
# Palette values are the documented reference instance (dataviz skill,
# references/palette.md). Only the first three categorical slots are used:
# a bubble chart uses the all-pairs pairlist, where slots 1-3 are the set
# validated in both light and dark (CVD dE 9.2 light / 9.4 dark).

STANDFIRST <- "2017–2024 · the most recent data available from Austin Public Health"
CREDIT     <- "Source: City of Austin new-resident-name records; Texas statewide top-100 name lists."

# All CSS lives under .gv so pasting an embed inline cannot collide with the
# host page's styles. Tokens are declared on :root so they resolve for the
# scoped block and for getComputedStyle() reads in JS.
shared_css <- function() r"---(
  :root{
    color-scheme: light;
    --surface-1:#fcfcfb; --surface-2:#f4f3f0; --surface-3:#eceae6;
    --line:#dedcd6;
    --text-primary:#0b0b0b; --text-secondary:#52514e; --text-muted:#7a7873;
    --s1:#2a78d6; --s2:#eb6834; --s3:#1baf7a;
    --div-lo:#2a78d6; --div-mid:#e6e4df; --div-hi:#e34948;
    --seq-1:#cde2fb; --seq-2:#9ec5f4; --seq-3:#6da7ec;
    --seq-4:#3987e5; --seq-5:#2a78d6; --seq-6:#256abf; --seq-7:#184f95;
    --focus:#0b0b0b;
  }
  @media (prefers-color-scheme: dark){
    :root:where(:not([data-theme="light"])){
      color-scheme: dark;
      --surface-1:#1a1a19; --surface-2:#232322; --surface-3:#2d2d2b;
      --line:#3a3a37;
      --text-primary:#ffffff; --text-secondary:#c3c2b7; --text-muted:#8f8e86;
      --s1:#3987e5; --s2:#d95926; --s3:#199e70;
      --div-lo:#3987e5; --div-mid:#383835; --div-hi:#e66767;
      --seq-1:#184f95; --seq-2:#1c5cab; --seq-3:#256abf;
      --seq-4:#2a78d6; --seq-5:#3987e5; --seq-6:#5598e7; --seq-7:#86b6ef;
      --focus:#ffffff;
    }
  }
  html,body{margin:0;padding:0;background:transparent}
  .gv *{box-sizing:border-box}
  .gv{
    color:var(--text-primary); background:transparent;
    font:15px/1.5 ui-sans-serif,-apple-system,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;
    -webkit-font-smoothing:antialiased; -webkit-text-size-adjust:100%;
    padding:14px 14px 18px; max-width:1360px; margin:0 auto;
  }
  @media (max-width:560px){ .gv{padding:10px 10px 14px} }
  .gv h1{font-size:21px;letter-spacing:-.02em;margin:0 0 3px;line-height:1.2}
  .gv .standfirst{margin:0;color:var(--text-secondary);font-size:13px}
  .gv .standfirst b{color:var(--text-primary);font-variant-numeric:tabular-nums}
  .gv .credit{margin:14px 0 0;color:var(--text-muted);font-size:11.5px;line-height:1.6}
  .gv .card{background:var(--surface-2);border:1px solid var(--line);border-radius:12px}
  .gv .sr{position:absolute;width:1px;height:1px;overflow:hidden;
    clip:rect(0 0 0 0);white-space:nowrap}

  /* segmented control -- shared by every embed's toggles */
  .gv .ctl{display:flex;flex-direction:column;gap:6px;min-width:0}
  .gv .ctl > span,.gv .ctl > label{font-size:11px;letter-spacing:.07em;
    text-transform:uppercase;color:var(--text-muted)}
  .gv .seg{display:flex;flex-wrap:wrap;gap:3px;background:var(--surface-3);
    padding:3px;border-radius:9px}
  .gv .seg button{
    border:0;background:transparent;color:var(--text-secondary);font:inherit;font-size:13px;
    padding:7px 12px;border-radius:6px;cursor:pointer;font-variant-numeric:tabular-nums;
    min-height:38px;                      /* comfortable touch target */
  }
  .gv .seg button:hover{color:var(--text-primary)}
  .gv .seg button[aria-pressed="true"]{background:var(--surface-1);color:var(--text-primary);
    font-weight:600;box-shadow:0 1px 2px rgba(0,0,0,.10)}
  .gv .seg button:focus-visible{outline:2px solid var(--focus);outline-offset:1px}
)---"

# Colour maths + query folding. Shared because both the bubble and the Texas
# embed read tokens and mix diverging ramps at runtime.
shared_js <- function() r"---(
const css = n => getComputedStyle(document.documentElement).getPropertyValue(n).trim();
function hex2rgb(h){ h=h.replace("#",""); if(h.length===3) h=h.split("").map(c=>c+c).join("");
  return [parseInt(h.slice(0,2),16),parseInt(h.slice(2,4),16),parseInt(h.slice(4,6),16)]; }
function mix(a,b,t){ const A=hex2rgb(a),B=hex2rgb(b);
  return `rgb(${Math.round(A[0]+(B[0]-A[0])*t)},${Math.round(A[1]+(B[1]-A[1])*t)},${Math.round(A[2]+(B[2]-A[2])*t)})`; }
// Diverging: two poles through a neutral midpoint, never a hue at the middle.
function diverge(v, cap){
  const t = Math.max(-1, Math.min(1, (v||0)/cap));
  return t >= 0 ? mix(css("--div-mid"), css("--div-hi"), t)
                : mix(css("--div-mid"), css("--div-lo"), -t);
}
// Relative luminance, so a label can pick ink that survives whatever fill it
// lands on -- the sequential and diverging ramps both run pale to dark.
function lum(col){
  let r,g,b;
  if(col[0] === "#"){ [r,g,b] = hex2rgb(col); }
  else { const m = col.match(/\d+/g); r=+m[0]; g=+m[1]; b=+m[2]; }
  const f = v => { v/=255; return v <= 0.03928 ? v/12.92 : Math.pow((v+0.055)/1.055, 2.4); };
  return 0.2126*f(r) + 0.7152*f(g) + 0.0722*f(b);
}
// Accent- and punctuation-insensitive: typing "jose" must reach JOSÉ.
const fold = s => s.normalize("NFD").replace(/[̀-ͯ]/g,"").toLowerCase().trim();
const esc  = s => String(s).replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;");

)---"

# Appended AFTER the embed's own script, so the first measurement happens once
# the chart has actually been drawn rather than against an empty container.
height_js <- function() r"---(
// --- responsive embed height -------------------------------------------
// Same contract as the food-inspection embeds: the child broadcasts its own
// height and the host page sizes the iframe. Body, not documentElement --
// <html> stretches to the viewport when the parent oversizes the frame, which
// locks the reported height at whatever the parent last set.
function postHeight(){
  var b = document.body, cs = getComputedStyle(b);
  var h = b.getBoundingClientRect().height +
          parseFloat(cs.marginTop || 0) + parseFloat(cs.marginBottom || 0);
  window.parent.postMessage({
    embed: "austin-baby-names",
    slug: (location.pathname.split("/").pop() || "").replace(".html", ""),
    height: Math.ceil(h)
  }, "*");
}
postHeight();
addEventListener("load", postHeight);
addEventListener("resize", postHeight);
// Fonts landing late change the wrap points and therefore the height.
if (document.fonts && document.fonts.ready) document.fonts.ready.then(postHeight);
if (window.ResizeObserver) new ResizeObserver(postHeight).observe(document.body);
)---"

# Assemble one standalone embed file.
embed_html <- function(id, title, body, script, data_json = NULL, extra_css = "") {
  paste0(
    '<!doctype html>\n<html lang="en">\n<head>\n<meta charset="utf-8">\n',
    '<meta name="viewport" content="width=device-width, initial-scale=1">\n',
    "<title>", title, "</title>\n<style>",
    shared_css(), extra_css,
    "\n</style>\n</head>\n<body>\n<div class=\"gv\" id=\"root\">\n",
    body,
    "\n</div>\n<script>\n\"use strict\";\n",
    if (is.null(data_json)) "" else paste0("const DATA = ", data_json, ";\n"),
    shared_js(),
    script,
    height_js(),
    "\n</script>\n</body>\n</html>\n"
  )
}
