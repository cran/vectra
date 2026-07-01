## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----va-engine, echo = FALSE, results = "asis", eval = TRUE-------------------
cat('
<style>
pre, pre.sourceCode, div.sourceCode { overflow-x: auto; max-width: 100%; }
canvas { display:block; margin: 6px auto; }
</style>
<script>
window.VA = {
  ease:function(t){return t<0.5?2*t*t:1-Math.pow(-2*t+2,2)/2;},
  easeOut:function(t){return 1-Math.pow(1-t,3);},
  lerp:function(a,b,t){return a+(b-a)*t;},
  clamp:function(v,a,b){return Math.max(a,Math.min(b,v));},
  C:{bg:"#0b0d17",grid:"#19203a",green:"#33ff8a",red:"#ff3b6b",cyan:"#3ad7ff",amber:"#ffc24b",purple:"#b388ff",ink:"#d7def7",mut:"#7f8bb5",off:"#1d2540"},
  F:function(sz,bold){return (bold?"bold ":"")+Math.round(sz*1.25)+"px monospace";},
  bg:function(c,w,h){c.fillStyle=this.C.bg;c.fillRect(0,0,w,h);c.strokeStyle=this.C.grid;c.lineWidth=1;for(var gx=26;gx<w;gx+=26){c.beginPath();c.moveTo(gx+0.5,0);c.lineTo(gx+0.5,h);c.stroke();}for(var gy=26;gy<h;gy+=26){c.beginPath();c.moveTo(0,gy+0.5);c.lineTo(w,gy+0.5);c.stroke();}},
  scan:function(c,w,h){c.save();c.globalAlpha=0.07;c.fillStyle="#000";for(var y=0;y<h;y+=3)c.fillRect(0,y,w,1);c.restore();},
  glow:function(c,col,b){c.shadowColor=col;c.shadowBlur=b;},
  noglow:function(c){c.shadowBlur=0;c.shadowColor="transparent";},
  setup:function(id){var cv=document.getElementById(id);if(!cv)return null;var ctx=cv.getContext("2d");var w=cv.width,h=cv.height,d=window.devicePixelRatio||1;cv.width=w*d;cv.height=h*d;cv.style.width=w+"px";cv.style.height="auto";ctx.scale(d,d);return {ctx:ctx,w:w,h:h};},
  run:function(drawAt,period,btnId,key,cvId){var fz=(window.__FREEZE&&(key in window.__FREEZE))?window.__FREEZE[key]:null;if(fz!=null){drawAt(((fz%period)+period)%period);return;}var start=performance.now();function loop(now){var e=(now-start)/1000;if(e<0)e=0;drawAt(e%period);requestAnimationFrame(loop);}requestAnimationFrame(loop);function restart(){start=performance.now();}if(btnId){var b=document.getElementById(btnId);if(b)b.onclick=restart;}if(cvId){var cv=document.getElementById(cvId);if(cv){cv.style.cursor="pointer";cv.title="click to replay";cv.onclick=restart;}}}
};
</script>
')

## ----streaming-memory-anim, echo = FALSE, results = "asis", eval = TRUE-------
body <- "
var s=VA.setup('sm-cv'); if(!s)return; var x=s.ctx,W=s.w,H=s.h,C=VA.C;
var PX=72,PY=58,PW=W-72-36,PH=H-58-66,RAM=0.74;
var PERIOD=10.0;
function draw(tc){VA.bg(x,W,H);
  VA.glow(x,C.cyan,7);x.fillStyle=C.cyan;x.textAlign='left';x.font=VA.F(15,true);x.fillText('MEMORY  AS  ROWS  ARE  PROCESSED',16,28);VA.noglow(x);
  x.strokeStyle=C.mut;x.lineWidth=1;x.beginPath();x.moveTo(PX,PY);x.lineTo(PX,PY+PH);x.lineTo(PX+PW,PY+PH);x.stroke();
  x.fillStyle=C.mut;x.font=VA.F(11);x.textAlign='right';x.fillText('memory',PX-6,PY+10);x.textAlign='center';x.fillText('rows processed',PX+PW/2,PY+PH+24);
  var ry=PY+PH*(1-RAM);x.strokeStyle=C.red;x.setLineDash([6,4]);x.beginPath();x.moveTo(PX,ry);x.lineTo(PX+PW,ry);x.stroke();x.setLineDash([]);x.fillStyle=C.red;x.font=VA.F(11,true);x.textAlign='left';x.fillText('RAM limit',PX+6,ry-6);
  var t=VA.clamp(tc/(PERIOD-1.2),0,1),le=Math.floor(t*PW);
  var redMax=1.16;
  x.strokeStyle=C.red;x.lineWidth=2.5;VA.glow(x,C.red,6);x.beginPath();
  for(var i=0;i<=le;i++){var v=(i/PW)*redMax;x.lineTo(PX+i,PY+PH*(1-Math.min(v,1.02)));}x.stroke();VA.noglow(x);
  var g=0.16;x.strokeStyle=C.green;x.lineWidth=2.5;VA.glow(x,C.green,6);x.beginPath();
  for(var i=0;i<=le;i++){x.lineTo(PX+i,PY+PH*(1-g-0.012*Math.sin(i/13)));}x.stroke();VA.noglow(x);
  var LX=PX+Math.min(le,PW-96),SY=PY+PH*(1-g)-9;
  x.fillStyle=C.green;x.font=VA.F(12,true);x.textAlign='left';x.fillText('streaming',LX,SY);
  var redv=t*redMax,ly=PY+PH*(1-Math.min(redv,1.02));x.fillStyle=C.red;x.fillText('in memory',LX,Math.min(Math.max(PY+13,ly-9),SY-20));
  x.fillStyle=C.mut;x.font=VA.F(11);x.textAlign='center';x.fillText('streaming keeps the footprint flat as the file grows',W/2,H-14);
  VA.scan(x,W,H);
}
VA.run(draw,PERIOD,null,'sm','sm-cv');
"
cat(paste0(
  "<canvas id='sm-cv' width='760' height='340' style='max-width:100%'></canvas>\n",
  "<script>\n(function(){\n", body, "\n})();\n</script>\n"))

## ----ram-box-anim, echo = FALSE, results = "asis", eval = TRUE----------------
body <- "
var s=VA.setup('rb-cv'); if(!s)return; var x=s.ctx,W=s.w,H=s.h,C=VA.C;
var BX=150,BW=W-150-40,RAM=0.7,ramX=BX+BW*RAM,BH=52,PERIOD=9.0;
var rows=[
 {y:88,name:'in memory',over:true,segs:[['data',0.44,C.cyan],['working copy',0.34,C.amber],['R',0.15,C.purple]]},
 {y:176,name:'streaming',over:false,segs:[['batch',0.09,C.green],['R',0.15,C.purple]]}
];
function draw(tc){VA.bg(x,W,H);
  VA.glow(x,C.cyan,7);x.fillStyle=C.cyan;x.textAlign='left';x.font=VA.F(15,true);x.fillText('WHAT  HAS  TO  FIT  IN  RAM',16,28);VA.noglow(x);
  x.strokeStyle=C.red;x.setLineDash([6,4]);x.beginPath();x.moveTo(ramX,58);x.lineTo(ramX,H-46);x.stroke();x.setLineDash([]);x.fillStyle=C.red;x.font=VA.F(12,true);x.textAlign='left';x.fillText('RAM',ramX+8,70);
  var t=VA.clamp(tc/(PERIOD-1.6),0,1);
  for(var r=0;r<2;r++){var rw=rows[r],X=BX;
    x.fillStyle=C.ink;x.font=VA.F(13,true);x.textAlign='right';x.fillText(rw.name,BX-14,rw.y+BH/2+5);
    for(var k=0;k<rw.segs.length;k++){var seg=rw.segs[k],w=seg[1]*BW*t;
      x.fillStyle=seg[2];x.globalAlpha=0.85;x.fillRect(X,rw.y,w,BH);x.globalAlpha=1;x.strokeStyle=C.bg;x.lineWidth=1.5;x.strokeRect(X,rw.y,w,BH);
      x.font=VA.F(11,true);if(w>x.measureText(seg[0]).width+12){x.fillStyle=C.bg;x.textAlign='center';x.fillText(seg[0],X+w/2,rw.y+BH/2+4);}
      X+=w;}
    if(rw.over&&X>ramX){var ox=Math.max(ramX,BX);x.save();x.beginPath();x.rect(ox,rw.y,X-ox,BH);x.clip();x.strokeStyle='rgba(255,59,107,0.5)';x.lineWidth=1.5;for(var hx=ox-BH;hx<X;hx+=10){x.beginPath();x.moveTo(hx,rw.y+BH);x.lineTo(hx+BH,rw.y);x.stroke();}x.restore();VA.glow(x,C.red,8);x.strokeStyle=C.red;x.lineWidth=2;x.strokeRect(ox,rw.y,X-ox,BH);VA.noglow(x);x.lineWidth=1;}
    if(!rw.over){VA.glow(x,C.green,6);x.fillStyle=C.green;x.font=VA.F(12,true);x.textAlign='left';x.fillText('fits',X+12,rw.y+BH/2+5);VA.noglow(x);}}
  x.fillStyle=C.mut;x.font=VA.F(11);x.textAlign='center';x.fillText('streaming holds one batch and R at a time',W/2,H-14);
  VA.scan(x,W,H);
}
VA.run(draw,PERIOD,null,'rb','rb-cv');
"
cat(paste0(
  "<canvas id='rb-cv' width='760' height='300' style='max-width:100%'></canvas>\n",
  "<script>\n(function(){\n", body, "\n})();\n</script>\n"))

## ----lazy-pipeline-anim, echo = FALSE, results = "asis", eval = TRUE----------
body <- "
var s=VA.setup('lp-cv'); if(!s)return; var x=s.ctx,W=s.w,H=s.h,C=VA.C;
var nodes=[{n:'scan',d:'read row group',c:C.cyan},{n:'filter',d:'cyl > 4',c:C.amber},{n:'project',d:'mpg, hp',c:C.green},{n:'collect',d:'data.frame',c:C.purple}];
var NN=nodes.length,BX=W/2-94,BW=188,BH=44,GAPY=22,TOP=70;
var BUILD=0.55,buildEnd=NN*BUILD+0.3,PERIOD=buildEnd+8.8;
function ny(i){return TOP+i*(BH+GAPY);}
function box(i,active){var y=ny(i),nd=nodes[i];if(active)VA.glow(x,nd.c,12);x.fillStyle=active?'rgba(58,215,255,0.08)':C.bg;x.strokeStyle=active?nd.c:C.off;x.lineWidth=active?2:1;x.beginPath();x.rect(BX,y,BW,BH);x.fill();x.stroke();VA.noglow(x);x.lineWidth=1;x.fillStyle=active?C.ink:C.mut;x.font=VA.F(13,true);x.textAlign='left';x.fillText(nd.n+'()',BX+14,y+19);x.fillStyle=nd.c;x.font=VA.F(11);x.fillText(nd.d,BX+14,y+35);}
function arrow(i){var y0=ny(i)+BH,y1=ny(i+1);x.strokeStyle=C.grid;x.lineWidth=1;x.beginPath();x.moveTo(BX+BW/2,y0);x.lineTo(BX+BW/2,y1);x.stroke();}
function token(tx,ty,col,lab){VA.glow(x,col,9);x.fillStyle=col;x.beginPath();x.arc(tx,ty,6,0,6.2832);x.fill();VA.noglow(x);x.fillStyle=col;x.font=VA.F(10,true);x.textAlign='center';x.fillText(lab,tx,ty-12);}
function draw(tc){VA.bg(x,W,H);
  VA.glow(x,C.cyan,7);x.fillStyle=C.cyan;x.textAlign='left';x.font=VA.F(15,true);x.fillText('LAZY PLAN, THEN ONE BATCH PULLED THROUGH',16,28);VA.noglow(x);
  var built=tc<buildEnd?Math.min(NN,Math.floor(tc/BUILD)+1):NN;
  for(var i=0;i<built-1;i++)arrow(i);
  var pulling=tc>=buildEnd,reqPos=-1,batchPos=-1;
  if(pulling){var pt=((tc-buildEnd)%4.4)/4.4;if(pt<0.4)reqPos=(NN-1)-(pt/0.4)*(NN-1);else batchPos=((pt-0.4)/0.6)*(NN-1);}
  for(var i=0;i<built;i++){var lit=pulling?((reqPos>=0&&Math.abs(i-reqPos)<0.55)||(batchPos>=0&&Math.abs(i-batchPos)<0.55)):true;box(i,lit);}
  if(reqPos>=0)token(BX-26,ny(0)+BH/2+reqPos*(BH+GAPY),C.cyan,'pull');
  if(batchPos>=0)token(BX+BW+26,ny(0)+BH/2+batchPos*(BH+GAPY),C.green,'batch');
  x.fillStyle=C.mut;x.font=VA.F(11);x.textAlign='center';x.fillText(pulling?'collect() requests; one batch flows back up the tree':'building the plan, no data moves yet',W/2,H-16);
  VA.scan(x,W,H);
}
VA.run(draw,PERIOD,null,'lp','lp-cv');
"
cat(paste0(
  "<canvas id='lp-cv' width='760' height='360' style='max-width:100%'></canvas>\n",
  "<script>\n(function(){\n", body, "\n})();\n</script>\n"))

## ----write-read---------------------------------------------------------------
library(vectra)

f <- tempfile(fileext = ".vtr")
write_vtr(mtcars, f)

node <- tbl(f)
node

## ----collect------------------------------------------------------------------
tbl(f) |> collect() |> head()

## ----write-batch-size---------------------------------------------------------
f_batched <- tempfile(fileext = ".vtr")
write_vtr(mtcars, f_batched, batch_size = 10)
tbl(f_batched) |> collect() |> nrow()

## ----filter-and---------------------------------------------------------------
tbl(f) |>
  filter(cyl == 6, mpg > 19) |>
  select(mpg, cyl, hp, wt) |>
  collect()

## ----filter-or----------------------------------------------------------------
tbl(f) |>
  filter(cyl == 4 | cyl == 8) |>
  select(mpg, cyl) |>
  collect() |>
  head()

## ----filter-in----------------------------------------------------------------
tbl(f) |>
  filter(cyl %in% c(4, 6)) |>
  select(mpg, cyl) |>
  collect() |>
  head()

## ----select-helpers-----------------------------------------------------------
tbl(f) |>
  select(starts_with("d"), mpg) |>
  collect() |>
  head()

## ----select-negate------------------------------------------------------------
tbl(f) |>
  select(-am, -vs, -gear, -carb) |>
  collect() |>
  head()

## ----explain-filter-----------------------------------------------------------
tbl(f) |>
  filter(cyl > 4) |>
  select(mpg, cyl, hp) |>
  explain()

## ----mutate-arith-------------------------------------------------------------
tbl(f) |>
  mutate(kpl = mpg * 0.425144, hp_per_wt = hp / wt) |>
  select(mpg, kpl, hp, wt, hp_per_wt) |>
  collect() |>
  head()

## ----mutate-math--------------------------------------------------------------
tbl(f) |>
  mutate(
    log_hp = log(hp),
    hp_floor = floor(hp / 10) * 10,
    bounded = pmin(pmax(mpg, 15), 25)
  ) |>
  select(hp, log_hp, hp_floor, mpg, bounded) |>
  collect() |>
  head()

## ----transmute----------------------------------------------------------------
tbl(f) |>
  transmute(
    efficiency = mpg / wt,
    power_ratio = hp / disp
  ) |>
  collect() |>
  head()

## ----mutate-cast--------------------------------------------------------------
tbl(f) |>
  mutate(cyl_str = as.character(cyl)) |>
  select(cyl, cyl_str) |>
  collect() |>
  head(3)

## ----mutate-control-----------------------------------------------------------
tbl(f) |>
  mutate(
    size = case_when(
      cyl == 4 ~ "small",
      cyl == 6 ~ "medium",
      cyl == 8 ~ "large"
    ),
    mpg_class = if_else(mpg > 20, "high", "low"),
    in_range = between(hp, 100, 200)
  ) |>
  select(cyl, size, mpg, mpg_class, hp, in_range) |>
  collect() |>
  head()

## ----mutate-coalesce----------------------------------------------------------
df_na <- data.frame(
  a = c(NA, 2, NA, 4),
  b = c(10, NA, NA, 40),
  stringsAsFactors = FALSE
)
f_na <- tempfile(fileext = ".vtr")
write_vtr(df_na, f_na)

tbl(f_na) |>
  mutate(filled = coalesce(a, b, 0)) |>
  collect()

## ----string-data--------------------------------------------------------------
people <- data.frame(
  name = c("  Alice  ", "Bob", "Charlie Brown", "Diana"),
  city = c("Amsterdam", "Berlin", "Chicago", "Dublin"),
  email = c("alice@example.com", "bob@test.org",
            "charlie.b@work.net", "diana@example.com"),
  stringsAsFactors = FALSE
)
fs <- tempfile(fileext = ".vtr")
write_vtr(people, fs)

## ----string-basic-------------------------------------------------------------
tbl(fs) |>
  mutate(
    name_trimmed = trimws(name),
    name_len = nchar(trimws(name)),
    city_prefix = substr(city, 1, 3)
  ) |>
  select(name_trimmed, name_len, city_prefix) |>
  collect()

## ----string-case--------------------------------------------------------------
tbl(fs) |>
  mutate(
    city_upper = toupper(city),
    is_example = endsWith(email, "example.com"),
    starts_a = startsWith(city, "A")
  ) |>
  select(city_upper, email, is_example, starts_a) |>
  collect()

## ----string-grepl-------------------------------------------------------------
tbl(fs) |>
  mutate(has_at = grepl("@example", email)) |>
  select(email, has_at) |>
  collect()

## ----string-gsub--------------------------------------------------------------
tbl(fs) |>
  mutate(domain = gsub(".*@", "", email, fixed = FALSE)) |>
  select(email, domain) |>
  collect()

## ----string-extract-----------------------------------------------------------
tbl(fs) |>
  mutate(user = str_extract(email, "^[^@]+")) |>
  select(email, user) |>
  collect()

## ----string-paste-------------------------------------------------------------
tbl(fs) |>
  mutate(
    greeting = paste0("Hello, ", trimws(name), "!"),
    label = paste(trimws(name), city, sep = " - ")
  ) |>
  select(greeting, label) |>
  collect()

## ----summarise-basic----------------------------------------------------------
tbl(f) |>
  group_by(cyl) |>
  summarise(
    count = n(),
    avg_mpg = mean(mpg),
    total_hp = sum(hp),
    best_mpg = max(mpg)
  ) |>
  collect()

## ----summarise-advanced-------------------------------------------------------
tbl(f) |>
  group_by(cyl) |>
  summarise(
    mpg_sd = sd(mpg),
    mpg_var = var(mpg),
    first_hp = first(hp),
    last_hp = last(hp)
  ) |>
  collect()

## ----summarise-median---------------------------------------------------------
tbl(f) |>
  group_by(cyl) |>
  summarise(
    med_mpg = median(mpg),
    unique_gears = n_distinct(gear)
  ) |>
  collect()

## ----count--------------------------------------------------------------------
tbl(f) |>
  count(cyl, sort = TRUE) |>
  collect()

## ----tally--------------------------------------------------------------------
tbl(f) |>
  group_by(gear) |>
  tally() |>
  collect()

## ----across-summarise---------------------------------------------------------
tbl(f) |>
  group_by(cyl) |>
  summarise(across(c(mpg, hp, wt), mean)) |>
  collect()

## ----across-multi-------------------------------------------------------------
tbl(f) |>
  group_by(cyl) |>
  summarise(across(
    c(mpg, hp),
    list(avg = mean, total = sum),
    .names = "{.col}_{.fn}"
  )) |>
  collect()

## ----ungroup------------------------------------------------------------------
tbl(f) |>
  group_by(cyl, gear) |>
  summarise(n = n(), .groups = "keep") |>
  ungroup() |>
  arrange(desc(n)) |>
  collect()

## ----arrange------------------------------------------------------------------
tbl(f) |>
  select(mpg, cyl, hp) |>
  arrange(cyl, desc(mpg)) |>
  collect() |>
  head(8)

## ----slice-head---------------------------------------------------------------
tbl(f) |>
  slice_head(n = 5) |>
  collect()

## ----slice-min----------------------------------------------------------------
tbl(f) |>
  select(mpg, cyl, hp) |>
  slice_min(order_by = mpg, n = 3) |>
  collect()

## ----slice-no-ties------------------------------------------------------------
tbl(f) |>
  select(mpg, cyl) |>
  slice_min(order_by = cyl, n = 3, with_ties = FALSE) |>
  collect()

## ----slice-max----------------------------------------------------------------
tbl(f) |>
  select(mpg, cyl, hp) |>
  slice_max(order_by = hp, n = 4, with_ties = FALSE) |>
  collect()

## ----join-setup---------------------------------------------------------------
cyl_info <- data.frame(
  cyl = c(4, 6, 8),
  engine_type = c("inline", "v-type", "v-type"),
  stringsAsFactors = FALSE
)
f_cyl <- tempfile(fileext = ".vtr")
write_vtr(cyl_info, f_cyl)

## ----left-join----------------------------------------------------------------
tbl(f) |>
  select(mpg, cyl, hp) |>
  left_join(tbl(f_cyl), by = "cyl") |>
  collect() |>
  head()

## ----semi-anti----------------------------------------------------------------
tbl(f) |>
  select(mpg, cyl) |>
  anti_join(
    tbl(f_cyl) |> filter(engine_type == "v-type"),
    by = "cyl"
  ) |>
  collect() |>
  head()

## ----join-named---------------------------------------------------------------
ratings <- data.frame(
  cylinders = c(4, 6, 8),
  rating = c("A", "B", "C"),
  stringsAsFactors = FALSE
)
f_rat <- tempfile(fileext = ".vtr")
write_vtr(ratings, f_rat)

tbl(f) |>
  select(mpg, cyl) |>
  inner_join(tbl(f_rat), by = c("cyl" = "cylinders")) |>
  collect() |>
  head()

## ----fuzzy-join---------------------------------------------------------------
ref_species <- data.frame(
  canonical = c("Quercus robur", "Quercus petraea",
                 "Fagus sylvatica"),
  code = c("QR", "QP", "FS"),
  stringsAsFactors = FALSE
)
query_species <- data.frame(
  name = c("Quercus robur", "Qurecus petraea",
           "Fagus sylvatca"),
  stringsAsFactors = FALSE
)
f_ref <- tempfile(fileext = ".vtr")
f_query <- tempfile(fileext = ".vtr")
write_vtr(ref_species, f_ref)
write_vtr(query_species, f_query)

tbl(f_query) |>
  fuzzy_join(
    tbl(f_ref),
    by = c("name" = "canonical"),
    method = "dl",
    max_dist = 0.15
  ) |>
  collect()

## ----window-rank--------------------------------------------------------------
tbl(f) |>
  select(mpg, cyl, hp) |>
  slice_head(n = 8) |>
  mutate(
    rn = row_number(),
    mpg_rank = rank(mpg),
    mpg_dense = dense_rank(mpg)
  ) |>
  collect()

## ----window-lag-lead----------------------------------------------------------
tbl(f) |>
  select(mpg, hp) |>
  slice_head(n = 6) |>
  mutate(
    prev_mpg = lag(mpg),
    next_mpg = lead(mpg),
    prev2_hp = lag(hp, n = 2, default = 0)
  ) |>
  collect()

## ----window-cum---------------------------------------------------------------
tbl(f) |>
  select(mpg, hp) |>
  slice_head(n = 6) |>
  mutate(
    running_hp = cumsum(hp),
    running_avg = cummean(mpg),
    running_min = cummin(mpg)
  ) |>
  collect()

## ----window-grouped-----------------------------------------------------------
tbl(f) |>
  select(mpg, cyl) |>
  group_by(cyl) |>
  mutate(rn = row_number(), pct = percent_rank(mpg)) |>
  slice_head(n = 10) |>
  collect()

## ----date-data----------------------------------------------------------------
events <- data.frame(
  event_date = as.Date(c("2020-03-15", "2020-07-01",
                          "2021-01-15", "2021-06-30")),
  event_time = as.POSIXct(c("2020-03-15 09:30:00",
                             "2020-07-01 14:00:00",
                             "2021-01-15 08:15:00",
                             "2021-06-30 17:45:00"),
                           tz = "UTC"),
  value = c(10, 20, 30, 40)
)
fd <- tempfile(fileext = ".vtr")
write_vtr(events, fd)

## ----date-extract-------------------------------------------------------------
tbl(fd) |>
  mutate(
    yr = year(event_date),
    mo = month(event_date),
    dy = day(event_date)
  ) |>
  group_by(yr) |>
  summarise(total = sum(value)) |>
  collect()

## ----time-extract-------------------------------------------------------------
tbl(fd) |>
  mutate(
    hr = hour(event_time),
    mn = minute(event_time)
  ) |>
  select(event_time, hr, mn) |>
  collect()

## ----date-filter--------------------------------------------------------------
tbl(fd) |>
  filter(event_date >= as.Date("2021-01-01")) |>
  collect()

## ----date-arith---------------------------------------------------------------
tbl(fd) |>
  mutate(plus_30 = event_date + 30) |>
  select(event_date, plus_30) |>
  collect()

## ----similarity-data----------------------------------------------------------
species <- data.frame(
  name = c("Quercus robur", "Quercus rubra",
           "Fagus sylvatica", "Acer platanoides",
           "Quercus petraea"),
  stringsAsFactors = FALSE
)
fs2 <- tempfile(fileext = ".vtr")
write_vtr(species, fs2)

## ----similarity-metrics-------------------------------------------------------
tbl(fs2) |>
  mutate(
    lev = levenshtein(name, "Quercus robur"),
    dl = dl_dist(name, "Quercus robur"),
    jw = jaro_winkler(name, "Quercus robur")
  ) |>
  filter(lev <= 5) |>
  arrange(lev) |>
  collect()

## ----similarity-norm----------------------------------------------------------
tbl(fs2) |>
  mutate(
    lev_norm = levenshtein_norm(name, "Quercus robur"),
    dl_norm = dl_dist_norm(name, "Quercus robur")
  ) |>
  collect()

## ----dl-transposition---------------------------------------------------------
tbl(fs2) |>
  mutate(
    lev = levenshtein(name, "Qurecus robur"),
    dl = dl_dist(name, "Qurecus robur")
  ) |>
  collect()

## ----resolve------------------------------------------------------------------
taxa <- data.frame(
  id        = c(1L, 2L, 3L, 4L),
  name      = c("Fagaceae", "Quercus", "Q. robur", "Q. petraea"),
  parent_id = c(NA, 1L, 2L, 2L),
  stringsAsFactors = FALSE
)
ft <- tempfile(fileext = ".vtr")
write_vtr(taxa, ft)

tbl(ft) |>
  mutate(parent_name = resolve(parent_id, id, name)) |>
  collect()

## ----propagate----------------------------------------------------------------
tbl(ft) |>
  mutate(family = propagate(
    parent_id, id,
    if_else(is.na(parent_id), name, NA_character_)
  )) |>
  collect()

## ----csv-roundtrip------------------------------------------------------------
csv_in <- tempfile(fileext = ".csv")
write.csv(mtcars, csv_in, row.names = FALSE)

tbl_csv(csv_in) |>
  filter(cyl == 6) |>
  select(mpg, cyl, hp) |>
  collect()

## ----sqlite-roundtrip---------------------------------------------------------
db <- tempfile(fileext = ".sqlite")
f_src <- tempfile(fileext = ".vtr")
write_vtr(mtcars, f_src)
tbl(f_src) |> write_sqlite(db, "cars")

tbl_sqlite(db, "cars") |>
  filter(mpg > 25) |>
  collect()

## ----format-conversion--------------------------------------------------------
csv_file <- tempfile(fileext = ".csv")
vtr_file <- tempfile(fileext = ".vtr")
csv_out <- tempfile(fileext = ".csv")

write.csv(mtcars, csv_file, row.names = FALSE)
tbl_csv(csv_file) |> write_vtr(vtr_file)

tbl(vtr_file) |>
  filter(cyl == 6) |>
  write_csv(csv_out)

read.csv(csv_out) |> head()

## ----index-create-------------------------------------------------------------
f_idx <- tempfile(fileext = ".vtr")
write_vtr(
  data.frame(id = letters, val = 1:26, stringsAsFactors = FALSE),
  f_idx,
  batch_size = 5
)

has_index(f_idx, "id")  # FALSE
create_index(f_idx, "id")
has_index(f_idx, "id")  # TRUE

## ----index-query--------------------------------------------------------------
tbl(f_idx) |>
  filter(id == "m") |>
  collect()

## ----index-composite----------------------------------------------------------
f_comp <- tempfile(fileext = ".vtr")
write_vtr(
  data.frame(
    region = rep(c("north", "south"), each = 13),
    id = letters,
    val = 1:26,
    stringsAsFactors = FALSE
  ),
  f_comp,
  batch_size = 5
)
create_index(f_comp, c("region", "id"))

tbl(f_comp) |>
  filter(region == "north", id == "c") |>
  collect()

## ----append-------------------------------------------------------------------
fa <- tempfile(fileext = ".vtr")
write_vtr(mtcars[1:16, ], fa)
append_vtr(mtcars[17:32, ], fa)
tbl(fa) |> collect() |> nrow()

## ----delete-------------------------------------------------------------------
delete_vtr(fa, c(0, 1, 2))  # 0-based row indices
tbl(fa) |> collect() |> nrow()
unlink(c(fa, paste0(fa, ".del")))

## ----diff---------------------------------------------------------------------
fd1 <- tempfile(fileext = ".vtr")
fd2 <- tempfile(fileext = ".vtr")
old <- data.frame(id = 1:5, val = letters[1:5],
                  stringsAsFactors = FALSE)
new <- data.frame(id = c(3L, 4L, 5L, 6L, 7L),
                  val = c("C", "d", "e", "f", "g"),
                  stringsAsFactors = FALSE)
write_vtr(old, fd1)
write_vtr(new, fd2)

d <- diff_vtr(fd1, fd2, "id")
d$deleted
collect(d$added)
unlink(c(fd1, fd2))

## ----block-materialize--------------------------------------------------------
blk_data <- data.frame(
  taxonID = c("T1", "T2", "T3", "T4", "T5"),
  name = c("Quercus robur", "Pinus sylvestris",
           "Fagus sylvatica", "Acer campestre",
           "Betula pendula"),
  stringsAsFactors = FALSE
)
f_blk <- tempfile(fileext = ".vtr")
write_vtr(blk_data, f_blk)

blk <- materialize(tbl(f_blk))
blk

## ----block-lookup-------------------------------------------------------------
block_lookup(blk, "name", c("Quercus robur", "Betula pendula"))

## ----block-fuzzy--------------------------------------------------------------
block_fuzzy_lookup(
  blk, "name",
  c("Qurecus robur", "Pinus silvestris"),
  method = "dl",
  max_dist = 0.2
)

## ----explain-full-------------------------------------------------------------
tbl(f) |>
  filter(cyl > 4) |>
  select(mpg, cyl, hp) |>
  arrange(desc(mpg)) |>
  explain()

## ----glimpse------------------------------------------------------------------
tbl(f) |> glimpse()

## ----cleanup------------------------------------------------------------------
unlink(c(f, f_batched, f_na, fs, fs2, f_cyl, f_rat, f_ref, f_query, fd,
         ft, csv_in, csv_out, csv_file, vtr_file, db, f_src, f_idx,
         paste0(f_idx, ".id.vtri"), f_comp,
         paste0(f_comp, ".region_id.vtri"), f_blk))

