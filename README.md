---
title: "Insurance Data"
output: 
  html_document:
     highlight: zenburn
     theme: lumen
     df_print: paged
     fig_align: center
     code_folding: hide
     keep_md: yes
---




```r
data(ausNLHYClaimByState)

aus <- ausNLHYClaimByState %>%
    as_tibble()

aus %>%
    head()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Class"],"name":[1],"type":["chr"],"align":["left"]},{"label":["NSWACT200506"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["NSWACT200512"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["NSWACT200606"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["NSWACT200612"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["NSWACT200706"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["NSWACT200712"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["NSWACT200806"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["NSWACT200812"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["NSWACT200906"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["NSWACT200912"],"name":[11],"type":["dbl"],"align":["right"]},{"label":["NSWACT201006"],"name":[12],"type":["dbl"],"align":["right"]},{"label":["VIC200506"],"name":[13],"type":["dbl"],"align":["right"]},{"label":["VIC200512"],"name":[14],"type":["dbl"],"align":["right"]},{"label":["VIC200606"],"name":[15],"type":["dbl"],"align":["right"]},{"label":["VIC200612"],"name":[16],"type":["dbl"],"align":["right"]},{"label":["VIC200706"],"name":[17],"type":["dbl"],"align":["right"]},{"label":["VIC200712"],"name":[18],"type":["dbl"],"align":["right"]},{"label":["VIC200806"],"name":[19],"type":["dbl"],"align":["right"]},{"label":["VIC200812"],"name":[20],"type":["dbl"],"align":["right"]},{"label":["VIC200906"],"name":[21],"type":["dbl"],"align":["right"]},{"label":["VIC200912"],"name":[22],"type":["dbl"],"align":["right"]},{"label":["VIC201006"],"name":[23],"type":["dbl"],"align":["right"]},{"label":["QLD200506"],"name":[24],"type":["dbl"],"align":["right"]},{"label":["QLD200512"],"name":[25],"type":["dbl"],"align":["right"]},{"label":["QLD200606"],"name":[26],"type":["dbl"],"align":["right"]},{"label":["QLD200612"],"name":[27],"type":["dbl"],"align":["right"]},{"label":["QLD200706"],"name":[28],"type":["dbl"],"align":["right"]},{"label":["QLD200712"],"name":[29],"type":["dbl"],"align":["right"]},{"label":["QLD200806"],"name":[30],"type":["dbl"],"align":["right"]},{"label":["QLD200812"],"name":[31],"type":["dbl"],"align":["right"]},{"label":["QLD200906"],"name":[32],"type":["dbl"],"align":["right"]},{"label":["QLD200912"],"name":[33],"type":["dbl"],"align":["right"]},{"label":["QLD201006"],"name":[34],"type":["dbl"],"align":["right"]},{"label":["SA200506"],"name":[35],"type":["dbl"],"align":["right"]},{"label":["SA200512"],"name":[36],"type":["dbl"],"align":["right"]},{"label":["SA200606"],"name":[37],"type":["dbl"],"align":["right"]},{"label":["SA200612"],"name":[38],"type":["dbl"],"align":["right"]},{"label":["SA200706"],"name":[39],"type":["dbl"],"align":["right"]},{"label":["SA200712"],"name":[40],"type":["dbl"],"align":["right"]},{"label":["SA200806"],"name":[41],"type":["dbl"],"align":["right"]},{"label":["SA200812"],"name":[42],"type":["dbl"],"align":["right"]},{"label":["SA200906"],"name":[43],"type":["dbl"],"align":["right"]},{"label":["SA200912"],"name":[44],"type":["dbl"],"align":["right"]},{"label":["SA201006"],"name":[45],"type":["dbl"],"align":["right"]},{"label":["WA200506"],"name":[46],"type":["dbl"],"align":["right"]},{"label":["WA200512"],"name":[47],"type":["dbl"],"align":["right"]},{"label":["WA200606"],"name":[48],"type":["dbl"],"align":["right"]},{"label":["WA200612"],"name":[49],"type":["dbl"],"align":["right"]},{"label":["WA200706"],"name":[50],"type":["dbl"],"align":["right"]},{"label":["WA200712"],"name":[51],"type":["dbl"],"align":["right"]},{"label":["WA200806"],"name":[52],"type":["dbl"],"align":["right"]},{"label":["WA200812"],"name":[53],"type":["dbl"],"align":["right"]},{"label":["WA200906"],"name":[54],"type":["dbl"],"align":["right"]},{"label":["WA200912"],"name":[55],"type":["dbl"],"align":["right"]},{"label":["WA201006"],"name":[56],"type":["dbl"],"align":["right"]},{"label":["TAS200506"],"name":[57],"type":["dbl"],"align":["right"]},{"label":["TAS200512"],"name":[58],"type":["dbl"],"align":["right"]},{"label":["TAS200606"],"name":[59],"type":["dbl"],"align":["right"]},{"label":["TAS200612"],"name":[60],"type":["dbl"],"align":["right"]},{"label":["TAS200706"],"name":[61],"type":["dbl"],"align":["right"]},{"label":["TAS200712"],"name":[62],"type":["dbl"],"align":["right"]},{"label":["TAS200806"],"name":[63],"type":["dbl"],"align":["right"]},{"label":["TAS200812"],"name":[64],"type":["dbl"],"align":["right"]},{"label":["TAS200906"],"name":[65],"type":["dbl"],"align":["right"]},{"label":["TAS200912"],"name":[66],"type":["dbl"],"align":["right"]},{"label":["TAS201006"],"name":[67],"type":["dbl"],"align":["right"]},{"label":["NT200506"],"name":[68],"type":["dbl"],"align":["right"]},{"label":["NT200512"],"name":[69],"type":["dbl"],"align":["right"]},{"label":["NT200606"],"name":[70],"type":["dbl"],"align":["right"]},{"label":["NT200612"],"name":[71],"type":["dbl"],"align":["right"]},{"label":["NT200706"],"name":[72],"type":["dbl"],"align":["right"]},{"label":["NT200712"],"name":[73],"type":["dbl"],"align":["right"]},{"label":["NT200806"],"name":[74],"type":["dbl"],"align":["right"]},{"label":["NT200812"],"name":[75],"type":["dbl"],"align":["right"]},{"label":["NT200906"],"name":[76],"type":["dbl"],"align":["right"]},{"label":["NT200912"],"name":[77],"type":["dbl"],"align":["right"]},{"label":["NT201006"],"name":[78],"type":["dbl"],"align":["right"]},{"label":["Total200506"],"name":[79],"type":["dbl"],"align":["right"]},{"label":["Total200512"],"name":[80],"type":["dbl"],"align":["right"]},{"label":["Total200606"],"name":[81],"type":["dbl"],"align":["right"]},{"label":["Total200612"],"name":[82],"type":["dbl"],"align":["right"]},{"label":["Total200706"],"name":[83],"type":["dbl"],"align":["right"]},{"label":["Total200712"],"name":[84],"type":["dbl"],"align":["right"]},{"label":["Total200806"],"name":[85],"type":["dbl"],"align":["right"]},{"label":["Total200812"],"name":[86],"type":["dbl"],"align":["right"]},{"label":["Total200906"],"name":[87],"type":["dbl"],"align":["right"]},{"label":["Total200912"],"name":[88],"type":["dbl"],"align":["right"]},{"label":["Total201006"],"name":[89],"type":["dbl"],"align":["right"]}],"data":[{"1":"Houseowners/householders","2":"659","3":"680","4":"632","5":"635","6":"1100","7":"1291","8":"809","9":"977","10":"1065","11":"1003","12":"808","13":"422","14":"421","15":"457","16":"457","17":"479","18":"613","19":"632","20":"618","21":"1162","22":"1327","23":"1135","24":"407","25":"427","26":"566","27":"665","28":"648","29":"588","30":"633","31":"808","32":"986","33":"861","34":"628","35":"136","36":"149","37":"155","38":"158","39":"189","40":"223","41":"190","42":"178","43":"200","44":"207","45":"216","46":"158","47":"191","48":"189","49":"154","50":"176","51":"209","52":"219","53":"206","54":"220","55":"261","56":"549","57":"38","58":"43","59":"45","60":"42","61":"51","62":"61","63":"60","64":"58","65":"65","66":"67","67":"72","68":"3","69":"4","70":"3","71":"4","72":"5","73":"5","74":"6","75":"7","76":"7","77":"7","78":"9","79":"1824","80":"1915","81":"2047","82":"2115","83":"2650","84":"2990","85":"2550","86":"2852","87":"3706","88":"3734","89":"3417"},{"1":"Commercial motor vehicle","2":"386","3":"361","4":"382","5":"413","6":"429","7":"507","8":"556","9":"544","10":"539","11":"532","12":"539","13":"251","14":"271","15":"293","16":"310","17":"316","18":"338","19":"371","20":"357","21":"338","22":"356","23":"383","24":"169","25":"185","26":"215","27":"225","28":"202","29":"253","30":"292","31":"247","32":"255","33":"257","34":"205","35":"85","36":"83","37":"85","38":"91","39":"115","40":"129","41":"117","42":"107","43":"107","44":"108","45":"118","46":"58","47":"97","48":"106","49":"120","50":"138","51":"150","52":"163","53":"152","54":"160","55":"170","56":"194","57":"18","58":"19","59":"20","60":"20","61":"21","62":"24","63":"25","64":"23","65":"28","66":"26","67":"27","68":"7","69":"7","70":"7","71":"7","72":"29","73":"29","74":"4","75":"4","76":"9","77":"9","78":"4","79":"975","80":"1024","81":"1107","82":"1187","83":"1251","84":"1429","85":"1528","86":"1435","87":"1436","88":"1457","89":"1471"},{"1":"Domestic motor vehicle","2":"1572","3":"1685","4":"1591","5":"1707","6":"1821","7":"1992","8":"2037","9":"2051","10":"2138","11":"2119","12":"1873","13":"1105","14":"1165","15":"1259","16":"1314","17":"1259","18":"1376","19":"1465","20":"1478","21":"1442","22":"1461","23":"1963","24":"778","25":"834","26":"862","27":"883","28":"885","29":"951","30":"1022","31":"1067","32":"952","33":"932","34":"960","35":"239","36":"252","37":"256","38":"262","39":"288","40":"383","41":"402","42":"327","43":"350","44":"367","45":"370","46":"311","47":"315","48":"360","49":"381","50":"422","51":"436","52":"488","53":"486","54":"407","55":"456","56":"922","57":"55","58":"62","59":"63","60":"65","61":"66","62":"71","63":"78","64":"78","65":"61","66":"61","67":"90","68":"6","69":"7","70":"7","71":"9","72":"9","73":"11","74":"13","75":"13","76":"6","77":"6","78":"18","79":"4067","80":"4321","81":"4399","82":"4622","83":"4749","84":"5220","85":"5505","86":"5500","87":"5357","88":"5401","89":"6196"},{"1":"Travel","2":"110","3":"110","4":"112","5":"140","6":"143","7":"125","8":"82","9":"157","10":"230","11":"239","12":"147","13":"NA","14":"NA","15":"NA","16":"17","17":"NA","18":"NA","19":"NA","20":"NA","21":"NA","22":"NA","23":"NA","24":"NA","25":"NA","26":"NA","27":"5","28":"NA","29":"NA","30":"NA","31":"NA","32":"NA","33":"NA","34":"13","35":"2","36":"4","37":"2","38":"4","39":"4","40":"NA","41":"NA","42":"NA","43":"NA","44":"NA","45":"NA","46":"5","47":"5","48":"4","49":"7","50":"7","51":"12","52":"13","53":"14","54":"NA","55":"NA","56":"NA","57":"0","58":"0","59":"0","60":"1","61":"1","62":"1","63":"1","64":"1","65":"NA","66":"NA","67":"NA","68":"0","69":"0","70":"0","71":"0","72":"0","73":"1","74":"1","75":"1","76":"1","77":"1","78":"1","79":"143","80":"135","81":"136","82":"175","83":"180","84":"175","85":"134","86":"233","87":"307","88":"316","89":"210"},{"1":"Fire and ISR","2":"515","3":"398","4":"460","5":"590","6":"603","7":"863","8":"780","9":"809","10":"827","11":"480","12":"445","13":"275","14":"220","15":"304","16":"394","17":"387","18":"589","19":"544","20":"463","21":"488","22":"506","23":"559","24":"175","25":"176","26":"241","27":"294","28":"296","29":"251","30":"1390","31":"1501","32":"392","33":"288","34":"108","35":"63","36":"67","37":"70","38":"81","39":"94","40":"109","41":"80","42":"67","43":"116","44":"118","45":"93","46":"80","47":"88","48":"115","49":"86","50":"57","51":"94","52":"94","53":"179","54":"200","55":"139","56":"186","57":"14","58":"14","59":"16","60":"13","61":"14","62":"91","63":"91","64":"17","65":"17","66":"21","67":"21","68":"2","69":"3","70":"8","71":"9","72":"5","73":"8","74":"9","75":"7","76":"8","77":"7","78":"4","79":"1125","80":"966","81":"1213","82":"1468","83":"1457","84":"2005","85":"2987","86":"3044","87":"2047","88":"1560","89":"1417"},{"1":"Marine and aviation","2":"108","3":"136","4":"125","5":"132","6":"143","7":"114","8":"125","9":"150","10":"147","11":"169","12":"159","13":"45","14":"37","15":"45","16":"63","17":"51","18":"65","19":"64","20":"47","21":"55","22":"55","23":"54","24":"47","25":"44","26":"48","27":"56","28":"55","29":"53","30":"66","31":"73","32":"64","33":"58","34":"52","35":"9","36":"14","37":"15","38":"18","39":"18","40":"13","41":"14","42":"18","43":"20","44":"13","45":"12","46":"20","47":"16","48":"17","49":"25","50":"26","51":"26","52":"29","53":"26","54":"31","55":"48","56":"38","57":"4","58":"4","59":"4","60":"5","61":"5","62":"4","63":"6","64":"4","65":"4","66":"7","67":"6","68":"5","69":"6","70":"9","71":"7","72":"1","73":"3","74":"5","75":"5","76":"5","77":"4","78":"2","79":"239","80":"257","81":"263","82":"306","83":"298","84":"278","85":"309","86":"324","87":"327","88":"352","89":"324"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
data(nzcathist)

nz <- nzcathist %>%
    as_tibble()

nz %>%
    head()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Year"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["Quarter"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["Date"],"name":[3],"type":["chr"],"align":["left"]},{"label":["FirstDay"],"name":[4],"type":["date"],"align":["right"]},{"label":["Event"],"name":[5],"type":["fctr"],"align":["left"]},{"label":["Type"],"name":[6],"type":["fctr"],"align":["left"]},{"label":["Location"],"name":[7],"type":["fctr"],"align":["left"]},{"label":["OriginalCost"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["NormCost2011"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["NormCost2014"],"name":[10],"type":["dbl"],"align":["right"]}],"data":[{"1":"2014","2":"4","3":"5Oct","4":"2014-10-05","5":"Auckland Power Outage","6":"Power outage","7":"Auckland","8":"2.0","9":"NA","10":"2.00"},{"1":"2014","2":"2","3":"25Jun","4":"2014-06-25","5":"Nelson-Tasman Floods","6":"Flood","7":"Nelson, Tasman","8":"2.7","9":"NA","10":"2.72"},{"1":"2014","2":"2","3":"9-11Jun","4":"2014-06-09","5":"Severe Weather North and South Islands","6":"Weather","7":"North and South Islands","8":"37.6","9":"NA","10":"37.85"},{"1":"2014","2":"2","3":"17Apr","4":"2014-04-17","5":"Easter Weekend Storm and Floods","6":"Flood, Storm","7":"NA","8":"55.3","9":"NA","10":"55.67"},{"1":"2014","2":"1","3":"15-16Mar","4":"2014-03-15","5":"Cyclone Lusi","6":"Cyclone","7":"NA","8":"3.6","9":"NA","10":"3.63"},{"1":"2014","2":"1","3":"23-Feb","4":"2014-02-23","5":"Canterbury storm","6":"Storm","7":"Canterbury","8":"4.8","9":"NA","10":"4.84"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## For AUS data

- **Class** : Class of business.
- **NSWACTYYYYMM** : New South Wales / Australian Capital Territory for year YYYY.
- **VICYYYYMM** : Victoria in year YYYY reported onDate YYYYMM.
- **QLDYYYMM** : Queensland in year YYYY reported onDate YYYYMM.
- **SAYYYYMM** : South Australia in year YYYY reported onDate YYYYMM.
- **WAYYYYMM** : Western Australia in year YYYY reported onDate YYYYMM.
- **TAYYYYMM** : Tasmania in year YYYY reported onDate YYYYMM.
- **NTYYYYMM** : Northern Territory in year YYYY reported onDate YYYYMM.
- **TotalYYYYMM** : Total in year YYYY reported onDate YYYYMM.

Data is in values of millions of Australian dollars (AUD)

## For NZ data

nzcathist is a data frame of 9 columns:

- **Year** : numeric for the Year.
- **Quarter** : numeric for the quarter of the year.
- **Date** : character string for the date.
- **FirstDayaDateobject** : for the first day of natural catastrophe.
- **Event** : character string describing the event.
- **Type** : factor describing the event type among the list:
   "Cyclone"
   "Earthquake"
   "Flood"
   "Flood, Storm"
   "Hailstorm"
   "Other"
   "Power outage"
   "Storm"
   "Tornado"
   "Weather"
- **Location** character string describing the location.
- **OriginalCostOriginal** cost in million of Australian dollars (NZD).
- **NormCost2011Normed** cost in million of 2011 New Zealand dollars (NZD).
- **NormCost2014Normed** cost in million of 2014 New Zealand dollars (NZD)
