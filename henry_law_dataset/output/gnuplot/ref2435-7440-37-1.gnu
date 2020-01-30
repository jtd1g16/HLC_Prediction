# load "ref2435-7440-37-1.gnu"
# chem = "argon"

set terminal postscript eps color
set title "ref = 2435; chem = argon; casrn = 7440-37-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1407663E-04 * exp(  -1581.539    *(1/   298.    -1/T))

set label "" at    275.0990    ,   0.2242915E-04 point
set label "" at    276.1390    ,   0.2189166E-04 point
set label "" at    277.1370    ,   0.2133220E-04 point
set label "" at    277.1400    ,   0.2132250E-04 point
set label "" at    277.6440    ,   0.2103975E-04 point
set label "" at    278.1630    ,   0.2072645E-04 point
set label "" at    279.1490    ,   0.2018577E-04 point
set label "" at    280.2270    ,   0.1971385E-04 point
set label "" at    281.1460    ,   0.1928807E-04 point
set label "" at    281.1500    ,   0.1927907E-04 point
set label "" at    283.1570    ,   0.1845358E-04 point
set label "" at    288.1580    ,   0.1655853E-04 point
set label "" at    288.1650    ,   0.1656497E-04 point
set label "" at    293.1420    ,   0.1507354E-04 point
set label "" at    298.1380    ,   0.1383856E-04 point
set label "" at    298.1520    ,   0.1383154E-04 point
set label "" at    298.1560    ,   0.1382152E-04 point
set label "" at    303.1570    ,   0.1281131E-04 point
set label "" at    303.1580    ,   0.1281900E-04 point
set label "" at    308.1590    ,   0.1195899E-04 point
set label "" at    313.1450    ,   0.1126553E-04 point
set label "" at    313.1500    ,   0.1127832E-04 point
set label "" at    298.1500    ,   0.1407663E-04 point ps 2 pt 6

plot [270:320] H(T)
