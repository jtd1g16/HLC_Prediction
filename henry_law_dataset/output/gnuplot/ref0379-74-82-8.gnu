# load "ref0379-74-82-8.gnu"
# chem = "methane"

set terminal postscript eps color
set title "ref = 379; chem = methane; casrn = 74-82-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1316937E-04 * exp(  -1943.965    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.2449479E-04 point
set label "" at    274.1500    ,   0.2378148E-04 point
set label "" at    275.1500    ,   0.2309018E-04 point
set label "" at    276.1500    ,   0.2242530E-04 point
set label "" at    277.1500    ,   0.2177804E-04 point
set label "" at    278.1500    ,   0.2115719E-04 point
set label "" at    279.1500    ,   0.2055836E-04 point
set label "" at    280.1500    ,   0.1998595E-04 point
set label "" at    281.1500    ,   0.1943115E-04 point
set label "" at    282.1500    ,   0.1889837E-04 point
set label "" at    283.1500    ,   0.1839201E-04 point
set label "" at    284.1500    ,   0.1792967E-04 point
set label "" at    285.1500    ,   0.1748055E-04 point
set label "" at    286.1500    ,   0.1704904E-04 point
set label "" at    287.1500    ,   0.1663955E-04 point
set label "" at    288.1500    ,   0.1624767E-04 point
set label "" at    289.1500    ,   0.1587780E-04 point
set label "" at    290.1500    ,   0.1552115E-04 point
set label "" at    291.1500    ,   0.1518210E-04 point
set label "" at    292.1500    ,   0.1486507E-04 point
set label "" at    293.1500    ,   0.1456566E-04 point
set label "" at    294.1500    ,   0.1427945E-04 point
set label "" at    295.1500    ,   0.1400205E-04 point
set label "" at    296.1500    ,   0.1373346E-04 point
set label "" at    297.1500    ,   0.1347808E-04 point
set label "" at    298.1500    ,   0.1323590E-04 point
set label "" at    299.1500    ,   0.1299813E-04 point
set label "" at    300.1500    ,   0.1277357E-04 point
set label "" at    301.1500    ,   0.1255782E-04 point
set label "" at    302.1500    ,   0.1235527E-04 point
set label "" at    303.1500    ,   0.1216153E-04 point
set label "" at    298.1500    ,   0.1316937E-04 point ps 2 pt 6

plot [270:310] H(T)
