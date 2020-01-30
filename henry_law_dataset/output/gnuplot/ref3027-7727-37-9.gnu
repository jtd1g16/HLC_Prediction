# load "ref3027-7727-37-9.gnu"
# chem = "nitrogen"

set terminal postscript eps color
set title "ref = 3027; chem = nitrogen; casrn = 7727-37-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6510231E-05 * exp(  -1356.242    *(1/   298.    -1/T))

set label "" at    273.2400    ,   0.1030340E-04 point
set label "" at    273.2700    ,   0.1031220E-04 point
set label "" at    273.2500    ,   0.1031220E-04 point
set label "" at    273.2200    ,   0.1032541E-04 point
set label "" at    273.2000    ,   0.1032541E-04 point
set label "" at    273.2200    ,   0.1033422E-04 point
set label "" at    283.1700    ,   0.8163462E-05 point
set label "" at    283.1200    ,   0.8154656E-05 point
set label "" at    283.1500    ,   0.8189881E-05 point
set label "" at    283.1500    ,   0.8189881E-05 point
set label "" at    283.2000    ,   0.8172268E-05 point
set label "" at    293.1900    ,   0.6780869E-05 point
set label "" at    293.1700    ,   0.6802885E-05 point
set label "" at    293.1400    ,   0.6789675E-05 point
set label "" at    293.0800    ,   0.6763256E-05 point
set label "" at    293.2000    ,   0.6807288E-05 point
set label "" at    293.2000    ,   0.6798482E-05 point
set label "" at    293.1500    ,   0.6697209E-05 point
set label "" at    293.1500    ,   0.6789675E-05 point
set label "" at    303.1500    ,   0.5922253E-05 point
set label "" at    303.1000    ,   0.5904640E-05 point
set label "" at    303.0800    ,   0.5904640E-05 point
set label "" at    303.1600    ,   0.5904640E-05 point
set label "" at    303.1500    ,   0.5873818E-05 point
set label "" at    303.0800    ,   0.5891430E-05 point
set label "" at    313.0900    ,   0.5195731E-05 point
set label "" at    313.0700    ,   0.5204537E-05 point
set label "" at    313.1900    ,   0.5204537E-05 point
set label "" at    313.1600    ,   0.5217747E-05 point
set label "" at    313.2000    ,   0.5213343E-05 point
set label "" at    313.2000    ,   0.5204537E-05 point
set label "" at    323.1500    ,   0.4773027E-05 point
set label "" at    323.1800    ,   0.4790640E-05 point
set label "" at    323.0300    ,   0.4737802E-05 point
set label "" at    323.2200    ,   0.4795043E-05 point
set label "" at    323.1200    ,   0.4786237E-05 point
set label "" at    323.2000    ,   0.4825865E-05 point
set label "" at    298.1500    ,   0.6510231E-05 point ps 2 pt 6

plot [270:330] H(T)
