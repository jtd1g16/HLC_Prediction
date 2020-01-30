# load "ref0525-7782-44-7.gnu"
# chem = "oxygen"

set terminal postscript eps color
set title "ref = 525; chem = oxygen; casrn = 7782-44-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1218590E-04 * exp(  -1792.820    *(1/   298.    -1/T))

set label "" at    273.6300    ,   0.2129312E-04 point
set label "" at    273.6700    ,   0.2122579E-04 point
set label "" at    273.7900    ,   0.2117108E-04 point
set label "" at    278.3600    ,   0.1863779E-04 point
set label "" at    283.2500    ,   0.1654425E-04 point
set label "" at    283.3700    ,   0.1649375E-04 point
set label "" at    287.7000    ,   0.1502090E-04 point
set label "" at    287.8700    ,   0.1497251E-04 point
set label "" at    288.1200    ,   0.1489256E-04 point
set label "" at    288.1400    ,   0.1489466E-04 point
set label "" at    288.2700    ,   0.1484206E-04 point
set label "" at    288.2900    ,   0.1484837E-04 point
set label "" at    293.2300    ,   0.1333976E-04 point
set label "" at    293.2500    ,   0.1334817E-04 point
set label "" at    298.2500    ,   0.1212151E-04 point
set label "" at    298.5000    ,   0.1206470E-04 point
set label "" at    298.5100    ,   0.1208153E-04 point
set label "" at    302.9500    ,   0.1115364E-04 point
set label "" at    302.9600    ,   0.1116837E-04 point
set label "" at    307.9100    ,   0.1024047E-04 point
set label "" at    307.9700    ,   0.1024258E-04 point
set label "" at    298.1500    ,   0.1218590E-04 point ps 2 pt 6

plot [270:310] H(T)
