# load "ref2837-111-76-2.gnu"
# chem = "3-oxa-1-heptanol"

set terminal postscript eps color
set title "ref = 2837; chem = 3-oxa-1-heptanol; casrn = 111-76-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    12.79748     * exp(  -8298.536    *(1/   298.    -1/T))

set label "" at    293.1500    ,    24.07130     point
set label "" at    295.1500    ,    15.66545     point
set label "" at    296.1500    ,    14.55639     point
set label "" at    298.1500    ,    12.18424     point
set label "" at    298.1500    ,    12.09465     point
set label "" at    303.1500    ,    9.771517     point
set label "" at    303.1500    ,    7.310543     point
set label "" at    298.1500    ,    12.79748     point ps 2 pt 6

plot [290:310] H(T)
