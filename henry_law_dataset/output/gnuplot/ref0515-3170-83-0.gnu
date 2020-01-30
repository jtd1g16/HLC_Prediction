# load "ref0515-3170-83-0.gnu"
# chem = "hydroperoxy radical"

set terminal postscript eps color
set title "ref = 515; chem = hydroperoxy radical; casrn = 3170-83-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    38.30028     * exp(  -5932.077    *(1/   298.    -1/T))

set label "" at    249.0000    ,    1973.847     point
set label "" at    275.0000    ,    197.3847     point
set label "" at    298.0000    ,    39.47693     point
set label "" at    298.1500    ,    38.30028     point ps 2 pt 6

plot [240:300] H(T)
