# load "ref0985-100-52-7.gnu"
# chem = "benzaldehyde"

set terminal postscript eps color
set title "ref = 985; chem = benzaldehyde; casrn = 100-52-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3451781     * exp(  -6985.842    *(1/   298.    -1/T))

set label "" at    278.0000    ,    1.802643     point
set label "" at    283.0000    ,    1.214259     point
set label "" at    288.0000    ,   0.8352247     point
set label "" at    293.0000    ,   0.5547106     point
set label "" at    298.0000    ,   0.3228788     point
set label "" at    298.1500    ,   0.3451781     point ps 2 pt 6

plot [270:300] H(T)
