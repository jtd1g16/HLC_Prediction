# load "ref1942-58-89-9.gnu"
# chem = "$\gamma$-1,2,3,4,5,6-hexachlorocyclohexane"

set terminal postscript eps color
set title "ref = 1942; chem = $\gamma$-1,2,3,4,5,6-hexachlorocyclohexane; casrn = 58-89-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    6.021394     * exp(  -6170.728    *(1/   298.    -1/T))

set label "" at    278.0000    ,    28.67999     point
set label "" at    283.0000    ,    16.07698     point
set label "" at    288.0000    ,    11.60622     point
set label "" at    293.0000    ,    11.50753     point
set label "" at    298.0000    ,    5.260301     point
set label "" at    298.1500    ,    6.021394     point ps 2 pt 6

plot [270:300] H(T)
