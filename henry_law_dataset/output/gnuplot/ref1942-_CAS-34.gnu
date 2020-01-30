# load "ref1942-_CAS-34.gnu"
# chem = "4-(3',5'-dimethyl-3'-heptyl)-phenol(-)"

set terminal postscript eps color
set title "ref = 1942; chem = 4-(3',5'-dimethyl-3'-heptyl)-phenol(-); casrn = _CAS-34"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    3.274624     * exp(  -8606.797    *(1/   298.    -1/T))

set label "" at    278.0000    ,    23.67629     point
set label "" at    283.0000    ,    16.30397     point
set label "" at    288.0000    ,    9.533679     point
set label "" at    293.0000    ,    6.582778     point
set label "" at    298.0000    ,    2.753516     point
set label "" at    298.1500    ,    3.274624     point ps 2 pt 6

plot [270:300] H(T)
