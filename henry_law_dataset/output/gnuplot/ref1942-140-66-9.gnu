# load "ref1942-140-66-9.gnu"
# chem = "4-(1,1,3,3-tetramethylbutyl)-phenol"

set terminal postscript eps color
set title "ref = 1942; chem = 4-(1,1,3,3-tetramethylbutyl)-phenol; casrn = 140-66-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    2.313209     * exp(  -9038.229    *(1/   298.    -1/T))

set label "" at    278.0000    ,    19.81742     point
set label "" at    283.0000    ,    12.60301     point
set label "" at    288.0000    ,    5.585986     point
set label "" at    293.0000    ,    5.664940     point
set label "" at    298.0000    ,    1.924500     point
set label "" at    298.1500    ,    2.313209     point ps 2 pt 6

plot [270:300] H(T)
