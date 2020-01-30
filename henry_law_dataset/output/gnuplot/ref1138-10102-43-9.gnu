# load "ref1138-10102-43-9.gnu"
# chem = "nitrogen monoxide"

set terminal postscript eps color
set title "ref = 1138; chem = nitrogen monoxide; casrn = 10102-43-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1908061E-04 * exp(  -1462.961    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.1908061E-04 point
set label "" at    288.1500    ,   0.2262281E-04 point
set label "" at    298.1500    ,   0.1908061E-04 point ps 2 pt 6

plot [280:300] H(T)
