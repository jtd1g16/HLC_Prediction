# load "ref2576-1184-57-2.gnu"
# chem = "hydroxymethylmercury"

set terminal postscript eps color
set title "ref = 2576; chem = hydroxymethylmercury; casrn = 1184-57-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    979.0991     * exp(  -7748.085    *(1/   298.    -1/T))

set label "" at    288.1500    ,    2412.688     point
set label "" at    293.1500    ,    1525.189     point
set label "" at    298.1500    ,    979.0991     point ps 2 pt 6

plot [280:300] H(T)
