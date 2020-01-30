# load "ref0276-13517-11-8.gnu"
# chem = "hypobromous acid"

set terminal postscript eps color
set title "ref = 276; chem = hypobromous acid; casrn = 13517-11-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1764360E-01 * exp(  -4048.790    *(1/   298.    -1/T))

set label "" at    240.0000    ,   0.4737232     point
set label "" at    298.0000    ,   0.1776462E-01 point
set label "" at    298.1500    ,   0.1764360E-01 point ps 2 pt 6

plot [240:300] H(T)
