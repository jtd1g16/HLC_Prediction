# load "ref2996-678-39-7.gnu"
# chem = "8:2 FTOH"

set terminal postscript eps color
set title "ref = 2996; chem = 8:2 FTOH; casrn = 678-39-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1708597E-03 * exp(  -8774.486    *(1/   298.    -1/T))

set label "" at    338.1500    ,   0.5197596E-05 point
set label "" at    348.1500    ,   0.2529387E-05 point
set label "" at    358.1500    ,   0.1256633E-05 point
set label "" at    363.1500    ,   0.8632814E-06 point
set label "" at    298.1500    ,   0.1708597E-03 point ps 2 pt 6

plot [330:370] H(T)
