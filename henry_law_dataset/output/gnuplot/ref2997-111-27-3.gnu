# load "ref2997-111-27-3.gnu"
# chem = "1-hexanol"

set terminal postscript eps color
set title "ref = 2997; chem = 1-hexanol; casrn = 111-27-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5069836     * exp(  -6142.369    *(1/   298.    -1/T))

set label "" at    323.1500    ,   0.1052632     point
set label "" at    333.1500    ,   0.5747126E-01 point
set label "" at    343.1500    ,   0.3289474E-01 point
set label "" at    353.1500    ,   0.2079002E-01 point
set label "" at    363.1500    ,   0.1282051E-01 point
set label "" at    298.1500    ,   0.5069836     point ps 2 pt 6

plot [320:370] H(T)
