# load "ref2484-76-19-7.gnu"
# chem = "octafluoropropane"

set terminal postscript eps color
set title "ref = 2484; chem = octafluoropropane; casrn = 76-19-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1238376E-06 * exp(  -6887.830    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.7048138E-06 point
set label "" at    283.1500    ,   0.3593516E-06 point
set label "" at    288.1500    ,   0.2992722E-06 point
set label "" at    298.1500    ,   0.1238376E-06 point ps 2 pt 6

plot [270:290] H(T)
