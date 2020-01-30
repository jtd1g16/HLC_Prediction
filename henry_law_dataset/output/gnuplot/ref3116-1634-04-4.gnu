# load "ref3116-1634-04-4.gnu"
# chem = "methyl t-butyl ether"

set terminal postscript eps color
set title "ref = 3116; chem = methyl t-butyl ether; casrn = 1634-04-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1100697E-01 * exp(  -4386.740    *(1/   298.    -1/T))

set label "" at    313.0000    ,   0.5512142E-02 point
set label "" at    323.0000    ,   0.3543016E-02 point
set label "" at    333.0000    ,   0.2319443E-02 point
set label "" at    343.0000    ,   0.1627703E-02 point
set label "" at    298.1500    ,   0.1100697E-01 point ps 2 pt 6

plot [310:350] H(T)
