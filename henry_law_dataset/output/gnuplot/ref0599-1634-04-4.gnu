# load "ref0599-1634-04-4.gnu"
# chem = "Me t-Bu ether"

set terminal postscript eps color
set title "ref = 599; chem = Me t-Bu ether; casrn = 1634-04-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1555703E-01 * exp(  -7721.200    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.1869173E-01 point
set label "" at    303.1500    ,   0.8293473E-02 point
set label "" at    313.1500    ,   0.4465716E-02 point
set label "" at    318.1500    ,   0.2718797E-02 point
set label "" at    323.1500    ,   0.2418930E-02 point
set label "" at    298.1500    ,   0.1555703E-01 point ps 2 pt 6

plot [290:330] H(T)
