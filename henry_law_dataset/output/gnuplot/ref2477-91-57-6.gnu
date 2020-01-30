# load "ref2477-91-57-6.gnu"
# chem = "2-methylnaphthalene"

set terminal postscript eps color
set title "ref = 2477; chem = 2-methylnaphthalene; casrn = 91-57-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1949349E-01 * exp(  -5419.106    *(1/   298.    -1/T))

set label "" at    277.2500    ,   0.7692308E-01 point
set label "" at    284.1500    ,   0.4761905E-01 point
set label "" at    291.1500    ,   0.3012048E-01 point
set label "" at    298.1500    ,   0.1949318E-01 point
set label "" at    304.1500    ,   0.1364256E-01 point
set label "" at    298.1500    ,   0.1949349E-01 point ps 2 pt 6

plot [270:310] H(T)
