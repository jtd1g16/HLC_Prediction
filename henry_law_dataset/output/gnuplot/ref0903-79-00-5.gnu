# load "ref0903-79-00-5.gnu"
# chem = "1,1,2-trichloroethane"

set terminal postscript eps color
set title "ref = 903; chem = 1,1,2-trichloroethane; casrn = 79-00-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1241783E-01 * exp(  -5927.140    *(1/   298.    -1/T))

set label "" at    299.3500    ,   0.1219512E-01 point
set label "" at    308.9500    ,   0.5434783E-02 point
set label "" at    317.9500    ,   0.3861004E-02 point
set label "" at    298.1500    ,   0.1241783E-01 point ps 2 pt 6

plot [290:320] H(T)
