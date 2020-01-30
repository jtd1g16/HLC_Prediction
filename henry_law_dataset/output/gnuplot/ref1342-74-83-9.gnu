# load "ref1342-74-83-9.gnu"
# chem = "bromomethane"

set terminal postscript eps color
set title "ref = 1342; chem = bromomethane; casrn = 74-83-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1728740E-02 * exp(  -3388.836    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.4892402E-02 point
set label "" at    295.1500    ,   0.1940456E-02 point
set label "" at    298.1500    ,   0.1728740E-02 point ps 2 pt 6

plot [270:300] H(T)
