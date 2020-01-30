# load "ref2171-7439-97-6.gnu"
# chem = "mercury"

set terminal postscript eps color
set title "ref = 2171; chem = mercury; casrn = 7439-97-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1260611E-02 * exp(  -2673.678    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.2402228E-02 point
set label "" at    298.1500    ,   0.1260611E-02 point
set label "" at    298.1500    ,   0.1260611E-02 point ps 2 pt 6

plot [270:300] H(T)
