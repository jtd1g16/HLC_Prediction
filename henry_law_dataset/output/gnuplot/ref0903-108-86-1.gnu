# load "ref0903-108-86-1.gnu"
# chem = "bromobenzene"

set terminal postscript eps color
set title "ref = 903; chem = bromobenzene; casrn = 108-86-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5305127E-02 * exp(  -5346.394    *(1/   298.    -1/T))

set label "" at    303.1500    ,   0.3906250E-02 point
set label "" at    308.1500    ,   0.3012048E-02 point
set label "" at    317.9500    ,   0.1727116E-02 point
set label "" at    298.1500    ,   0.5305127E-02 point ps 2 pt 6

plot [300:320] H(T)
