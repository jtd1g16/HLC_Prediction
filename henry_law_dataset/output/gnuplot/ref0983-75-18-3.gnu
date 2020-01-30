# load "ref0983-75-18-3.gnu"
# chem = "dimethyl sulfide"

set terminal postscript eps color
set title "ref = 983; chem = dimethyl sulfide; casrn = 75-18-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4244383E-02 * exp(  -4307.136    *(1/   298.    -1/T))

set label "" at    291.1500    ,   0.5986872E-02 point
set label "" at    298.1500    ,   0.4291441E-02 point
set label "" at    308.1500    ,   0.2619494E-02 point
set label "" at    317.1500    ,   0.1797292E-02 point
set label "" at    298.1500    ,   0.4244383E-02 point ps 2 pt 6

plot [290:320] H(T)
