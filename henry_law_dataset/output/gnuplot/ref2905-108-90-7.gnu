# load "ref2905-108-90-7.gnu"
# chem = "chlorobenzene"

set terminal postscript eps color
set title "ref = 2905; chem = chlorobenzene; casrn = 108-90-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1935882E-02 * exp(  -1739.136    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.2443196E-02 point
set label "" at    298.1500    ,   0.1818182E-02 point
set label "" at    308.1500    ,   0.1654807E-02 point
set label "" at    298.1500    ,   0.1935882E-02 point ps 2 pt 6

plot [280:310] H(T)
