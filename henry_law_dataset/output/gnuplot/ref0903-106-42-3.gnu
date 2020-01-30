# load "ref0903-106-42-3.gnu"
# chem = "1,4-dimethylbenzene"

set terminal postscript eps color
set title "ref = 903; chem = 1,4-dimethylbenzene; casrn = 106-42-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1235451E-02 * exp(  -3072.052    *(1/   298.    -1/T))

set label "" at    300.1500    ,   0.1168224E-02 point
set label "" at    308.9500    ,   0.8410429E-03 point
set label "" at    319.1500    ,   0.6345178E-03 point
set label "" at    298.1500    ,   0.1235451E-02 point ps 2 pt 6

plot [300:320] H(T)
