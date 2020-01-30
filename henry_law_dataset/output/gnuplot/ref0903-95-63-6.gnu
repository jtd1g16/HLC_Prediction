# load "ref0903-95-63-6.gnu"
# chem = "1,2,4-trimethylbenzene"

set terminal postscript eps color
set title "ref = 903; chem = 1,2,4-trimethylbenzene; casrn = 95-63-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1506411E-02 * exp(  -4295.461    *(1/   298.    -1/T))

set label "" at    300.1500    ,   0.1420455E-02 point
set label "" at    308.1500    ,   0.8810573E-03 point
set label "" at    318.1500    ,   0.6285355E-03 point
set label "" at    298.1500    ,   0.1506411E-02 point ps 2 pt 6

plot [300:320] H(T)
