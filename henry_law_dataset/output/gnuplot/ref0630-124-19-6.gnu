# load "ref0630-124-19-6.gnu"
# chem = "nonanal"

set terminal postscript eps color
set title "ref = 630; chem = nonanal; casrn = 124-19-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1002173E-01 * exp(  -6698.608    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.3059462E-01 point
set label "" at    298.1500    ,   0.1184308E-01 point
set label "" at    308.1500    ,   0.4539847E-02 point
set label "" at    318.1500    ,   0.2368616E-02 point
set label "" at    298.1500    ,   0.1002173E-01 point ps 2 pt 6

plot [280:320] H(T)
