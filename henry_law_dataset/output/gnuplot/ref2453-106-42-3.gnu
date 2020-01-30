# load "ref2453-106-42-3.gnu"
# chem = "1,4-dimethylbenzene"

set terminal postscript eps color
set title "ref = 2453; chem = 1,4-dimethylbenzene; casrn = 106-42-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1320593E-02 * exp(  -4762.646    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.2315561E-02 point
set label "" at    298.1500    ,   0.1305234E-02 point
set label "" at    308.1500    ,   0.7872249E-03 point
set label "" at    318.1500    ,   0.4854553E-03 point
set label "" at    298.1500    ,   0.1320593E-02 point ps 2 pt 6

plot [280:320] H(T)
