# load "ref2926-106-38-7.gnu"
# chem = "1-bromo-4-methylbenzene"

set terminal postscript eps color
set title "ref = 2926; chem = 1-bromo-4-methylbenzene; casrn = 106-38-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3413381E-02 * exp(  -4558.103    *(1/   298.    -1/T))

set label "" at    293.1000    ,   0.4689992E-02 point
set label "" at    298.1600    ,   0.3236369E-02 point
set label "" at    308.1000    ,   0.2005142E-02 point
set label "" at    318.0300    ,   0.1373248E-02 point
set label "" at    322.9100    ,   0.1048142E-02 point
set label "" at    298.1500    ,   0.3413381E-02 point ps 2 pt 6

plot [290:330] H(T)
