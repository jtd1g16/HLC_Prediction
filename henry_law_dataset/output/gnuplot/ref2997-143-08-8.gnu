# load "ref2997-143-08-8.gnu"
# chem = "1-nonanol"

set terminal postscript eps color
set title "ref = 2997; chem = 1-nonanol; casrn = 143-08-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1365009     * exp(  -6249.855    *(1/   298.    -1/T))

set label "" at    323.1500    ,   0.2433090E-01 point
set label "" at    333.1500    ,   0.1639344E-01 point
set label "" at    343.1500    ,   0.9174312E-02 point
set label "" at    353.1500    ,   0.5681818E-02 point
set label "" at    363.1500    ,   0.2857143E-02 point
set label "" at    298.1500    ,   0.1365009     point ps 2 pt 6

plot [320:370] H(T)
