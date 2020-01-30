# load "ref2463-71-43-2.gnu"
# chem = "benzene"

set terminal postscript eps color
set title "ref = 2463; chem = benzene; casrn = 71-43-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1590474E-02 * exp(  -4277.336    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.3481685E-02 point
set label "" at    288.1500    ,   0.2560705E-02 point
set label "" at    298.1500    ,   0.1563548E-02 point
set label "" at    308.1500    ,   0.1013778E-02 point
set label "" at    298.1500    ,   0.1590474E-02 point ps 2 pt 6

plot [280:310] H(T)
