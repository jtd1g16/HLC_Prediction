# load "ref2905-95-50-1.gnu"
# chem = "1,2-dichlorobenzene"

set terminal postscript eps color
set title "ref = 2905; chem = 1,2-dichlorobenzene; casrn = 95-50-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4912066E-02 * exp(  -4355.859    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.8326395E-02 point
set label "" at    298.1500    ,   0.4705882E-02 point
set label "" at    308.1500    ,   0.3125977E-02 point
set label "" at    298.1500    ,   0.4912066E-02 point ps 2 pt 6

plot [280:310] H(T)
