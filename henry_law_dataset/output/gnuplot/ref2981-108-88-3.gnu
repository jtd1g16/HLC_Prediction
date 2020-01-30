# load "ref2981-108-88-3.gnu"
# chem = "methylbenzene"

set terminal postscript eps color
set title "ref = 2981; chem = methylbenzene; casrn = 108-88-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1574847E-02 * exp(  -5036.586    *(1/   298.    -1/T))

set label "" at    277.6500    ,   0.5596683E-02 point
set label "" at    279.4800    ,   0.4914521E-02 point
set label "" at    280.2100    ,   0.4584093E-02 point
set label "" at    282.1100    ,   0.4097045E-02 point
set label "" at    284.9000    ,   0.3398359E-02 point
set label "" at    285.2500    ,   0.3339376E-02 point
set label "" at    288.2500    ,   0.2787231E-02 point
set label "" at    291.0800    ,   0.2417182E-02 point
set label "" at    293.2100    ,   0.2108386E-02 point
set label "" at    298.1500    ,   0.1574847E-02 point ps 2 pt 6

plot [270:300] H(T)
