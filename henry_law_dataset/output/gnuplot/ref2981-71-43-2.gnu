# load "ref2981-71-43-2.gnu"
# chem = "benzene"

set terminal postscript eps color
set title "ref = 2981; chem = benzene; casrn = 71-43-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1746914E-02 * exp(  -4443.566    *(1/   298.    -1/T))

set label "" at    277.6500    ,   0.5341107E-02 point
set label "" at    279.4800    ,   0.4776811E-02 point
set label "" at    280.2100    ,   0.4485372E-02 point
set label "" at    282.1100    ,   0.4058675E-02 point
set label "" at    284.9000    ,   0.3449018E-02 point
set label "" at    285.2500    ,   0.3385757E-02 point
set label "" at    288.2500    ,   0.2883199E-02 point
set label "" at    291.0800    ,   0.2553536E-02 point
set label "" at    293.2100    ,   0.2260157E-02 point
set label "" at    298.1500    ,   0.1746914E-02 point ps 2 pt 6

plot [270:300] H(T)
