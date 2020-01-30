# load "ref2953-108-88-3.gnu"
# chem = "methylbenzene"

set terminal postscript eps color
set title "ref = 2953; chem = methylbenzene; casrn = 108-88-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1637598E-02 * exp(  -4075.651    *(1/   298.    -1/T))

set label "" at    275.1500    ,   0.5906968E-02 point
set label "" at    279.1500    ,   0.4228185E-02 point
set label "" at    283.1500    ,   0.3280043E-02 point
set label "" at    291.1500    ,   0.2096925E-02 point
set label "" at    298.1500    ,   0.1544393E-02 point
set label "" at    303.1500    ,   0.1215881E-02 point
set label "" at    313.1500    ,   0.8236601E-03 point
set label "" at    323.1500    ,   0.5687460E-03 point
set label "" at    333.1500    ,   0.4373827E-03 point
set label "" at    298.1500    ,   0.1637598E-02 point ps 2 pt 6

plot [270:340] H(T)
