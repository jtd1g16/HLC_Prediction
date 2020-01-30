# load "ref1936-75-18-3.gnu"
# chem = "dimethyl sulfide"

set terminal postscript eps color
set title "ref = 1936; chem = dimethyl sulfide; casrn = 75-18-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5498632E-02 * exp(  -3751.529    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.8496177E-02 point
set label "" at    293.1500    ,   0.6839945E-02 point
set label "" at    298.1500    ,   0.5491488E-02 point
set label "" at    303.1500    ,   0.4458315E-02 point
set label "" at    308.1500    ,   0.3660322E-02 point
set label "" at    298.1500    ,   0.5498632E-02 point ps 2 pt 6

plot [280:310] H(T)
