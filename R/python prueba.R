library(reticulate)
py_install("matplotlib")

# Importamos matplotlib de Python
plt <- import("matplotlib.pyplot")

# Generamos datos en R
x <- seq(0, 10, length.out = 100)

# Pasamos datos a Python
py$x <- x
py$plt <- plt

# Ejecutamos código Python para graficar usando matplotlib
py_run_string("
import numpy as np
y = np.sin(x)
plt.plot(x, y)
plt.title('Seno de x desde Python ejecutado en R')
plt.xlabel('x')
plt.ylabel('sin(x)')
plt.grid(True)
plt.show()
")
