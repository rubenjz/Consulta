/* 1. Crear una función con nombre (integracion) que use el método de Simpson para
  calcular el valor aproximado de cualquier función . Se recomienda analizar lo
  siguiente:
1. El tipo de dato que devolverá la función integracion
  Double
2. ¿Cuáles son los parámetros que recibe la función?
  Una funcion, un valor a (que es el limite inferir), un valor b (limite superior)
*/

val limite_superior = 5
val limite_inferior = 3
val valor_esperado = 7.33

def f1(x: Int): Double = {
  -(x*x) + 8*x -12
}

def f2(x: Int): Double = {
  3 * (x*x)
}

def f3(x: Int): Double = {
  x + 2*(x*x) - (x*x*x) + 5*(x*x*x*x)
}

def f4(x: Int): Double = {
  (2*(x) + 1) / ((x*x) + x)
}

def f5(x: Int): Double = {
  math.exp(x)
}

def f6(x: Int): Double = {
  1 / (math.sqrt(x - 1))
}

def f7(x: Int): Double = {
  1 / (1 + (x*x))
}

def integracion(f: Int => Double, a: Int, b: Int): Double = {
  val x = (a + b) / 2

  (b - a) * (f(a) + 4*f(x) + f(b)) / 6
}

//Metodo para calcular el valor obtenido
def calcularError(esperado: Double, salida: Double): Double = {
  math.abs(esperado - salida)
}

println(integracion(f1,limite_inferior,limite_superior))

print(calcularError(valor_esperado, integracion(f1,limite_inferior,limite_superior)))

// Por: Ruben Nicolai Jimenez Lanchi