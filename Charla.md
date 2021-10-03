# Recursion Schemes in Scala

---

## Introduction

---

Es casi seguro que todos, de una maner o de otra, hemos oido hablar de recursión. De manera intuitiva, un objeto es recursivo si se puede definir inductivamente sobre unos casos base y unas reglas de construcción. Uno de los ejemplos más sencillos es el de los números naturales: tenemos un primer elemento llamado 0 que es un número natural y después tenemos la regla _todo número natural tiene un siguiente_. Así podemos construir todos los números que nos plazcan. De mano de los objetos recursivos, tenemos las funciones recursivas, que recorren la estructura recursiva para calcular su valor. Un nuevo ejemplo sencillo de esto es la suma de naturales. Por ejemplo, la función que suma un cierto m es:

$$
m + 0 = 0 \\
m + s(n) = s(m + n)
$$

donde s(n) es la función que asigna el siguiente de n.

La esencia es que las funciones recursivas definidas sobre una estructura inductiva recorren la estructura hasta llegar a los casos base y después van construyendo desde ese punto un resultado. 

Voliendo al mundo de la programación, uno de los ejemplos más famosos de tipos recursivos es el de Listas y Trees. Vamos a desarrollar la charla sobre un ejemplo tonto para que sea más notorio el argumento que vamos a realizar que la implementación como tal que se necesita.

Imaginemos que queremos implementar la estructura de los números enteros con las operaciones de suma y producto. Para ello, podemos definir una estructura recursiva que genere, desde los casos base (números enteros) un arbol de expresiones que recreen estas operaciones. Un ejemplo sencillo en scala sería:

---

```scala
sealed trait Ring 
case object Zero extends Ring
case object One extends Ring
case class Elem(x: Int) extends Ring
case class Add(x: Ring, y: Ring) extends Ring
case class Mult(x: Ring, y: Ring) extends Ring
```
---

Una vez tenemos la estructura, nos interesa la forma en la que podemos evaluarla. Lo primero que podemos pensar es hacer corresponder cada clase asociada a una operación con su operación correspondiente, por ejemplo:

---
```scala
def toInt: Ring => Int = {
    case Zero => 0
    case One => 1
    case Elem(x) => x
    case Add(x, y) => toInt(x) + toInt(y)
    case Mult(x, y) => toInt(x) * toInt(y)
}
```
---

```scala
val expresion1 = Mult(Elem(4), Add(Elem(3), One))
toInt(expresion1)

16
```
![](example.png)

Si nos fijamos en las dos formas de evaluar nuestras expresiones, vemos claramente exactamente el mismo patrón. Sería interesante abstraer la noción de evaluación para que, dependiendo de lo que se quiera evaluar, se use el mismo patrón pero sobre casos diferentes (Int, String). En este punto es donde la idea de functor sale a la luz. El concepto de functor nace de una rama profunda de las matemáticas llamada teoría de categorías. Sin embargo, muchas de sus ideas encajan perfectamente en el mundo de la programación. Por lo que a nosotros respecta, un functor es una forma de constrir tipos a partir de otros y construir funciones a partir de funciones entre tipos. Siguiendo esta idea, un functor en scala tiene esta pinta:

```scala
trait Functor[F[_]] {
    def map[A,B](f: A => B): F[A] => F[B]
}
```
 Es decir, dame un tipo `A` y sé darte un tipo `F[A]`. Para las funciones es map el que nos da una función `F[A] => F[B]` a partir de A y B. Para tener los pies en la tierra, puedes convencerte de que `List[ ]` es un functor.
 
Por ejemplo, si tuviesemos nuestr anillo Ring definido sobre un tipo genérico, podríamos darle estructura de functor y ayudarnos de map para saber evaluar las expresiones de Int y String a partir de la funcón base toString: Int => String. Vamos a tener esto en mente

Otra cosa que notamos es que en estas implementaciones (a raíz de al definición de Ring) tenemos la definición de caso base y la forma de recorrerlo. Esto no es complicado en estos casos, pero los momentos de llamada a las funciones durante la recursión son una de las cosas que más dolor de cabeza puede provocar. Por poner un ejemplo tonto, si tenemos una forma de optimizar los cálculos de manera que si sumamos dos expresiones iguales nos genere esa expresión multiplicada por dos, las llamadas en su implementación pueden ser delicadas:

```scala
def optimizeSum: Ring => Ring = {
    case Zero => Zero
    case One => One
    case Elem(x) => Elem(x)
    case Add(x, y) => {
        if (x == y) Mult(Elem(2), optimizeSum(x))
        else Add(x, y)
    }
    case Mult(x, y) => Mult(optimizeSum(x), optimizeSum(y))
}
```

Como puede apreciarse, tenemos unas llamadas de optimize en diversos lugares, cosa que empieza a complicar la vida, porque el compilador no se va a dar cuenta de qeu estamos cometiendo un error al prescindir de una de sus llamadas. El problema en este caso viene, como ya hemos dicho, de haber mezclado la idea de evaluar los casos base con el recorrido de la estructura inductiva del tipo. ¿Sería posible separar estas dos ideas?

## Recursion schemes

La idea detrás de los esquemas de recursión es abstraer el paso de la recursión del caso base, de manera que podamos dar un tipo base y, por un proceso general e independiente del tipo base, generar su estructura recursiva. Para ello vamos a definir un Functor asociado al anillo que contemple únicamente los casos base (olvidando la recursión por el momento):

```scala
sealed trait RingF[+A]
sealed trait RingFF[+A] extends RingF[A]
case object Zero extends RingF[Nothing]
case object One extends RingF[Nothing]
case class Elem[B](x: Int) extends RingF[Nothing]
case class Add[A](x: A, y: A) extends RingF[A]
case class Mult[A](x:A, y: A) extends RingF[A]
```

Hemos supuesto que el conjunto que toma la estructura de anillo es el de los enteros, más adelante generalizaremos el conjunto base (al igual que se hace `List`). 

Desde este punto, podemos definir, por un lado, la estructura de functor de este tipo `RingF[A]`. Esto es uy fácil, pues dada na función `f: A => B` podemos contruir una función `map(f): RingF[A] => RingF[B]` simplemente aplicando `f` a los argumentos de las case classes que definen `RingF`:

```scala
val ringFunctor = new Functor[RingF] {
    override def map[A, B](f: A => B): RingF[A] => RingF[B] = {
        case Zero => Zero
        case One => One
        case Elem(x) => Elem(x)
        case Add(x, y) => Add(f(x), f(y))
        case Mult(x, y) => Mult(f(x), f(y))
    }
}
```

Además de tener la estructura de functor, podemos redefinir las funciones de evaluación para que se centren únicamente de los casos base:

```scala
def evalToInt: RingF[Int] => Int = {
    x => x match {
        case Zero => 0
        case One => 1
        case Elem(x) => x
        case Add(x, y) => x + y
        case Mult(x, y) => x*y
    }
}

def evalToStr: RingF[String] => String = {
    x => x match {
        case Zero => "0"
        case One => "1"
        case Elem(x) => x.toString
        case Add(x, y) => x + " + " + y
        case Mult(x, y) => x + " * " + y
    }
}
```

Vale, dos de los puntos que hemos comentado ya los hemos conseguido, sin embargo, esto produce algunos problemas. Si, por ejemplo, queremos definir un valor similar a `expression1` necesitamos anidar las tipos `A` dentro de RingF:

```scala
val expresion1F: RingF[RingF[RingF[Nothing]]] = Mult(Elem(4), Add(Elem(3), One))
```

Como puede verse, esta estructura dificulta mucho su construcción y no es nada cómoda. Además, otro problema es que no podemos evaluarla, porque la signatura de evalToInt es `RingF[Int]` y no una estructura anidada de `RingF[RingF[..]]`. Para solucionar esto, necesitamos abstraer RingF para que tenga un sabor recursivo. Vamos a desarrollar esto despacio porque en este punto es donde vienen las curvas.