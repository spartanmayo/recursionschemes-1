# Recursion Schemes in Scala

---

In this piece of work, I want to share some notions about recursion schemes. Recursion schemes are a paradigm based on the abstraction of the recursive steps and the base cases in recursive data types. I will try to provide some justifications about their utilities and a lot of examples where it can be usefull. All the exampes can be used as scala snippets and feel free to play with all the conctructions in Scastie. Enjoy!

## Introduction

---

I'm sure that all of you, in some way, has been heard about recursion. The idea of recursion lies in the usage of base definition and some rules to build new structures based on the previous one. One of the main examples are natural numbers, in the core of mathematics, and Lists in computer science. In both cases we have a base case that tell us an examples of an element of our structure. In the case of natural numbers, 0 is a natural number and for the case of Lists, Nil is a List. The next step is to provide a rule to build, inductively, new numbers and new lists. For the first one we have the basic rule _every natural number has a next element_. For lists, _every list is Nil or has a head of type A and a tail of type List[A]_.  It is trivial to see the similarities between the way of constructing this types. A simple implementetation of this two types in scala is:

```scala
sealed trait Nat
final object Zero extends Nat
final case class Suc(n: Nat) extends Nat

sealed trait List[+A]
final object Nil extends List[Nothing]
final case class Cons[A](head: A, tail) extends List[A]
```

In both cases we can see the recursive construction strategy in the Cons and Suc definitions, with an argument defined by its own type. 

With every recursive structure, there are functions acting on them. We can define recursive functions acting on a inductively defined type, like Nat or Lists. For example, lets define the length of a list:

```scala
def length: List[A] => Int = {
    case Nil => 0
    case Cons(h, t) => 1 + length(t)
} 
```

We can follow every step in the recursion to find the length of a given List. 

**Note:** Try to do something similar to define a recursive definition for de function _sum 2_ over Nat.

With these examples we can observe two behaviors:

   1) Every recursive structure use its own type to define the rule to build the next iteration.
    
   2) Every function with a domain over the a recursive Type needs to traverse the structure until the base case and them iterates to produce the result.
   
This observations are pretty well defined over the two examples. But there are some cases where the definition of the case cases (for function evaluation an recursive types) are not trivial and the recursion is not so easy to see. For these reason, it would be interesting to have a clear separation between the recursion step and the base cases definition, In fact, what we will try to build is a blueprint for general recursion The blue print is the following:

   1) Give me a funtor F[A] and a way to evaluate it for a fixed type B (think in the evaluation of an expresion to an integer)
   
   2) I have a magic type constructor to turn F[A] into its associated recursive type and a way to lift the base case evaluations to a traversavle evaluator function acting over the recursive type.
   
Lets do this.

## Functors and F-algebras

---

Lets start with a simple example. Imagine we want to implement the ring structure for the set of integers numbers, i.e, the set with multiplication and addition. The recursive version of this is:

```scala
sealed trait Ring 
case object Zero extends Ring
case object One extends Ring
case class Elem(x: Int) extends Ring
case class Add(x: Ring, y: Ring) extends Ring
case class Mult(x: Ring, y: Ring) extends Ring
```

As we have comment in the previous section, we want to separate the base cases for the recursive steps. For this porpouse, lets imagine that our base type is Int and we want to implemente it sum and mult operations. It can be defined in scala as the following:

```scala 
sealed trait RingF[+A]
case object Zero extends RingF[Nothing]
case object One extends RingF[Nothing]
case class Elem[B](x: Int) extends RingF[Nothing]
case class Add[A](x: A, y: A) extends RingF[A]
case class Mult[A](x:A, y: A) extends RingF[A]
```

For the definition of Ring, we can build a value as deep as we want, for example:

```scala
val expresion1 = Mult(Elem(4), Add(Elem(3), One))
```
![](example.png)

But, for the RingF, we are able to build just base cases like Elem(7), Add(3, 4), etc. 

Lets imagine that we want to evaluate an expresion to, for example, to it associated value after resolving the operations. This can be made by the recursive function:

```scala
def toInt: Ring => Int = {
    case Zero => 0
    case One => 1
    case Elem(x) => x
    case Add(x, y) => toInt(x) + toInt(y)
    case Mult(x, y) => toInt(x) * toInt(y)
}
```
 and in Case we want to define it for RingF we can simply resolve it without recursive calls:
 
```scala
def evalToInt: RingF[Int] => Int = {
    x => x match {
        case Zero => 0
        case One => 1
        case Elem(x) => x
        case Add(x, y) => x + y
        case Mult(x, y) => x * y
    }
}
```

Here is where the magic comes, if we want to evaluate, for example, a value of the form `RingF[RingF[A]]` we need first and evaluation of the form `RingF[RingF[A]] => RingF[A]` and them another evaluatuion `RingF[A] => A`. In the previous example, A is Int. The way of doing this is based on a good property of RingF, i.e it is a functor. A functor, for our porpouses, is a type constructor with the property that we can implement a way of translating functions between tipes in functions between the constructed types by the type constructor. So for this simple example, for every type A we can build a new type `RingF[A]`. Now, given a function `f: A => B` we need to know how to build a function with the signature `fmap: RingF[A] => RingF[B]`. This can be easily define and implemented in scala in the following way:

```scala 
trait Functor[F[_]] {
    def map[A,B](f: A => B): F[A] => F[B]
}

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

we simply encapsulate the image of f under the RingF constructor. Now that we know how to lift functions using the map property of the functor, we can lift the first step evaluation evaltoInt into a evaluation between `RingF[RingF[A]] => RingF[A]` by simply taking map with `A` as `RingF[A]` and `B` as `A` :

```scala
def liftInt: RingF[RingF[Int]] => RingF[Int] = {
    ringFunctor.map(evalToInt)
}
```

So, thanks to the map property we know how to lift one floor of recursion our base cases. Our goals now are to define the full recursive expresion and lift out evalToInt to the infinite floor of recursion. 

The argument to do this steps can be formaly made in terms of initial objects of certain cathegory, but let me try to express the idea before geting in details.If we want to find the full recursive type thats mean that we dont case abaout the depth of the expresion of our ring, so we can think about it as a type `H` related to `RingF[A]` and with an infinity depth. Its obious that an object with this two properties must to verified that `RingF[H] = H`, read as, if we are in the infinite recursive step, we dont get nothing if we repeat the aplication of `RingF`. We have to take care with this equaliaty, is better to understand that as _All the information inside `RingF[H]` is insede `H` and the converse is also true_. The equality understood as this property is called an isomorphism between the types `H` and `RingF[H]`. This is why we prefer to call `H` as `Fix[RingF]` because it is a _fixpoint_ of the equation `RingF[H] = H`. So the final signature is `Fix[RingF] = RingF[Fix[RingF]]`. Avoiding the formal and tecnical proofs, we can just implement the previous definition. Even more, this argument works for every Functor `F[A]`, so we can simply write an abstractions like:

```scala
case class Fix[F[_]](value: F[Fix[F]] )
object Fix {
  def fix[F[_]](ff: F[Fix[F]]): Fix[F] = new Fix[F](ff)
  def unfix[F[_]]: Fix[F] => F[Fix[F]] = f => f.value
}
```
fix and unfix defines the way of translating information back and forth (this functions defines the isomorphism of the fixpoint).

Now, we can build our recursive type Ring by simply fixing the RingF functor:

```scala
type Ring = Fix[RingF]
val zero = Fix[RingF](Zero)
val one =  Fix[RingF](One)
val elem(x: Int) = Fix[RingF](Elem(x))
def add: (Ring, Ring) => Ring = (x, y) =>  Fix[RingF](Add(x, y))
def mult: (Ring, Ring) => Ring = (x, y) =>  Fix[RingF](Mult(x, y))
```
and we can build recursive values like:

```scala
val expresion2 = mult(elem(4), add(elem(3), one))
```
![](exam)