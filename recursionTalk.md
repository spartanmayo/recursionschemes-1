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


