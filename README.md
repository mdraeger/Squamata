# Squamata - a rudimentary algebra library

This is a little project I started when I first tried to learn Scala. 

The implementation of vector and matrix are inspired by [scalinear](http://letstalkdata.com/2015/07/introduction-scalinear-0-1-a-simple-linear-algebra-library-for-scala/) by Phillip Johnson.

I implemented sparse and dense versions of vectors and matrices. They can be added and multiplied together. However,
if you multiply a sparse matrix with a dense matrix, you get a sparse matrix and vice versa.

Usage is pretty straightforward:
```scala
scala> import org.draegisoft.squamata.vectorspace._
import org.draegisoft.squamata.vectorspace._

scala> import org.draegisoft.squamata.field._
import org.draegisoft.squamata.field._

scala>  val A = DenseMatrix(DenseVector(Complex.i, Complex(3.0, 2.0)), DenseVector(Complex(1.0,1.0), Complex(0.0)))
A: org.draegisoft.squamata.vectorspace.DenseMatrix[org.draegisoft.squamata.field.Complex] = [[i, 3.0+2.0*i], [1.0+1.0*i, 0.0]]

scala> val v = DenseVector(Complex.i, Complex(1.0))
v: org.draegisoft.squamata.vectorspace.DenseVector[org.draegisoft.squamata.field.Complex] = [i, 1.0]

scala> A*v
res0: org.draegisoft.squamata.vectorspace.DenseVector[org.draegisoft.squamata.field.Complex] = [2.0+2.0*i, -1.0+1.0*i]
```
When you construct a matrix from a list of vectors, these vectors are interpreted as row vectors. 
When you construct a sparse matrix, this also applies and you only need to specify row vectors that are not the origin.

Other than complex numbers in the example above, implementations for real numbers, rationals, and the GF(2) 
are available right now.
