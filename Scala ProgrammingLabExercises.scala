~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	Scala Programming Language Exercises 
	Author: Welemhret Welay Baraki
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//2.3. Write a function that returns the maximum element from a list. 
def max(l:List[Int]):Int =
{
	if(l.length==1) l.head
	else if(l.head > l.tail.head && l.tail.length >= 1) max(l.head::l.tail.tail)
	else if(l.head < l.tail.head) max(l.tail)
	else throw new Error("Empty List") 
}

max(List(240,5,10,11,9,7,34, 3))
//2.4. Write a reverse function to reverse the elements of a list
def reverse(l:List[Int]):List[Int]=
	l match {
	case h::t => reverse(t):::List(h)
	case Nil => Nil
	}
//Tail Recursive Form
def reverse(l:List[Int]):List[Int]=
   {
	   def loop(lst:List[Int],reversed:List[Int]):List[Int]={
		lst match {
		case h::t => loop(t,h::reversed)
		case Nil => reversed
		}
		}
	loop(l,Nil)
   }
//2.5. Write a function concatenate of two different lists    
def concat(Lst1:List[Int],Lst2:List[Int]):List[Int]=
{
	Lst1 match {
	case Nil => Lst2 
	case h::t => h::concat(t,Lst2)
	}
}
//2.6. Write a zip function of the form (List[Int],List[Int])-> List[(Int,Int)]
def zip(lst1:List[Int],lst2:List[Int]):List[(Int,Int)]=
	{
		(lst1,lst2) match {
		case (Nil,Nil) => Nil
		case (Nil,h::t) => throw new Error("Can't Zip")
		case (h::t,Nil) => throw new Error("Can't Zip")
		case (h::t,hh::tt) =>(h,hh)::zip(t,tt)
		}
	}   
 //2.7. Has Subsequence 
 def subSeq(seq:List[Int],sub:List[Int]):Boolean={
 	     seq match {
		case Nil => false 
		case hh::tt=> sub match {
			case Nil  => true
			case h::t => h==hh && subSeq(tt,t) 
		}			
	   }
	}

//2.8. The Nth smallest number which takes a list of integers and a number and returns  the number
def nSmallest(ns:List[Int],n:Int):Int ={
	 def isort(xs:List[Int]):List[Int] =xs match { //Insertion Sort
		case List() => List()
		case x::xs  => insert(x,isort(xs)) //Inserts the element at the appropraite position 
	}
	def insert (x:Int, xs:List[Int]): List[Int] = xs match {
		case List() => List(x)
		case y::ys  => if(x<=y) x::xs else y::insert(x,ys)
	}
    if(n>ns.length) throw new Error("Index Out of Bound")
    else (isort(ns))(n-1)
}
//2.9 Standard deviation
def sum(Lst:List[Int]):Int={
	if(Lst==Nil) 0
	else Lst.head + sum(Lst.tail) 
}
def mean(ls:List[Int]):Double=
{
	sum(ls)/ls.length
}
def std(mean:Double,xi:Int):Double={

}
def STD(Lst:List[Int]):Int =
{
	val mn=mean(Lst)	
}
//2.11. dot product of two vectors (Tail Recursive)
def dot(ns1:List[Int],ns2:List[Int]):Int ={
	def loop(ls1:List[Int],ls2:List[Int],acc:Int):Int=
	{
		(ls1,ls2) match {
		  case (Nil,Nil) => acc
		  case (h::t,hh::tt) => loop(t,tt,h*hh +acc)
		}	
	}
	loop(ns1,ns2,0)
}

//3. Higher-order functions
//3.1. Create a function count which takes a list of integers, and a function of the form Int → Boolean 
def count(ns:List[Int],f:Int=>Boolean):Int={
	if(ns==Nil) 0
	else if(f(ns.head)) 1 +count(ns.tail,f)
	else count(ns.tail,f)
} 
//3.2. Map - Create a function map which takes a list of integers, and a function of the form Int → Int; and returns a new list with values transformed by the given function.
def map(lst:List[Int], f:Int => Int):List[Int]=
{
	if(lst==Nil) lst  //same as Nil
	else f(lst.head)::map(lst.tail, f)
}

// = List(1, 4, 9, 16, 25)
// = List(1, 8, 27, 64, 125)
//3.3 Sorted -Write a function sorted which checks whether a list of integers is sorted according to a comparison function of the form (Int, Int) → Boolean.
def sorted(ns:List[Int],f:(Int,Int)=>Boolean):Boolean=
	ns match {
	case _::Nil => true 
	case h::t =>f(h,t.head) && sorted(t,f)
	}
sorted(List(1, 2, 3, 4, 5), (a, b) => a < b)
sorted(List(1, 2, 3, 4, 5, 3), (a, b) => a < b)
//3.4. Filter-Write a function filter which takes a list of integers and a function, and returns a new list with only those values which evaluate to True according to the given function.
def filter(ns:List[Int],f:Int=>Boolean): List[Int]=
{
     ns match {
	case Nil => Nil 
	case h::t if(f(h)) => h::filter(t,f)
	case h::t if(!f(h)) =>filter(t,f)
	}	
}
//3.5 Partition-Write a function partition which takes a list of integers and a function of the form Int → Boolean and returns a tuple with to lists, one of elements that evaluate to true (as given by the function), and those to false.
def partition(ns:List[Int],f:Int =>Boolean):(List[Int],List[Int])=
{   //Tail-Recursive Functions
  def loop(ns1:List[Int],part1:List[Int],part2:List[Int]):(List[Int],List[Int]) =
  {
  	if(ns1==Nil) (part1.reverse,part2.reverse)
  	else if(f(ns1.head)) loop(ns1.tail,ns1.head::part1, part2)
  	else  	loop(ns1.tail,part1, ns1.head::part2)
  }
  loop(ns,Nil,Nil)
}
//3.6 Reduce  -Write a function reduce which takes a list of integers and a function, and returns an integer resulting from having combined all values of the list using the given function.
def reduce(ls:List[Int],f:(Int,Int)=>Int):Int ={
	ls match {
	case Nil => throw new Error("Error")
	case h::Nil => h
	case h::t => f(h,reduce(t,f))
	}
}
//3.7. Fold -write a function fold which takes an initial value, a list of integers and a function and returns an integer resulting from having combined all values of the list. 
def fold(n:Int,ns:List[Int],f:(Int,Int)=>Int):Int= 
{
    ns match {
      case Nil  => n
      case h::t => fold(f(n,h),t,f)
    }
}
//3.8. Merge and reduce
def mergeReduce(a:List[Int],b:List[Int],merge:(Int,Int)=>Int,reduce:(Int,Int)=>Int):Int={
	//Refer to the Lecture Notes 
}
//3.9. Bisection method
def SignOpp(x:Double,y:Double):Boolean=
{
	if (x < 0 && y > 0) true
	else if (x > 0 && y < 0) true
	else false
}

def bisection(a:Double,b:Double,E:Double,f:Double=>Double):Double=
{
	if(Math.abs(a-b) <= E) {a}
	else {
	   val nextGuess=(a+b)/2.0
	   if(SignOpp(f(nextGuess), f(a))) bisection(a,nextGuess,E,f)
	   else bisection(nextGuess,b,E,f)
	}
}

val ffx2n1: (Double => Double) = (x) => {(x*x)-1}
val ffx2n2: (Double => Double) = (x) => {(x*x)-2}
bisection (-2.0,2.0,0.0001,ffx2n1) 
bisection (-2.0,2.0,0.0001,ffx2n2) 
bisection (0.0,2.0,0.0001,ffx2n2)
//4.3. Scale 
def scale(min:Double,max:Double)=(a:Double,b:Double) =>(x:Double)=>(((b-a)*(x-min))/(max-min)) + a
val scale=(min:Double,max:Double)=>(a:Double,b:Double) =>(x:Double)=>(((b-a)*(x-min))/(max-min)) + a

//5. Streams (LazyList)
//5.1. Natural Numbers- Write a function numbersFrom which takes an integer and returns a LazyList with the natural numbers starting from the given number.
def numbersFrom(n:Int):LazyList[Int] ={ 
	  n#::numbersFrom(n+1)
	}
//5.2 Fibonacci stream -Write a function fibonacciStream with no parameters which returns a LazyList with the Fibonacci sequence.
def fibonacciStream:LazyList[Int]= {
	def loop(a:Int,b:Int):LazyList[Int]= a#::loop(b,a+b)
	loop(0,1)
}
//5.3. Moving Average 
def MovingAverage(n:Int,ls:LazyList[Int]):LazyList[Double]={
		val a= ls.take(n).sum/n 
		a#::MovingAverage(n,ls.drop(n))
		} 
val ns = numbersFrom(1)
MovingAverage(5, ns).take(3).toList
MovingAverage(4, ns).take(3).toList

//5.4. Leibniz sequence for Pi -Write a function leibnizStream with no parameters, which returns a LazyList with the values for the Leibniz sequence.
def leibnizStream():LazyList[Double]= {
    def loop(k:Int,ls:Double):LazyList[Double]= ls#::loop(k+1,Math.pow(-1,k)/(2*k+1))
loop(1,1)
}
//5.5. Sieve of Eratosthenes Write a function primesStream with no parameters which returns a LazyList of only prime numbers. Note: Built-in functions allowed.
def sieve(s: LazyList[Int]): LazyList[Int] = 
	s.head #:: sieve(s.tail filter (_ % s.head != 0))
	
val ns = numbersFrom(1)
sieve(ns).take(10).toList
//6. Functional data structures
//6.1 Binary tree
//Some Function implementations
def min:Int ={
	def loop(t:Tree, m:Int):Int={
		if(isEmpty) m
		else loop(left,value)
	}
if(isEmpty) throw new Exception("Empty Tree")
else loop(left,value)
}
def max:Int ={
def loop(t:Tree, m:Int):Int=
{
	if(t.isEmpty) m
	else loop(right,value)
}
if(isEmpty) throw new Exception("Empty Tree")
else loop(right,value)
}
def remove(x:Int):Tree=
	if(isEmpty)  leaf
	else if(x<value) Branch(value,left.remove(x),right)
	else if(x>value) Branch(value,left,right.remove(x))
	else {
		if(left.isEmpty && right.isEmpty) leaf
		else if(right.isEmpty) left
		else if(left.isEmpty) right
		else {
		val succ=right.min
		Branch(succ,left,right.remove(succ))
		}
	}

def apply(n:Int):Tree =
   {
   if(isEmpty) fail("The Tree doesn't Contain a "+ n " th element")
   else if(n<left.size) left.apply(n) 
   else if(n>left.size) right.apply(n-size-1)
   else value
   }
 abstract sealed class Tree{
	def isEmpty:Boolean
	def add(x:Int):Tree
	def toList:List[Int] //returns an ordered list with the elements in the tree.
	//find : takes a function f , and returns Some(e) with the first element e for
	//the function holds (equals true), and None if it yields false for all elements.
	//count : counts the number of elements in the tree.
	//maxDepth : returns the length of the longest path from the root to a leaf.
	def dfs:List[Int]
}

case object leaf extends Tree{
	def isEmpty:Boolean=true
	def add(x:Int):Tree= throw new Exception("No value/Tree to Insert")
	def toList:List[Nothing] =throw new Error("The Tree is Empty!")
	def dfs:List[Int]=throw new Error("The Tree is Empty!")
}

case class Branch(value:Int, left:Tree =leaf, right:Tree =leaf) extends Tree{
	def isEmpty:Boolean =false
	def add(x:Int):Tree={
		if(isEmpty) Branch(x)
		else if(x<value) Branch(value,left.add(x),right)
		else if(x>value) Branch(value,left,right.add(x))
		else this
	}
	def toList:List[Int]=throw new Exception("Empty Tree")
	//if(isEmpty) leaf
	//else value::this.
	//}
 	def dfs:List[Int]={
 		def loops(s:List[Tree]):List[Int]={
 		if(isEmpty) Nil 
 		else if(s.head.isEmpty) loop(s.tail)
 		else s.head.value::loop(s.head.right::s.head.left::s.tail)
 		}
 	   loop(List(this))
 	}
 	def valuesByBreadth: List[Int] = {
		import scala.collection.immutable.Queue
	   def loop(q: Queue[Tree]): List[Int] =
		if (q.isEmpty) Nil
		else if (q.head.isEmpty) loop(q.tail)
		else q.head.value :: loop(q.tail :+ q.head.left :+ q.head.right)
	loop(Queue(this))
	}
	def invert: Tree =
		if (isEmpty) Leaf else Branch(-value, right.invert, left.invert)
	
	
	
	
	
}
val a:Tree =Branch(1,Branch(5),Branch(6))
//6.2 MySet
//Sets are collections of elements which do not re-occur, that is, a collection of unique elements. Consider the following definition of a functional data structure for sets:

trait MySet {
	def e:Int => Boolean
	def contains(x:Int):Boolean 
	def union(s:MySet):MySet 
	//def intersect(s:Int => Boolean, t:Int => Boolean):Int => Boolean 
}
class NonEmpty(val e:Int=>Boolean) extends MySet {
	def contains(elem:Int):Boolean =e(elem)
	def union(s:MySet):MySet = 
		{
			if(e(s)
		}
	def intersect(s:Int => Boolean, t:Int => Boolean):Int => Boolean =
				(elem:Int)=>s(elem) && t(elem)
	def add(
}
object Empty extends MySet{
	val e:Int =>Boolean = _ => false
	def contains(elem:Int):Boolean =false
	def union(s:Int => Boolean, t:Int => Boolean):Int => Boolean = (elem:Int)=> false
}
object MySet{
	def apply(n:Int):MySet =new NonEmpty(x=>x==n)
}

/**
   * Exercise 2 -Parentheses Balancing
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceCheck(Lst:List[Char],state:Int):Boolean ={
      if(Lst.isEmpty) state ==0
      else if( Lst.head == '(' )  balanceCheck(Lst.tail,state+1)
      //Warnning: Don't use "this quote" for chars instead use 'this',
      //use the former for strings because comparison always results false.
      else if( Lst.head == ')' )  state>0 && balanceCheck(Lst.tail,state-1)
      else balanceCheck(Lst.tail,state)
    }
    balanceCheck(chars,0)
  }

  /**
   * Exercise 3-Countchange using coins.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money<0 || coins.isEmpty) 0
    else if(money==0) 1
    else countChange(money,coins.tail) + countChange(money-coins.head,coins)
  }
//Quicksort
//Write a function quicksort which takes a list of integers and returns its sorted version.

 def qsort(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case head :: tail => {
        val (low, high) = tail.partition(_ < head)
        qsort(low) ::: head :: qsort(high)
    }
}

