package fp

import fp.LazyList.cons
import fp.LazyList.empty

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  // E5.1
  def toList: List[A] = this match
    case Empty      => List.empty[A]
    case Cons(h, t) => h() +: t().toList

  // E5.2
  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if (n > 0) => cons(h(), t().take(n - 1))
    case _                     => empty

  // E5.2
  def drop(n: Int): LazyList[A] = this match
    case s @ Cons(h, t) if (n > 0) => t().drop(n - 1)
    case _                         => empty

  // E5.3
  def dropWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if (p(h())) => cons(h(), t().dropWhile(p))
    case _                      => empty

  def foldRight[B](acc: => B)(f: (A, => B) => B): B = this match
    case Cons(h, t) => f(h(), t().foldRight(acc)(f))
    case _          => acc

  def exists(p: A => Boolean): Boolean =
    this.foldRight(false)((a, acc) => p(a) || acc)

  // E5.4
  def forAll(p: A => Boolean): Boolean =
    this.foldRight(true)((a, acc) => p(a) && acc)

  // E5.5
  def takeWhile(p: A => Boolean): LazyList[A] =
    this.foldRight(empty)((a, acc) => if (p(a)) then cons(a, acc) else empty)

  // E5.6
  def headOption: Option[A] =
    this.foldRight(Option.empty[A])((a, _) => Option(a))

  // E5.7
  def map[B](f: A => B): LazyList[B] =
    this.foldRight(empty[B])((a, acc) => cons(f(a), acc))

  // E5.7
  def filter(f: A => Boolean): LazyList[A] =
    this.foldRight(empty[A])((a, acc) => if (f(a)) then cons(a, acc) else acc)

object LazyList:

  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty else cons(as.head, apply(as.tail*))
