package fp

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  // E5.1
  def toList: List[A] = this match
    case Empty      => List.empty[A]
    case Cons(h, t) => h() +: t().toList

  // E5.2
  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if (n > 0) => LazyList.cons(h(), t().take(n - 1))
    case _                     => LazyList.empty

  // E5.2
  def drop(n: Int): LazyList[A] = this match
    case s @ Cons(h, t) if (n > 0) => t().drop(n - 1)
    case _                         => LazyList.empty

    // E5.3
  def dropWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if (p(h())) => LazyList.cons(h(), t().dropWhile(p))
    case _                      => LazyList.empty

object LazyList:

  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty else cons(as.head, apply(as.tail*))
