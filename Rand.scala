package fp

trait RNG:
  def nextInt: (Int, RNG)

// E6.1
def nonNegativeInt(rng: RNG): (Int, RNG) =
  val (i, r) = rng.nextInt
  (if (i < 0) then -(i + 1) else i, r)

// E6.2
def double(rng: RNG): (Double, RNG) =
  val (i, r) = nonNegativeInt(rng)
  (i.toDouble / (Int.MaxValue + 1), r)

// E6.3
def intDouble(rng: RNG): ((Int, Double), RNG) =
  val (i, r0) = rng.nextInt
  val (d, r1) = double(r0)
  ((i, d), r1)

def doubleInt(rng: RNG): ((Double, Int), RNG) =
  val ((i, d), r) = intDouble(rng)
  ((d, i), r)

def double3(rng: RNG): ((Double, Double, Double), RNG) =
  val (d0, r0) = double(rng)
  val (d1, r1) = double(r0)
  val (d2, r2) = double(r1)
  ((d0, d1, d2), r2)

// E6.4
def ints(count: Int)(rng: RNG): (List[Int], RNG) =
  val l = (1 to count).foldLeft(List(rng.nextInt))((intAndRngs, _) =>
    intAndRngs :+ intAndRngs.last._2.nextInt
  )
  (l.map(_._1), l.last._2)

type Rand[+A] = RNG => (A, RNG)

val int: Rand[Int] = rng => rng.nextInt

def unit[A](a: A): Rand[A] = rng => (a, rng)

def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
  rng =>
    val (a, r) = s(rng)
    (f(a), r)

// E6.5
def doubleWithMap: Rand[Double] =
  map(nonNegativeInt)(_.toDouble / (Int.MaxValue + 1))

// E6.6
def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  rng =>
    val (a, r0) = ra(rng)
    val (b, r) = rb(r0)
    (f(a, b), r)

// E6.7
def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = 
  rs.foldRight(unit(List.empty[A]))((r, l) => map2(r, l)(_ :: _))


