package postmodern1
import scala.collection.mutable

enum List[+A]:
  case Nil
  case Cons(hd: A, tl: List[A])
  override def toString(): String = {
    def go(s: mutable.StringBuilder, as: List[A]): String = {
      as match {
        case List.Nil =>
          s.append("]").result
        case List.Cons(h, t) =>
          go(s.append(h).append(if t != List.Nil then ", " else ""), t)
      }
    }
    go(new mutable.StringBuilder("["), this)
  }
  def reverse: List[A] = {
    def go(xs: List[A], r: List[A]): List[A] = 
      xs match {
        case List.Nil => r
        case List.Cons(h, t) => go(t, List.Cons(h, r))
    }
    go(this, List.Nil)
  }

  def contains[A](a: A): Boolean= {
    def cont[A](xs: List[A], a: A): Boolean = {
      xs match{
        case List.Nil => false
        case List.Cons(h,t) =>
          if a == h then true
          else cont(t, a)
      }
    }
    cont(this, a)
  }

object List:
  def apply[A](xs: A*) = of(xs*)
    def of[A](xs: A*): List[A] ={
      xs.foldRight(Nil: List[A]) { 
      case (x, acc) => Cons(x, acc)
      }
    }
  
  //spans
  def spans[A](xs: List[A], sep: A): List[List[A]] = {
    def go(r: List[List[A]], buf: List[A], xs: List[A], sep: A): List[List[A]] = {
      xs match {
        case List.Nil => List.Cons(buf, r)
        case List.Cons(h,t) =>
          if h == sep then go(List.Cons(buf, r), List.Nil, t, sep)
          else go(r, List.Cons(h, buf), t, sep)
      }
    }
    def rev(xs: List[List[A]]): List[List[A]] = { 
      xs match{
        case List.Nil => List.Nil
        case List.Cons(h,t) => List.Cons(h.reverse, rev(t))
      }
    }
    rev(go(List.Nil, List.Nil, xs, sep)).reverse
  }
    
  //intesert
  def intersect[A](xs: List[A], ys: List[A]): List[A] = {
    def found[A](xs: List[A], ys: List[A], zs: List[A]): List[A] = {
      xs match{
        case List.Nil => zs
        case List.Cons(h, t) =>
          if ys.contains(h) then found(t, ys, List.Cons(h, zs))
          else found(t, ys, zs)
      }
    }
    found(xs, ys, List.Nil)
  }

  //intersert2.0
  def intersectBy[A](xs: List[A], ys: List[A], f: (A) => A): List[A] = {
    def found[A](xs: List[A], ys: List[A], zs: List[A],f: (A) => A ): List[A] = {
      xs match{
        case List.Nil => zs
        case List.Cons(h, t) =>
          if ys.contains(f(h)) then found(t, ys, List.Cons(h, zs), f)
          else found(t, ys, zs, f)
      }
    }
    found(xs, ys, List.Nil, f)
  }

def sequence[A](xs: List[Either[String, A]]): Either[String, List[A]] = {
  def go(xs: List[Either[String, A]], r: List[A]): Either[String, List[A]] = {
      xs match {
        case List.Nil => Right(r)
        case List.Cons(h,t) =>
          h match{
            case Left(w) => Left(w)
            case Right(w) => go(t, List.Cons(w, r))
          }
      }
  }
  go(xs, List.Nil)
}

@main def run() =
  println("Hello")