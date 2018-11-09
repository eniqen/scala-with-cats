import cats.Eval

def foldRight[A, B](list: List[A], acc: B)(f:(A, B) => B): B = list match {
  case Nil => acc
  case x :: xs => f(x, foldRight(xs, acc)(f))
}

def evalFoldRight[A, B](list: List[A], acc: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = list match {
  case Nil => acc
  case x :: xs => Eval.defer(f(x, foldRight(xs, acc)(f)))
}

def foldRightLazy[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
  evalFoldRight(as, Eval.now(acc)) { (a, b) =>
  b.map(x => fn(a, x))
  }.value