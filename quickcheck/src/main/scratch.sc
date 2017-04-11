import quickcheck.{BinomialHeap, Bogus4BinomialHeap, IntHeap, QuickCheckHeap}


val h = new Object with IntHeap with Bogus4BinomialHeap

//val g: List[h.Node] = h.insert(3,h.empty)

val xs = Seq(6, 4, 5)

val empty = List[h.Node]()

val hh: List[h.Node] =
  xs.foldLeft(empty)( (h1,n) => h.insert(n, h1) )

def extract(heap: List[h.Node]): List[Int] =
  if (h.isEmpty(heap)) Nil else
  h.findMin(heap) :: extract(h.deleteMin(heap))

//h.findMin(hh)

//h.findMin(h.deleteMin(hh))

val extracted = extract(hh)
(extracted.length, xs.length)

