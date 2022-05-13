object Matrix {
  // define your functions here
  type Img = List[List[Int]]
  type Line = List[Int]

  def show(m: Img): String = {
    m.map(line => line.foldRight("\n")(_.toString+" "+_))
      .reduce(_ + _)
  }

  def hFlip(img: Img): Img = for (line <- img) yield line.reverse

  def vFlip(img: Img): Img = for (line <- img.reverse) yield line

  def rot90Left(img: Img): Img = {
    def transpose (m: Img): Img =
      m match {
        case Nil :: _ => Nil
        case _ => m.map(_.head) :: transpose(m.map(_.tail))
      }

    transpose(hFlip(img))
  }

  def rot90Right(img: Img): Img = hFlip(vFlip(rot90Left(img)))

  //Write a function which inverts an image (values 0 become 255, 1 - 254, and so forth).
  def invertImage(img: Img): Img =
    for (line <- img)
      yield for (elem <- line)
        yield 255 - elem

  def cropAt(img: Img, xSt:Int, ySt:Int, xEnd: Int, yEnd: Int): Img =
    for (line <- img.slice(ySt, ySt + yEnd))
      yield for (elem <- line.slice(xSt, xSt + xEnd))
        yield elem

  def largerPos(img: Img, int: Int): List[(Int,Int)] = {
    val flattened = img.flatten
    (for (x <- flattened.indices if flattened(x) > int)
      yield (x / img.head.length, x % img.length)).toList
  }

  def contrast(x: Int)(img: Img): Img =
    for (line <- img)
      yield for (elem <- line)
        yield x + elem

  def hglue(img1: Img, img2: Img): Any =
    for (line1 <- img1; line2 <- img2)
      yield line1 +: line2


  def main(args: Array[String]): Unit = {
    val m = List(List(1,2,3), List(4,5,6), List(7,8,9))

    println(show(m))
    println(show(hFlip(m)))
    println(show(vFlip(m)))
    println(show(rot90Left(m)))
    println(show(rot90Right(m)))
    println(show(invertImage(m)))

    val img = List(List(0,0,1,0,0), List(0,1,0,1,0), List(0,1,1,1,0), List(1,0,0,0,1), List(1,0,0,0,1))
    /*
    *     0 0 1 0 0
    *     0 1 0 1 0                                           1 0 1
    *     0 1 1 1 0     cropping from 1,1  to  3,2  yields:   1 1 1
    *     1 0 0 0 1
    *     1 0 0 0 1

   */
    println(show(cropAt(img, 1, 1, 3, 2)))
    println(largerPos(m, 2))
    println(hglue(m, m))

  }
}