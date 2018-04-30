package nclogic.binary

object CRC {

  def xor(b1: Boolean, b2: Boolean): Boolean = b1 != b2

  def calculateSum(list: List[Boolean], encoder: List[Boolean]): List[Boolean] = {
    val input = List(list, List.fill(encoder.length - 1)(false)).flatten.toArray

    for (i <- list.indices) {
      if (input(i)) {
        for (j <- encoder.indices) {
          input(i + j) = xor(input(i + j), encoder(j))
        }
      }
    }

    input.takeRight(encoder.length - 1).toList
  }

  def validate(list: List[Boolean], encoder: List[Boolean]): Boolean = {
    val input = list.toArray
    for (i <- 0 until (list.length - encoder.length+1)) {
      if (input(i)) {
        for (j <- encoder.indices) {
          input(i + j) = xor(input(i + j), encoder(j))
        }
      }
    }

    input.takeRight(encoder.length - 1).forall(x => !x)
  }

  def encode(list: List[Boolean], encoder: List[Boolean]): List[Boolean] = list ::: calculateSum(list, encoder)

  def main(args: Array[String]): Unit = {
    val input = "11010011101110".map(c => c == '1').toList
    val encoder = "1011".map(c => c == '1').toList

    val sum = calculateSum(input, encoder)

    val result = validate(input ++ sum, encoder)
    println(result)
  }

}
