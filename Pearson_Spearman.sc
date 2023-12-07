case class tweets (usuario: String, follower: Int, friends: Int)

val lista: List[tweets] = List(
  tweets("1108734367684276231",88, 125),
  tweets("1108734260763074561",309,656),
  tweets("1108734212473917442",516, 125),
  tweets("1108733839755501569",23571, 23791),
  tweets("1108733839755501569",4477, 3717)
)

def pearson(list: List[tweets]): Double = {
  val list_follower: List[Int] = list.map(_.follower)
  val list_friends: List[Int] = list.map(_.friends)

  val promediox: Double = list_follower.sum.toDouble / list_follower.size
  val promedioy: Double = list_friends.sum.toDouble / list_friends.size

  def media_y_multiplicacion(): Double = {
    val productos = (0 until list.length)
      .map(i => (list_follower(i) - promediox) * (list_friends(i) - promedioy))
    productos.sum
  }

  def desviacion(lista2: List[Int], promedio: Double): Double = {
    math.sqrt(lista2.map(k => math.pow(k - promedio, 2)).sum)
  }

  val numerador = media_y_multiplicacion
  val denominador = desviacion(list_follower, promediox) * desviacion(list_friends, promedioy)

  numerador / denominador
}

def spearman(list: List[tweets]): Double = {
  val list_follower: List[Int] = list.map(_.follower)
  val list_friends: List[Int] = list.map(_.friends)

  def lista_valor(lista: List[Int]): List[Int] = {
    lista.map(k => lista.count(_ < k) + 1)
  }

  val list_follower_2: List[Int] = lista_valor(list_follower)
  val list_friends_2: List[Int] = lista_valor(list_friends)

  def suma(): Double = {
    val productos = (0 until list.length)
      .map(i => math.pow(list_follower_2(i) - list_friends_2(i), 2))
    productos.sum
  }

  1 - ((6 * suma) / (list.length * (math.pow(list.length,2) - 1)))
}

val pearson1 = pearson(lista)
val spearman2 = spearman(lista)