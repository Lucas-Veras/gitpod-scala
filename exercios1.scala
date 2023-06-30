import io.StdIn.readInt

// Implemente as funções a seguir usando uma abordagem funcional
//   - sem usar 'var'
//   - Substitua ??? pela implementação de cada função

def maior(a: Int, b: Int) = if (a > b) a else b
def mdc(a: Int, b: Int): Int = {
  if (b == 0) a
  else mdc(b, a % b)
}
def somaRecursiva(lista: List[Int]): Int = {
  lista match {
    case Nil => 0  
    case head :: tail => head + somaRecursiva(tail)  
  }
}
def somaFold(lista: List[Int]): Int = {
  lista.foldRight(0)((a:Int, b:Int) => a + b)
}
def tamanhoRecursivo(lista: List[Int]): Int = {
  lista match {
    case Nil => 0
    case head :: tail => 1 + tamanhoRecursivo(tail)
  }
}
def pares(lista: List[Int]) = {
  lista.filter(_%2 == 0)
}
def maiorMenor(lista: List[Int]): (Int, Int) = (lista.max, lista.min)
def pertence(lista: List[Int], valor: Int) = lista.contains(valor)
def mediaPonderada(notas: List[Int], pesos: List[Int]) = {
  notas.zip(pesos).foldLeft(0)((a:Int, b:(Int,Int)) => a + b._1 * b._2)/pesos.foldLeft(0)((a:Int,b:Int) => a + b)
}

@main
def principal =
  println("Digite dois números:")
  val a, b = readInt()
  val lista = List(4,3,1,8,7,5,6)

  println(s"O maior número é ${maior(a,b)}")
  println(s"O mdc de $a e $b é ${mdc(a, b)}")
  println(s"A soma dos elementos da lista $lista é ${somaRecursiva(lista)}.")
  println(s"A soma dos elementos da lista $lista é ${somaFold(lista)}.")
  println(s"O tamanho da lista $lista é ${tamanhoRecursivo(lista)}.")
  println(s"Os números pares são ${pares(lista)}.")
  println(s"O maior número da lista $lista é ${maiorMenor(lista)._1}, o menor número é ${maiorMenor(lista)._2}.")
  println(s"O valor $a pertence a lista $lista: ${pertence(lista, a)}")
  println(s"A média ponderada da $lista é ${mediaPonderada(lista,lista)}")