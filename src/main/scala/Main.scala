import scala.::

case class WordEntry(first : Int, last : Int, len : Int, next : Int )
case class LineEntry(len : Int, start : Int, end : Int)
object Main {
  def main(args: Array[String]): Unit = {

    val loremText = "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
    val s = "In 1991, while studying computer science at University of Helsinki, Linus Torvalds began a project that later became the Linux kernel. He wrote the program specifically for the hardware he was using and independent of an operating system because he wanted to use the functions of his new PC with an 80386 processor. Development was done on MINIX using the GNU C Compiler."

    println(processString(loremText, 40))
    println(processString(s, 40))
    println(processString("easy text", 4))

  }

  def processString(text : String, maxSize : Int): String = {
    val words = splitWords(text)
    splitLines(words, maxSize).map(line => text.substring(line.start,line.end + 1)).mkString("\n")
  }

  def splitWords(text : String) : Seq[WordEntry] = {
    val textSize = text.size
    val (seq, _, _) = text.foldLeft((Seq[WordEntry](), WordEntry(0, 0, 0, 0), 0)) { (acc, e) =>
      val (seqWords, prevWord, idx) = acc
      if (e == ' ' || idx == textSize - 1) {
        if (prevWord.last == 0) {
          val entry = WordEntry(0, idx - 1, idx, -1)
          (seqWords :+ entry, entry, idx + 1)
        } else if (idx == textSize - 1) {
          val entry = WordEntry(prevWord.last + 2, idx, idx - (prevWord.last + 1), -1)
          (seqWords :+ entry, entry, idx + 1)
        }
        else {
          val entry = WordEntry(prevWord.last + 2, idx - 1, idx - (prevWord.last + 2), -1)
          (seqWords :+ entry, entry, idx + 1)
        }
      } else
        (seqWords, prevWord, idx + 1)
    }
    seq
  }
  def splitLines(words : Seq[WordEntry], maxLen : Int) : Seq[LineEntry] = {
    def aux(words : Seq[WordEntry], maxLen : Int, accLen : Int, acc : Seq[LineEntry], startLine : Int, prev : WordEntry) : Seq[LineEntry] = {
      words match {
        case last :: Nil =>
          acc :+ LineEntry(last.len + accLen, startLine, last.last)
        case h1 :: tail =>
          if(h1.len >= maxLen && accLen == 0)
            aux(tail, maxLen, 0, acc :+ LineEntry(h1.len, startLine, h1.last), h1.last + 2, h1)
          else if(h1.len + 1 + accLen > maxLen)
            aux(tail, maxLen, h1.len , acc :+ LineEntry(accLen, startLine, prev.last), h1.first, h1)
          else if(h1.len + 1 + accLen == maxLen)
            aux(tail, maxLen, 0, acc :+ LineEntry(h1.len + 1 + accLen, startLine, h1.last), h1.last + 2, h1)
          else {
            if(accLen == 0 && acc.isEmpty)
              aux(tail, maxLen, h1.len, acc, startLine, h1)
            else
              aux(tail, maxLen, accLen + h1.len + 1, acc, startLine, h1)
          }
      }
    }
    aux(words,maxLen,0,Seq[LineEntry](),0,words.head)
  }
}
