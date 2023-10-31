import org.scalatest.wordspec.AnyWordSpec

class LineBreackerSpec extends AnyWordSpec{
  "Linebreacker Word splitter function" when {
    "try to split a string by spaces to get WordEntries" should{
      "two words" in{
        val result = Main.splitWords("word1 word2")
        assert(result.size == 2)
        assert(result.head.len == 5)
        assert(result.head.first == 0)
        assert(result.head.last == 4)
      }
      "long words" in {
        val result = Main.splitWords("kdmcowienc09w8hecoiwnec9823nciwondcoiwj8ec oiwudnviwuenvcwiuwñdiuncwpiuncw " +
          "wpjcnwpieuncpwieucnwpkjscnpw89ehfd3p49uef3489fhw'p9iudc")
        assert(result.size == 3)
        assert(result.head.len == 42)
        assert(result.head.first == 0)
        assert(result.head.last == 41)
      }
      "long text" in {
        val s = "In 1991, while studying computer science at University of Helsinki, Linus Torvalds began a project that later became the Linux kernel. He wrote the program specifically for the hardware he was using and independent of an operating system because he wanted to use the functions of his new PC with an 80386 processor. Development was done on MINIX using the GNU C Compiler."
        val result = Main.splitWords(s)
        assert(result.size == 63)
        assert(result.head.len == 2)
      }
    }
    "cutting in lines a text " should {
      "split simple text with same word length" in {
        val words = Main.splitWords("easy text")
        val result = Main.splitLines(words,4)
        assert(result.size == 2)
        assert(result.head.len == 4)
      }
      "split simple text with diff word length" in {
        val words = Main.splitWords("easyasdfe text")
        val result = Main.splitLines(words, 4)
        assert(result.size == 2)
        assert(result.head.len == 9)
      }
      "split text with long words" in {
        val words = Main.splitWords("pokmedv90herv9uehdv08uewdñockmwdocpiwnc klmcpwqincpwkdsmcnaposckmapdiojcnwpdkcnwpiudnc")
        val result = Main.splitLines(words, 4)
        assert(result.size == 2)
        assert(result.head.len == 39)
        assert(result.last.len == 46)
      }
      "split text with more lines" in {
        val words = Main.splitWords("pokmedv90her v9uehdv08uewdño ckmwdocpiwnc klmcpwqin cpwkdsmcnaposc kmapdiojcnw pdkcnwpiudnc oubh ouybouygouy ouyguoiygouyg iytcu8rx")
        val result = Main.splitLines(words, 30)
        assert(result.size == 5)
        assert(result.head.len == 28)
      }
      "split real text" in {
        val s = "In 1991, while studying computer science at University of Helsinki, Linus Torvalds began a project that later became the Linux kernel. He wrote the program specifically for the hardware he was using and independent of an operating system because he wanted to use the functions of his new PC with an 80386 processor. Development was done on MINIX using the GNU C Compiler."
        val words = Main.splitWords(s)
        val result = Main.splitLines(words, 40)
        assert(result.size == 10)
        assert(result.head.len == 40)
        assert(result.last.len == 31)
        assert(result(4).len == 40)
      }
    }
  }

}
