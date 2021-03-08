package net.dafttech.filters4s.parser.implicits

import java.util.regex.Pattern

import fastparse.NoWhitespace._
import fastparse._

trait ParserUtils {
  def isSpace(char: Char): Boolean = Pattern.matches("\\s", String.valueOf(char))

  def space[_: P]: P[Unit] = CharPred(isSpace).opaque("space")

  def s[_: P]: P[Unit] = CharsWhile(isSpace, 0).opaque("spaces?")

  def s1[_: P]: P[Unit] = CharsWhile(isSpace, 1).opaque("spaces")

  def isLetter(char: Char): Boolean = (('A' to 'Z') ++ ('a' to 'z') ++ "äöüß").contains(char)

  def letter[_: P]: P[Unit] = CharPred(isLetter).opaque("character")

  def digit[_: P]: P[Unit] = CharPred(CharPredicates.isDigit).opaque("digit")

  def digits[_: P]: P[Unit] = CharsWhile(CharPredicates.isDigit).opaque("digits")

  def isSeparator(char: Char): Boolean = ",;".contains(char)

  def separator[_: P]: P[Unit] = CharPred(isSeparator).opaque("separator")

  def isQuote(char: Char): Boolean = "'\"".contains(char)

  def quote[_: P]: P[Unit] = CharPred(isQuote).opaque("quote")

  def isEscape(char: Char): Boolean = "\\".contains(char)

  def escape[_: P]: P[Unit] = CharPred(isEscape).opaque("escape character")

  def number[_: P]: P[BigDecimal] = ("-".? ~ (digits ~ ("." ~ digits).? | "." ~ digits)).!
    .map(BigDecimal.apply).opaque("number")

  def long[_: P]: P[Long] = ("-".? ~ (digits ~ ("." ~ digits).? | "." ~ digits)).!
    .map(_.toLong).opaque("long")

  def quoted[_: P]: P[String] =
    (quote ~ ((escape ~ (quote.! | escape.!)) | CharPred(char => !(isQuote(char) || isEscape(char))).!).rep
      .map(_.mkString) ~ quote).opaque("quoted text")

  def text[_: P]: P[String] = (quoted | identChar.rep(1).!).opaque("text")

  def isIdentStart(char: Char): Boolean = isLetter(char) || "_#".contains(char)

  def identStart[_: P]: P[Unit] = CharPred(isIdentStart)

  def identChar[_: P]: P[Unit] = identStart | digit

  def operator[E, _: P](parser: => P[E]): P[E] = s ~ parser

  def operator[_: P](name: String*): P[String] = operator(any(name.map(e => () => IgnoreCase(e))).!)

  def keywordEnd[_: P]: P[Unit] = (!(identChar | digit) | End).opaque("keyword end")

  def keyword[E, _: P](parser: => P[E]): P[E] = operator(parser) ~ keywordEnd

  def keyword[_: P](name: String*): P[String] = keyword(any(name.map(e => () => IgnoreCase(e))).!)

  def identifier[_: P]: P[String] =
    keyword((identStart ~ identChar.rep(0)).!)
      .map(_.toLowerCase).opaque("identifier")

  def any[V, _: P](parsers: Seq[() => P[V]]): P[V] = parsers.foldLeft(Fail: P[V])(_ | _.apply())
}
