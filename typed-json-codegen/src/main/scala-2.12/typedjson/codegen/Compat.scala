package typedjson.codegen

import scala.collection.JavaConverters.*
import scala.collection.mutable

private[codegen] object Compat {
  def javaIteratorToScalaIterator[A](it: java.util.Iterator[A]): Iterator[A] = it.asScala
  
  def javaListToScala[A](l: java.util.List[A]): mutable.Seq[A] = l.asScala
}
