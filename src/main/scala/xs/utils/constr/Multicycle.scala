package xs.utils.constr

import scala.collection.mutable

case class Multicycle (
  value:Int                    = 1,
  hold: Boolean                = false,
  start:Boolean                = false,
  end:Boolean                  = false,
  from:Option[ObjectHelper]    = None,
  to:Option[ObjectHelper]      = None,
  through:Option[ObjectHelper] = None,
) extends ConstrStmt {
  override def toString:String = {
    val strQueue = new mutable.Queue[String]()
    strQueue.addOne(s"set_multicycle_path $value")
    strQueue.addOne(if(hold) "-hold " else "-setup")
    if(start) strQueue.addOne(s"-start")
    if(end) strQueue.addOne(s"-end  ")
    from.foreach(s => strQueue.addOne(s"-from $s"))
    to.foreach(s => strQueue.addOne(s"-to   $s"))
    through.foreach(s => strQueue.addOne(s"-through $s"))
    strQueue.toSeq.mkString(" ")
  }
}
