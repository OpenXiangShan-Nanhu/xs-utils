package xs.utils.constr

import scala.collection.mutable

case class CreateClock(
  target:ObjectHelper = GetPorts(),
  period:Double = 0.5,
  name:Option[String] = None,
) extends ConstrStmt {
  override def toString:String = {
    val strQueue = new mutable.Queue[String]()
    strQueue.addOne(s"create_clock -period $period")
    name.foreach(s => strQueue.addOne(s"-name $s"))
    strQueue.addOne(s"$target")
    strQueue.toSeq.mkString(" ")
  }
}

case class CreateGeneratedClock(
  target:ObjectHelper = GetPorts(),
  source:ObjectHelper = GetPorts(),
  name:Option[String] = None,
  divide:Option[Int] = None,
  multi:Option[Int] = None,
  duty: Option[Double] = None,
  edges: Option[String] = None,
  comb: Boolean = false,
) extends ConstrStmt {
  override def toString:String = {
    val strQueue = new mutable.Queue[String]()
    strQueue.addOne(s"create_generated_clock")
    strQueue.addOne(s"-source $source")
    name.foreach(s => strQueue.addOne(s"-name $s"))
    divide.foreach(s => strQueue.addOne(s"-divide_by $s"))
    multi.foreach(s => strQueue.addOne(s"-multiply_by $s"))
    duty.foreach(s => strQueue.addOne(s"-duty_cycle ${scala.math.round(s * 100)}"))
    edges.foreach(s => strQueue.addOne(s"-edges {$s}"))
    if(comb) strQueue.addOne(s"-combinational")
    strQueue.addOne(s"$target")
    strQueue.toSeq.mkString(" ")
  }
}

case class SetClockGroup(
  name:Option[String] = None,
  async:Boolean = false,
  groups:Seq[GetClock],
) extends ConstrStmt {
  override def toString:String = {
    val strQueue = new mutable.Queue[String]()
    strQueue.addOne(s"set_clock_groups")
    name.foreach(s => strQueue.addOne(s"-name $s"))
    if(async) strQueue.addOne(s"-asynchronous")
    val grpStr = groups.map(g => s"  -group $g").mkString("  \\\n", "  \\\n", "")
    strQueue.addOne(grpStr)
    strQueue.toSeq.mkString(" ")
  }
}