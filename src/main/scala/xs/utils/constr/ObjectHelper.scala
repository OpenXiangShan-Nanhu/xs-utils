package xs.utils.constr

import scala.collection.mutable

abstract class ObjectHelper extends ConstrStmt {

  def cmd:String

  def target: () => String

  def filter:Option[String]

  def of:Option[ObjectHelper]

  def regexp:Boolean

  def _string(sq: mutable.Queue[String]):Unit

  override def toString:String = {
    val strQueue = new mutable.Queue[String]()
    strQueue.addOne(cmd)
    _string(strQueue)
    if(regexp) strQueue.addOne("-regexp")
    of.foreach(s => strQueue.addOne(s"-of_objects $s"))
    filter.foreach(s => strQueue.addOne(s"-filter $s"))
    strQueue.addOne(s"${target()}")
    strQueue.toSeq.mkString("[", " ", "]")
  }
}

case class GetPorts(
  target: () => String = () => "",
  filter:Option[String] = None,
  of:Option[ObjectHelper] = None,
  regexp:Boolean = false,
) extends ObjectHelper{
  val cmd = "get_ports"
  def _string(sq:mutable.Queue[String]):Unit = {
  }
}

case class GetPins(
  target: () => String = () => "",
  filter:Option[String] = None,
  of:Option[ObjectHelper] = None,
  regexp:Boolean = false,
) extends ObjectHelper{
  val cmd = "get_pins"
  def _string(sq:mutable.Queue[String]):Unit = {
  }
}

case class GetCells(
  target: () => String = () => "",
  filter:Option[String] = None,
  of:Option[ObjectHelper] = None,
  regexp:Boolean = false,
) extends ObjectHelper{
  val cmd = "get_cells"
  def _string(sq:mutable.Queue[String]):Unit = {
  }
}

case class GetNets(
  target: () => String = () => "",
  filter:Option[String] = None,
  of:Option[ObjectHelper] = None,
  regexp:Boolean = false,
) extends ObjectHelper{
  val cmd = "get_nets"
  def _string(sq:mutable.Queue[String]):Unit = {
  }
}

case class GetClock(
  target: () => String = () => "",
  filter:Option[String] = None,
  of:Option[ObjectHelper] = None,
  regexp:Boolean = false,
  includeGen:Boolean = false
) extends ObjectHelper{
  val cmd = "get_clocks"
  def _string(sq:mutable.Queue[String]):Unit = {
    if(includeGen) sq.addOne("-include_generated_clocks")
  }
}