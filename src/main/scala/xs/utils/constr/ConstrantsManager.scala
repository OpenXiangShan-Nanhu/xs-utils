package xs.utils.constr

import chisel3.Data
import xs.utils.FileRegisters

import scala.collection.mutable

abstract class ConstrStmt

case class AnyConstr (
  cmt:String = ""
) extends ConstrStmt {
  override def toString:String = cmt
}

object ConstrManager {
  private val constrFunQueue = new mutable.Queue[ConstrStmt]
  private val constrStrQueue = new mutable.Queue[String]
  private val prefixStack = new mutable.Stack[String]

  def addPathContext(instName:String):Unit = {
    prefixStack.push(instName)
  }

  def popPathContext():String = {
    prefixStack.pop()
  }

  def registerConstr(constr: ConstrStmt):Unit = {
    constrFunQueue.addOne(constr)
  }

  def evalConstr():Unit = {
    constrFunQueue.map(m => constrStrQueue.addOne(m.toString))
    constrFunQueue.clear()
  }

  def exportConstr(domain: String):Unit = {
    evalConstr()
    val str = constrStrQueue.toSeq.mkString("\n")
    if(constrStrQueue.nonEmpty) FileRegisters.add(filedir = "constr", filename = s"${domain}.sdc", contents = str, dontCarePrefix = true)
    constrStrQueue.clear()
  }

  private def buildPrefix():Seq[String] = {
    val q = new mutable.Queue[String]
    prefixStack.reverse.foreach(s => q.addOne(s))
    q.toSeq
  }

  def getPath(in: chisel3.InstanceId):String = {
    val s = buildPrefix() ++ in.pathName.split('.').drop(1).toSeq
    s.mkString("/")
  }
}