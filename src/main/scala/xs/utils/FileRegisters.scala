package xs.utils

import java.io.{File, FileWriter}
import scala.collection.mutable

object FileRegisters {
  val filesMap = mutable.Map[String, (String, String, Seq[() => String], Boolean)]()

  def add(filename: String, contents: => String): Unit = {
    add("", filename, contents, false)
  }

  def add(filedir: String, filename: String, contents: => String, dontCarePrefix: Boolean = false): Unit = {
    val fn = () => contents
    val tag = if(dontCarePrefix) s"$filedir/$filename" else s"$filedir/$filename/p"
    if(filesMap.contains(tag)) {
      val info = filesMap(tag)
      filesMap(tag) = (info._1, info._2, info._3 :+ fn, dontCarePrefix)
    } else {
      filesMap.addOne((tag, (filedir, filename, Seq(fn), dontCarePrefix)))
    }
  }

  def contains(filename: String): Boolean = {
    filesMap.count(_._2._2 == filename) != 0
  }

  def write(fileDir: String = "./build", filePrefix: String = ""): Unit = {
    filesMap.foreach {
      case (_, (fd, fn, fcs, df)) =>
        val fc = fcs.map(_()).mkString("")
        if(df) writeOutputFile(fileDir, fd, fn, fc)
        else writeOutputFile(fileDir, fd, filePrefix + fn, fc)
    }
  }

  def writeOutputFile(td: String, fd: String, fn: String, fc: String): File = {
    val dirStr = if(fd == "") td else s"$td/$fd"
    val dir = new File(dirStr)
    if(!dir.exists()) require(dir.mkdirs())
    val fname = s"$dirStr/$fn"
    val f = new File(fname)
    val fw = new FileWriter(f)
    fw.write(fc)
    fw.close()
    f
  }
}
