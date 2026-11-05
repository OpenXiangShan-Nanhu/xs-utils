package chisel3

import chisel3.internal.{Builder, BuilderContextCache}

import scala.collection.mutable

/** Adapter to Chisel's elaboration-local cache, shared with hierarchy Definitions.
  * No source-registration state survives an elaboration (including a failed one).
  */
private[chisel3] object VlogSourceCacheKey extends BuilderContextCache.Key[mutable.Map[String, String]]

object VlogSourceCache {
  def sources: mutable.Map[String, String] =
    Builder.contextCache.getOrElseUpdate(VlogSourceCacheKey, mutable.LinkedHashMap.empty[String, String])
}
