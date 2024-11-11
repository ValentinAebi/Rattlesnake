package lang

import identifiers.{NormalTypeId, TypeIdentifier}
import lang.Keyword.Fs
import lang.Types.Type

enum Device(val keyword: Keyword, val sig: DeviceSignature) {
  case FileSystem extends Device(Fs, DeviceSignature(
    NormalTypeId("FileSystem"),
    Map(
      ??? // TODO
    )
  ))

  override def toString: String = keyword.str
}

object Device {

  val kwToDevice: Map[Keyword, Device] = values.map(d => d.keyword -> d).toMap
  
  val deviceTypeToSig: Map[TypeIdentifier, DeviceSignature] = values.map(d => d.sig.tpe -> d.sig).toMap

}