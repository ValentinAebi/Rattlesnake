package lang

import identifiers.{NormalTypeId, TypeIdentifier}
import lang.Keyword.Fs

enum Device(val keyword: Keyword, val sig: DeviceSignature) {
  case FileSystem extends Device(Fs, DeviceSignature(
    NormalTypeId("FileSystem"),
    Map(/* TODO add methods (in particular open(String)) */)
  ))

  override def toString: String = keyword.str
}

object Device {

  val kwToDevice: Map[Keyword, Device] = values.map(d => d.keyword -> d).toMap

  val deviceTypeToSig: Map[TypeIdentifier, DeviceSignature] = values.map(d => d.sig.id -> d.sig).toMap

}
