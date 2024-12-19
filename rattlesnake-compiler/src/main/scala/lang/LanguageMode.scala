package lang

enum LanguageMode {
  case OcapEnabled, OcapDisabled
  
  def isOcapEnabled: Boolean = this == OcapEnabled
  
  def isOcapDisabled: Boolean = this == OcapDisabled
  
}
