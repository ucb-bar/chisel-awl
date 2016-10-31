package hbwif

import testchipip.SCRHeaderOutput

object Generator extends util.GeneratorApp {
  def generateSCRHeader {
    writeOutputFile(td, s"${names.configs}.scr.h", SCRHeaderOutput.contents.mkString("\n"))
  }

  val longName = names.topModuleClass + "." + names.configs
  generateFirrtl
  generateTestSuiteMakefrags // TODO: Needed only for legacy make targets
  generateParameterDump // TODO: Needed only for legacy make targets
  generateSCRHeader
}
