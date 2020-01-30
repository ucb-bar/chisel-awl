package hbwif

import freechips.rocketchip.util.GeneratorApp

object Generator extends GeneratorApp {
    override lazy val longName = names.topModuleProject + "." + names.topModuleClass + "." + names.configs
    generateFirrtl
    generateAnno
}
