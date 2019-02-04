package awl.test.hbwif

import freechips.rocketchip.util.GeneratorApp

object Generator extends GeneratorApp {
    val longName = names.topModuleProject + "." + names.topModuleClass + "." + names.configs
    generateFirrtl
    generateAnno
}
