// TODO delete this file altogether and use chisel-testers
package awl

import freechips.rocketchip.util.GeneratorApp

object Generator extends GeneratorApp {
    val longName = names.topModuleProject + "." + names.topModuleClass + "." + names.configs
    generateFirrtl
    generateAnno
    generateArtefacts
}
