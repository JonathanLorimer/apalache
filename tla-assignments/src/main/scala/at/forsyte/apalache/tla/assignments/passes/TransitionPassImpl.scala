package at.forsyte.apalache.tla.assignments.passes

import at.forsyte.apalache.infra.passes.{Pass, PassOptions, TlaModuleMixin}
import at.forsyte.apalache.tla.assignments._
import at.forsyte.apalache.tla.imp.findBodyOf
import at.forsyte.apalache.tla.imp.src.SourceStore
import at.forsyte.apalache.tla.lir._
import at.forsyte.apalache.io.lir.{TlaWriter, TlaWriterFactory}
import at.forsyte.apalache.tla.lir.storage.{BodyMapFactory, ChangeListener, SourceLocator}
import at.forsyte.apalache.tla.lir.transformations.TransformationTracker
import at.forsyte.apalache.tla.lir.transformations.standard.IncrementalRenaming
import at.forsyte.apalache.tla.pp.NormalizedNames
import com.google.inject.Inject
import com.google.inject.name.Named
import com.typesafe.scalalogging.LazyLogging

import java.io.File
import java.nio.file.Path

/**
 * This pass finds symbolic transitions in Init and Next.
 */
class TransitionPassImpl @Inject() (options: PassOptions, sourceStore: SourceStore, tracker: TransformationTracker,
    changeListener: ChangeListener, incrementalRenaming: IncrementalRenaming, writerFactory: TlaWriterFactory,
    @Named("AfterTransitionFinder") nextPass: Pass with TlaModuleMixin)
    extends TransitionPass with LazyLogging {

  /**
   * The name of the pass
   *
   * @return the name associated with the pass
   */
  override def name: String = "TransitionFinderPass"

  /**
   * Run the pass
   *
   * @return true, if the pass was successful
   */
  override def execute(): Boolean = {
    val inModule = tlaModule.get

    // extract transitions from InitPrimed
    val initOperName = options.getOrElse("checker", "init", "Init")
    val initOperNamePrimed = initOperName + "Primed"
    val initDeclarations = extractTransitions(inModule, initOperNamePrimed, NormalizedNames.INIT_PREFIX)
    logger.info(s"  > Found ${initDeclarations.size} initializing transitions")

    // extract transitions from Next
    val nextOperName = options.getOrElse("checker", "next", "Next")
    val nextDeclarations = extractTransitions(inModule, nextOperName, NormalizedNames.NEXT_PREFIX)
    logger.info(s"  > Found ${nextDeclarations.size} transitions")

    // convert an optional CInit operator
    val cinitDeclarations =
      options.get[String]("checker", "cinit") match {
        case None =>
          logger.info(s"  > No constant initializer")
          Seq()

        case Some(cinitName) =>
          logger.info(s"  > Found constant initializer $cinitName")
          val cinitEx = findBodyOf(cinitName + "Primed", inModule.operDeclarations: _*)
          // We don't perform the standard assignment-search on cinit,
          // we just replace EVERY x' = e with x' <- e
          val tr = AssignmentOperatorIntroduction({ _ => true }, tracker)
          val newEx = tr(cinitEx)
          Seq(ModuleAdapter.exprToOperDef(NormalizedNames.CONST_INIT, newEx))
      }

    // Add the constants, variables, and assumptions; then add CInit, Init*, Next*; then add verification conditions.
    val vcDeclarations = inModule.declarations.filter(NormalizedNames.isVC)
    val newDecls = inModule.constDeclarations ++ inModule.varDeclarations ++ inModule.assumeDeclarations ++
      cinitDeclarations ++ initDeclarations ++ nextDeclarations ++ vcDeclarations

    logger.info(s"  > Applying unique renaming")
    val outModule = incrementalRenaming.renameInModule(new TlaModule(inModule.name, newDecls))

    // print the resulting module
    val outdir = options.getOrError("io", "outdir").asInstanceOf[Path]
    writerFactory.writeModuleAllFormats(outModule.copy(name = "09_OutTransition"), TlaWriter.STANDARD_MODULES,
        outdir.toFile)

    setModule(outModule)
    true
  }

  private def extractTransitions(module: TlaModule, inOperName: String, outOperName: String): Seq[TlaOperDecl] = {
    val primedName = findBodyOf(inOperName, module.declarations: _*)
    val vars = module.varDeclarations.map(_.name)

    val sourceLoc = SourceLocator(sourceStore.makeSourceMap, changeListener)

    val operMap = BodyMapFactory.makeFromDecls(module.operDeclarations)
    val transitionPairs = SmtFreeSymbolicTransitionExtractor(tracker, sourceLoc)(vars.toSet, primedName, operMap)
    // sort the transitions by their identifiers, to make sure we have determinism
    val sortedTransitions = transitionPairs.map(_._2).sortBy(_.ID.id)
    ModuleAdapter.exprsToOperDefs(outOperName, sortedTransitions)
  }

  /**
   * Get the next pass in the chain. What is the next pass is up
   * to the module configuration and the pass outcome.
   *
   * @return the next pass, if exists, or None otherwise
   */
  override def next(): Option[Pass] = {
    tlaModule map { m =>
      nextPass.setModule(m)
      nextPass
    }
  }
}
