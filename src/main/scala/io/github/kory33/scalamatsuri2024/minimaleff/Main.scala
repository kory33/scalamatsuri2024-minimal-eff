package io.github.kory33.scalamatsuri2024.minimaleff

import cats.Monad
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.http4s.client.Client
import scala.util.NotGiven

object EffLibrary {
  enum Executable[Instr[_], ExitCode]:
    case Exit[I[_], EC](exitCode: EC) extends Executable[I, EC]
    case NonEmpty[I[_], BranchIndex, EC](
      headInstr: I[BranchIndex],
      branches: BranchIndex => Executable[I, EC]
    ) extends Executable[I, EC]

  def injectAtExits[I[_], A, B](program: Executable[I, A],
                                selectNext: A => Executable[I, B]
  ): Executable[I, B] = program match
    case Executable.Exit(exitCode) => selectNext(exitCode)
    case Executable.NonEmpty(headInstr, branches) =>
      Executable.NonEmpty(
        headInstr,
        branchIndex => injectAtExits(branches(branchIndex), selectNext)
      )

  def compactify[Instr[_], EC](
    program: Executable[Instr, EC]
  )(using monad: Monad[Instr]): Instr[EC] = program match
    case Executable.Exit(exitCode) => monad.pure(exitCode)
    case Executable.NonEmpty(headInstr, branches) =>
      monad.flatMap(headInstr)(branchIndex =>
        compactify(branches(branchIndex))
      )

  extension [I[_], A](exe: Executable[I, A])
    // an alias for `injectAtExits`, will be useful in for-expressions
    def flatMap[B](continuation: A => Executable[I, B]) = injectAtExits(exe, continuation)
    // will be useful in for-expressions
    def map[B](f: A => B): Executable[I, B] = exe.flatMap(a => Executable.Exit(f(a)))

  case class EitherK[Instr1[_], Instr2[_], A](value: Either[Instr1[A], Instr2[A]])
  infix type mix[F[_], G[_]] = [A] =>> EitherK[F, G, A]

  // NOTE: This typeclass is not mentioned in the talk;
  //       an `Inclusion[SI, I]` instance says that "SI is a sub-instruction of I",
  //       by asserting that any `SI[A]` can treated as `I[A]`.
  //
  //       You can think of `Include` as a dual to `Decompose` typeclass, in a sense that
  //        - `Decompose` allows us to do deep pattern matching on the instruction tree (making it a generalized ∨-elim), while
  //        - `Include` allows us to do deep construction of the instruction tree (making it a generalized ∨-intro).
  //
  //       To be extremely precise and pedantic, `Include` is not a perfect dual to `Decompose`
  //       since we only defined strict instances for `Decompose`
  //       (i.e. `Decompose.Aux[I, I, [A] =>> Nothing]` is not available), but we have
  //       the non-strict inclusion instance `identity` for `Include` (this is so that `asSingleExe` can be used
  //       even if the instruction we are trying to include is the entire instruction-set tree itself).
  trait Inclusion[SubInstr[_], Instr[_]]:
    def include[A](instr: SubInstr[A]): Instr[A]

  object Inclusion {
    given identity[Instr[_]]: Inclusion[Instr, Instr]
    with
      def include[A](instr: Instr[A]): Instr[A] = instr

    given leftLeaf[SubInstr[_], RightInstr[_]]: Inclusion[SubInstr, SubInstr mix RightInstr]
    with
      def include[A](instr: SubInstr[A]): (SubInstr mix RightInstr)[A] =
        EitherK(Left(instr))

    given rightLeaf[SubInstr[_], LeftInstr[_]]: Inclusion[SubInstr, LeftInstr mix SubInstr]
    with
      def include[A](instr: SubInstr[A]): (LeftInstr mix SubInstr)[A] =
        EitherK(Right(instr))

    // NOTE: We choose not to define a transitivity instance for `Inclusion`, since
    //       our `Inclusion` is a non-strict inclusion relation (i.e. `I Includes I` is always available)
    //       and having a transitivity instance would cause infinite loop in the implicit resolution.
    given leftNode[SubInstr[_], LeftI[_], RightI[_]](
      using leftInclusion: Inclusion[SubInstr, LeftI]
    ): Inclusion[SubInstr, LeftI mix RightI] with
      def include[A](instr: SubInstr[A]): (LeftI mix RightI)[A] =
        EitherK(Left(leftInclusion.include(instr)))

    given rightNode[SubInstr[_], LeftI[_], RightI[_]](
      using rightInclusion: Inclusion[SubInstr, RightI]
    ): Inclusion[SubInstr, LeftI mix RightI] with
      def include[A](instr: SubInstr[A]): (LeftI mix RightI)[A] =
        EitherK(Right(rightInclusion.include(instr)))
  }

  // an alias for `Inclusion` typeclass, so that we can read `I Includes F` as "I includes F as a sub-instruction"
  infix type Includes[Instr[_], SubInstr[_]] = Inclusion[SubInstr, Instr]

  extension [F[_], A](fa: F[A])
    def asSingleExe[I[_]](using IIncludesF: I Includes F): Executable[I, A] =
      Executable.NonEmpty(IIncludesF.include(fa), Executable.Exit.apply)

  trait Decompose[SubInstr[_], Instr[_]]:
    type Complement[_] // everything in Instr but not in SubInstr
    def classify[A](instr: Instr[A]): Either[SubInstr[A], Complement[A]]

  object Decompose {
    type Aux[SubInstr[_], Instr[_], Complement0[_]] = Decompose[SubInstr, Instr] {
      type Complement[A] = Complement0[A]
    }

    given leftLeaf[SubInstr[_], RightInstr[_]]: Decompose[SubInstr, SubInstr mix RightInstr]
    with
      type Complement[A] = RightInstr[A]
      def classify[A](instr: (SubInstr mix RightInstr)[A]): Either[SubInstr[A], RightInstr[A]] =
        instr.value

    given rightLeaf[SubInstr[_], LeftInstr[_]]: Decompose[SubInstr, LeftInstr mix SubInstr]
    with
      type Complement[A] = LeftInstr[A]
      def classify[A](instr: (LeftInstr mix SubInstr)[A]): Either[SubInstr[A], LeftInstr[A]] =
        instr.value.swap

    given transitivity[Instr0[_], Instr1[_], Instr2[_]](
      using d1: Decompose[Instr0, Instr1],
      d2: Decompose[Instr1, Instr2]
    ): Decompose[Instr0, Instr2] with
      type Complement[A] = (d1.Complement mix d2.Complement)[A]
      def classify[A](instr2: Instr2[A]): Either[Instr0[A], Complement[A]] =
        d2.classify(instr2) match
          case Left(instr1) => d1.classify(instr1) match
              case Left(instr0)     => Left(instr0)
              case Right(nonInstr0) => Right(EitherK(Left(nonInstr0)))
          case Right(nonInstr1) => Right(EitherK(Right(nonInstr1)))
  }

  def transpile[I1[_], I2[_], ExitCode](
    expand: [A] => I1[A] => Executable[I2, A]
  )(program: Executable[I1, ExitCode]): Executable[I2, ExitCode] =
    program match
      case Executable.Exit(exitCode) => Executable.Exit(exitCode)
      case Executable.NonEmpty(headInstr, branches) =>
        injectAtExits(
          expand(headInstr),
          branchIndex => transpile[I1, I2, ExitCode](expand)(branches(branchIndex))
        )

  def desugarSubInstruction[Sugar[_], Source[_], Desugared[_], ExitCode](
    desugar: [A] => Sugar[A] => Executable[Desugared, A]
  )(
    program: Executable[Source, ExitCode]
  )(using
  decomposition: Decompose.Aux[Sugar, Source, Desugared]): Executable[Desugared, ExitCode] =
    transpile[Source, Desugared, ExitCode]([A] =>
      (instr: Source[A]) =>
        decomposition.classify(instr) match
          case Left(sugarInstr)     => desugar(sugarInstr)
          case Right(nonSugarInstr) => nonSugarInstr.asSingleExe
    )(program)
}

object Example {
  import EffLibrary.*
  import scala.util.chaining.given

  enum LogWithLevel[A]:
    case Info(msg: String) extends LogWithLevel[Unit]
    case Error(msg: String) extends LogWithLevel[Unit]

  enum HttpGet[A]:
    case Request(url: String) extends HttpGet[Array[Byte]]

  def desugarHttpGet[A](httpClient: Client[IO])(
    exe: Executable[IO mix LogWithLevel mix HttpGet, A]
  ): Executable[IO mix LogWithLevel, A] =
    // Inferring higher-kinded types is inherently hard, so... yeah, we need these type arguments to be explicit.
    // Things actually work a lot better in a polymorphic transpilers because there are only a few types
    // that would potentially satisfy the constraints, so compilers may be able to infer them.
    desugarSubInstruction[HttpGet, IO mix LogWithLevel mix HttpGet, IO mix LogWithLevel, A]([
      X
    ] =>
      (req: HttpGet[X]) =>
        req match {
          case HttpGet.Request(url) =>
            for {
              _ <-
                LogWithLevel.Info(s"sending request to $url").asSingleExe[IO mix LogWithLevel]
              response <- httpClient.expect[Array[Byte]](url).asSingleExe[IO mix LogWithLevel]
              _ <-
                LogWithLevel.Info(s"received response of size ${response.length}").asSingleExe[
                  IO mix LogWithLevel
                ]
            } yield {
              // Removing `: X` results in an error. X =:= Array[Byte] (because `req` has been matched to `HttpGet.Request`),
              // but it seem like we need to explicitly tell the compiler that our intention is to return `X`.
              response: X
            }
      })(exe)

  def desugarLog[A](
    exe: Executable[IO mix LogWithLevel, A]
  ): Executable[IO, A] =
    desugarSubInstruction[LogWithLevel, IO mix LogWithLevel, IO, A]([X] =>
      (log: LogWithLevel[X]) =>
        log match {
          case LogWithLevel.Info(msg)  => IO.println(s"[INFO] $msg").asSingleExe
          case LogWithLevel.Error(msg) => IO.println(s"[ERROR] $msg").asSingleExe
        }: Executable[IO, X])(exe)

  class Machine(httpClient: Client[IO])(using ioRuntime: IORuntime) {
    def run[A](
      prog: Executable[IO mix LogWithLevel mix HttpGet, A]
    ): A = prog
      // staged transpilation phase
      .pipe(desugarHttpGet(httpClient))
      .pipe(desugarLog)
      // compactification and execution phase
      .pipe(compactify)
      .unsafeRunSync() /*(ioRuntime) implicitly*/
  }
}

@main def main(): Unit = {
  import Example.*
  import EffLibrary.*
  val program = for {
    _ <- LogWithLevel.Info("start").asSingleExe[IO mix LogWithLevel mix HttpGet]
  } yield ()
}
