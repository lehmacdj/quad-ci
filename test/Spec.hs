-- test/Spec.hs
module Main where

import Core
import GHC.Stack (HasCallStack)
import RIO
import Test.Hspec
import qualified Docker
import qualified RIO.Map as Map
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial
import qualified Runner
import qualified System.Process.Typed as Process

-- Helper functions
makeStep :: HasCallStack => Text -> Text -> [Text] -> Step
makeStep name image commands
  = Step
  { name = StepName name
  , image = Docker.Image image
  , commands = NonEmpty.Partial.fromList commands
  }

makePipeline :: HasCallStack => [Step] -> Pipeline
makePipeline steps = Pipeline { steps = NonEmpty.Partial.fromList steps }

-- Test values
testPipeline :: Pipeline
testPipeline = makePipeline
  [ makeStep "First step" "ubuntu" ["date"]
  , makeStep "Second step" "ubuntu" ["uname -r"]
  ]

testBuild :: Build
testBuild = Build
  { pipeline = testPipeline
  , state = BuildReady
  , completedSteps = mempty
  }

testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
  build <- runner.prepareBuild testPipeline
  result <- runner.runBuild build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testRunFailure :: Runner.Service -> IO ()
testRunFailure runner = do
  build <-
    runner.prepareBuild $ makePipeline
      [ makeStep "Should fail" "ubuntu" ["exit 1"]
      ]
  result <- runner.runBuild build
  result.state `shouldBe` BuildFinished BuildFailed
  Map.elems result.completedSteps
    `shouldBe` [StepFailed (Docker.ContainerExitCode 1)]

main :: IO ()
main = hspec do
  docker <- runIO Docker.createService
  runner <- runIO $ Runner.createService docker
  beforeAll cleanupDocker $ describe "Quad CI" do
    it "should run a build (success)" do
      testRunSuccess runner
    it "should run a build (failure)" do
      testRunFailure runner

cleanupDocker :: IO ()
cleanupDocker =
  void . Process.readProcessStdout $
    "docker rm -f $(docker ps -aq --filter \"label=quad\")"
