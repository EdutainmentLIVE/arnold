module Main
  ( main
  )
where

import qualified Arnold
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.UUID as Uuid
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec $ do
  Hspec.describe "makeLeaderboard" $ do

    Hspec.it "with no workouts" $ do
      Arnold.makeLeaderboard [] `Hspec.shouldBe` []

    Hspec.it "with one workout" $ do
      let
        taylor = newUserId "taylor"
        workout = Arnold.Workout
          { Arnold.workoutId = newWorkoutId 1
          , Arnold.workoutRecordedAt = newZonedTime 1 8
          , Arnold.workoutExercise = Arnold.ExercisePlank
          , Arnold.workoutUserId = taylor
          , Arnold.workoutCount = Arnold.Count 1
          }
      -- This person gets three points because they get one point for doing a
      -- workout and then they get two points for being the best of the day.
      Arnold.makeLeaderboard [workout]
        `Hspec.shouldBe` [(Arnold.Rank 1, Arnold.Score 3, taylor)]

    Hspec.it "with two workouts" $ do
      let
        taylor = newUserId "taylor"
        workout1 = Arnold.Workout
          { Arnold.workoutId = newWorkoutId 1
          , Arnold.workoutRecordedAt = newZonedTime 1 8
          , Arnold.workoutExercise = Arnold.ExercisePlank
          , Arnold.workoutUserId = taylor
          , Arnold.workoutCount = Arnold.Count 1
          }
        workout2 = Arnold.Workout
          { Arnold.workoutId = newWorkoutId 2
          , Arnold.workoutRecordedAt = newZonedTime 1 9
          , Arnold.workoutExercise = Arnold.ExercisePlank
          , Arnold.workoutUserId = taylor
          , Arnold.workoutCount = Arnold.Count 2
          }
      -- This person gets three points because they get two points for doing
      -- two workouts and then they get two points for being the best of the
      -- day. Note that they do not get any extra points for doing more
      -- exercises during their second set.
      Arnold.makeLeaderboard [workout1, workout2]
        `Hspec.shouldBe` [(Arnold.Rank 1, Arnold.Score 4, taylor)]

    Hspec.it "with two workouts over two days" $ do
      let
        taylor = newUserId "taylor"
        workout1 = Arnold.Workout
          { Arnold.workoutId = newWorkoutId 1
          , Arnold.workoutRecordedAt = newZonedTime 1 8
          , Arnold.workoutExercise = Arnold.ExercisePlank
          , Arnold.workoutUserId = taylor
          , Arnold.workoutCount = Arnold.Count 1
          }
        workout2 = Arnold.Workout
          { Arnold.workoutId = newWorkoutId 2
          , Arnold.workoutRecordedAt = newZonedTime 2 8
          , Arnold.workoutExercise = Arnold.ExercisePlank
          , Arnold.workoutUserId = taylor
          , Arnold.workoutCount = Arnold.Count 1
          }
      -- This person gets six points because they did two workouts (one point
      -- each) and each day they were the best of the day (two points each).
      Arnold.makeLeaderboard [workout1, workout2]
        `Hspec.shouldBe` [(Arnold.Rank 1, Arnold.Score 6, taylor)]

    Hspec.it "with two workouts over two days with improvement" $ do
      let
        taylor = newUserId "taylor"
        workout1 = Arnold.Workout
          { Arnold.workoutId = newWorkoutId 1
          , Arnold.workoutRecordedAt = newZonedTime 1 8
          , Arnold.workoutExercise = Arnold.ExercisePlank
          , Arnold.workoutUserId = taylor
          , Arnold.workoutCount = Arnold.Count 1
          }
        workout2 = Arnold.Workout
          { Arnold.workoutId = newWorkoutId 2
          , Arnold.workoutRecordedAt = newZonedTime 2 8
          , Arnold.workoutExercise = Arnold.ExercisePlank
          , Arnold.workoutUserId = taylor
          , Arnold.workoutCount = Arnold.Count 2
          }
      -- This person gets ten points because they did two workouts (one point
      -- each) and each day they were the best of the day (two points each) and
      -- the second day they did better than the first day (four points).
      Arnold.makeLeaderboard [workout1, workout2]
        `Hspec.shouldBe` [(Arnold.Rank 1, Arnold.Score 10, taylor)]

    Hspec.it "with three workouts over three days with improvement" $ do
      let
        taylor = newUserId "taylor"
        workout1 = Arnold.Workout
          { Arnold.workoutId = newWorkoutId 1
          , Arnold.workoutRecordedAt = newZonedTime 1 8
          , Arnold.workoutExercise = Arnold.ExercisePlank
          , Arnold.workoutUserId = taylor
          , Arnold.workoutCount = Arnold.Count 1
          }
        workout2 = Arnold.Workout
          { Arnold.workoutId = newWorkoutId 2
          , Arnold.workoutRecordedAt = newZonedTime 2 8
          , Arnold.workoutExercise = Arnold.ExercisePlank
          , Arnold.workoutUserId = taylor
          , Arnold.workoutCount = Arnold.Count 2
          }
        workout3 = Arnold.Workout
          { Arnold.workoutId = newWorkoutId 3
          , Arnold.workoutRecordedAt = newZonedTime 3 8
          , Arnold.workoutExercise = Arnold.ExercisePlank
          , Arnold.workoutUserId = taylor
          , Arnold.workoutCount = Arnold.Count 3
          }
      -- This person gets 17 points because they did three workouts (one point
      -- each) and each day they were the best of the day (two points each) and
      -- each day they did better than the previous day (four points each).
      Arnold.makeLeaderboard [workout1, workout2, workout3]
        `Hspec.shouldBe` [(Arnold.Rank 1, Arnold.Score 17, taylor)]

    Hspec.it "with three workouts over three days with some improvement" $ do
      let
        taylor = newUserId "taylor"
        workout1 = Arnold.Workout
          { Arnold.workoutId = newWorkoutId 1
          , Arnold.workoutRecordedAt = newZonedTime 1 8
          , Arnold.workoutExercise = Arnold.ExercisePlank
          , Arnold.workoutUserId = taylor
          , Arnold.workoutCount = Arnold.Count 1
          }
        workout2 = Arnold.Workout
          { Arnold.workoutId = newWorkoutId 2
          , Arnold.workoutRecordedAt = newZonedTime 2 8
          , Arnold.workoutExercise = Arnold.ExercisePlank
          , Arnold.workoutUserId = taylor
          , Arnold.workoutCount = Arnold.Count 3
          }
        workout3 = Arnold.Workout
          { Arnold.workoutId = newWorkoutId 3
          , Arnold.workoutRecordedAt = newZonedTime 3 8
          , Arnold.workoutExercise = Arnold.ExercisePlank
          , Arnold.workoutUserId = taylor
          , Arnold.workoutCount = Arnold.Count 2
          }
      -- This person gets 13 points because they did three workouts (one point
      -- each) and each day they were the best of the day (two points each) and
      -- one day they did better than the previous day (four points).
      Arnold.makeLeaderboard [workout1, workout2, workout3]
        `Hspec.shouldBe` [(Arnold.Rank 1, Arnold.Score 13, taylor)]

    Hspec.it "with two workouts over two people" $ do
      let
        taylor = newUserId "taylor"
        workoutTaylor = Arnold.Workout
          { Arnold.workoutId = newWorkoutId 1
          , Arnold.workoutRecordedAt = newZonedTime 1 8
          , Arnold.workoutExercise = Arnold.ExercisePlank
          , Arnold.workoutUserId = taylor
          , Arnold.workoutCount = Arnold.Count 1
          }
        cameron = newUserId "cameron"
        workoutCameron = Arnold.Workout
          { Arnold.workoutId = newWorkoutId 2
          , Arnold.workoutRecordedAt = newZonedTime 1 8
          , Arnold.workoutExercise = Arnold.ExercisePlank
          , Arnold.workoutUserId = cameron
          , Arnold.workoutCount = Arnold.Count 2
          }
      -- In this scenario each person gets one point for doing an exercise.
      -- Then Cameron gets two points for being the best of the day.
      Arnold.makeLeaderboard [workoutTaylor, workoutCameron]
        `Hspec.shouldBe` [ (Arnold.Rank 1, Arnold.Score 3, cameron)
                         , (Arnold.Rank 2, Arnold.Score 1, taylor)
                         ]

    Hspec.it "with two workouts over two people tied" $ do
      let
        taylor = newUserId "taylor"
        workoutTaylor = Arnold.Workout
          { Arnold.workoutId = newWorkoutId 1
          , Arnold.workoutRecordedAt = newZonedTime 1 8
          , Arnold.workoutExercise = Arnold.ExercisePlank
          , Arnold.workoutUserId = taylor
          , Arnold.workoutCount = Arnold.Count 1
          }
        cameron = newUserId "cameron"
        workoutCameron = Arnold.Workout
          { Arnold.workoutId = newWorkoutId 2
          , Arnold.workoutRecordedAt = newZonedTime 1 8
          , Arnold.workoutExercise = Arnold.ExercisePlank
          , Arnold.workoutUserId = cameron
          , Arnold.workoutCount = Arnold.Count 1
          }
      -- In this scenario each person gets one point for doing an exercise.
      -- Then both people get two points for being tied for the best of the
      -- day.
      Arnold.makeLeaderboard [workoutTaylor, workoutCameron]
        `Hspec.shouldBe` [ (Arnold.Rank 1, Arnold.Score 3, cameron)
                         , (Arnold.Rank 1, Arnold.Score 3, taylor)
                         ]

    Hspec.it "with three workouts over three people with two tied" $ do
      let
        taylor = newUserId "taylor"
        cameron = newUserId "cameron"
        dustin = newUserId "dustin"
        workoutTaylor = Arnold.Workout
          { Arnold.workoutId = newWorkoutId 1
          , Arnold.workoutRecordedAt = newZonedTime 1 8
          , Arnold.workoutExercise = Arnold.ExercisePlank
          , Arnold.workoutUserId = taylor
          , Arnold.workoutCount = Arnold.Count 1
          }
        workoutCameron = Arnold.Workout
          { Arnold.workoutId = newWorkoutId 2
          , Arnold.workoutRecordedAt = newZonedTime 1 8
          , Arnold.workoutExercise = Arnold.ExercisePlank
          , Arnold.workoutUserId = cameron
          , Arnold.workoutCount = Arnold.Count 2
          }
        workoutDustin = Arnold.Workout
          { Arnold.workoutId = newWorkoutId 3
          , Arnold.workoutRecordedAt = newZonedTime 1 8
          , Arnold.workoutExercise = Arnold.ExercisePlank
          , Arnold.workoutUserId = dustin
          , Arnold.workoutCount = Arnold.Count 2
          }
      Arnold.makeLeaderboard [workoutTaylor, workoutCameron, workoutDustin]
        `Hspec.shouldBe` [ (Arnold.Rank 1, Arnold.Score 3, cameron)
                         , (Arnold.Rank 1, Arnold.Score 3, dustin)
                         , (Arnold.Rank 3, Arnold.Score 1, taylor)
                         ]

    Hspec.it "with nine workouts" $ do
      let
        taylor = newUserId "taylor"
        makeWorkout n = Arnold.Workout
          { Arnold.workoutId = newWorkoutId n
          , Arnold.workoutRecordedAt = newZonedTime 1 n
          , Arnold.workoutExercise = Arnold.ExercisePlank
          , Arnold.workoutUserId = taylor
          , Arnold.workoutCount = Arnold.Count 1
          }
        workouts = fmap makeWorkout [8 .. 16]
      -- This person only gets 10 points (rather than 11) because even though
      -- they did nine workouts, they only get credit for eight. The other two
      -- points come from being the best of the day.
      Arnold.makeLeaderboard workouts
        `Hspec.shouldBe` [(Arnold.Rank 1, Arnold.Score 10, taylor)]

newUserId :: String -> Arnold.UserId
newUserId = Arnold.UserId . Text.pack

newWorkoutId :: Int -> Arnold.WorkoutId
newWorkoutId = Arnold.WorkoutId . Uuid.fromWords 0 0 0 . fromIntegral

newZonedTime :: Int -> Int -> Time.ZonedTime
newZonedTime day hour = Time.ZonedTime
  (Time.LocalTime (Time.fromGregorian 2019 7 day) (Time.TimeOfDay hour 50 0))
  (Time.hoursToTimeZone (-4))
